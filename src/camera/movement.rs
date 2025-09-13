//! Camera movement on user input

use super::{
	inputs::{MoveCameraMessage, ZoomCameraMessage},
	CameraHarness,
};
use crate::{graphics::*, AppSet};
use bevy::{
	math::bounding::{Aabb2d, BoundingVolume as _},
	prelude::*,
	render::camera::ScalingMode,
};

pub(super) fn plugin(app: &mut App) {
	app.configure_sets(Update, CameraPositionUpdate.after(AppSet::RecordInput))
		.add_systems(Update, update_camera.in_set(CameraPositionUpdate));
}

/// [`SystemSet`] where camera movement is updated
#[derive(SystemSet, Clone, Copy, PartialEq, Eq, Debug, Default, Hash)]
pub struct CameraPositionUpdate;

/// Support component for [`CameraHarness`] that tracks
/// persistent camera state
#[derive(Component, Clone, Copy, Default)]
pub struct CameraInertia {
	pub velocity: Vec2,
	pub zoom: f32,
}

/// Fraction of inertia that is conserved after one second
const PAN_FRICTION: f32 = 0.0005;

/// World units covered per second when at 1:1 zoom
const PAN_SPEED: f32 = 600.0;

/// Fraction of inertia that is conserved after one second
const ZOOM_FRICTION: f32 = PAN_FRICTION;

/// Orders of magnitude of zoom covered per second
const ZOOM_SPEED: f32 = 1.0;

fn zoom_movement_input(mut events: MessageReader<ZoomCameraMessage>) -> f32 {
	events
		.read()
		.map(|event| match event {
			ZoomCameraMessage::In => 1.0,
			ZoomCameraMessage::Out => -1.0,
		})
		.sum::<f32>()
		* ZOOM_SPEED
}

fn pan_movement_input(mut events: MessageReader<MoveCameraMessage>, current_scale: f32) -> Vec2 {
	events.read().map(|event| event.0).sum::<Vec2>() * PAN_SPEED / current_scale
}

impl CameraHarness {
	fn update_zoom(&mut self, inertia: &mut CameraInertia, movement_from_input: f32, delta_t: f32) {
		let level_size = self.level_size();
		// Allow zooming in to double g11 scale, or to see the whole level
		// if it is too small
		let maximum_zoom = (level_size / LEVEL_AREA_WIDTH * 2.0).max_element().max(1.0);
		// Allow zooming out to see the whole level,
		// or to g11 scale if the level is smaller than that
		let minimum_zoom = (level_size / LEVEL_AREA_WIDTH).max_element().min(1.0);

		if movement_from_input != 0.0 {
			// The zooming speed needed to cover a zoom ratio by inertia alone
			let zoom_terminal_speed = -ZOOM_FRICTION.ln();
			// Cap the zoom speed to the terminal speed to ease the movement near bounds
			let min_zoom_speed = (minimum_zoom / self.scale).ln() * zoom_terminal_speed;
			let max_zoom_speed = (maximum_zoom / self.scale).ln() * zoom_terminal_speed;
			inertia.zoom = movement_from_input.clamp(min_zoom_speed, max_zoom_speed);
		} else {
			inertia.zoom *= ZOOM_FRICTION.powf(delta_t);
		}

		self.scale *= 2f32.powf(inertia.zoom * delta_t);
		// Still clamp the positions, as a failsafe
		self.scale = self.scale.max(minimum_zoom).min(maximum_zoom);
	}

	fn camera_bounds(&self) -> Vec2 {
		self.level_size() / self.scale * Vec2::new(1.0, 1.0 / (1.0 - VERTICAL_PADDING_FRACTION))
	}

	fn level_size(&self) -> Vec2 {
		2.0 * self.level_bounds.half_size()
	}

	fn camera_movement_bounds(&self, vp_size: Vec2) -> Aabb2d {
		// +--------------------+------------+
		// |                    |viewport    |bounding box
		// |                    |            |
		// |          X - - - - + +          |
		// |          |         | |camera pan bounds
		// |          |     O   | |          |
		// +----------+---------+ |          |
		// |          + - - - - - +          |
		// |                                 |
		// |                                 |
		// +---------------------------------+
		//
		// O = Origin (center of world coordinate system and of level bounding box)
		// X = Center of viewport (harness.center)
		// (size of VP) = (size of BB, expanded to window aspect ratio) / scale
		// Pan bounds = BB - VP
		let bb_half_size = self.level_bounds.half_size();
		let mut expanded_bb_half_size = bb_half_size;
		if vp_size.x * bb_half_size.y > vp_size.y * bb_half_size.x {
			expanded_bb_half_size.x = bb_half_size.y * vp_size.x / vp_size.y;
		} else if vp_size.x * bb_half_size.y < vp_size.y * bb_half_size.x {
			expanded_bb_half_size.y = bb_half_size.x * vp_size.y / vp_size.x;
		}
		Aabb2d::new(
			Vec2::ZERO,
			(bb_half_size - expanded_bb_half_size / self.scale).max(Vec2::ZERO),
		)
	}

	fn update_pan(
		&mut self,
		inertia: &mut CameraInertia,
		movement_from_input: Vec2,
		viewport_size: Vec2,
		delta_t: f32,
	) {
		let movement_bounds = self.camera_movement_bounds(viewport_size);
		// The panning speed needed to cover a unit of distance by inertia alone
		let pan_terminal_speed = -PAN_FRICTION.ln();
		// Cap the pan speed to the terminal speed to ease the movement near bounds
		let min_pan_speed = (movement_bounds.min - self.center) * pan_terminal_speed;
		let max_pan_speed = (movement_bounds.max - self.center) * pan_terminal_speed;
		let clamped_input = movement_from_input.clamp(min_pan_speed, max_pan_speed);
		if movement_from_input.x != 0.0 {
			inertia.velocity.x = clamped_input.x;
		} else {
			inertia.velocity.x *= PAN_FRICTION.powf(delta_t);
		}
		if movement_from_input.y != 0.0 {
			inertia.velocity.y = clamped_input.y;
		} else {
			inertia.velocity.y *= PAN_FRICTION.powf(delta_t);
		}

		self.center += inertia.velocity * delta_t;
		// Still clamp the positions, as a failsafe
		self.center = movement_bounds.closest_point(self.center);
	}
}

fn update_camera(
	camera: Single<(
		&mut CameraHarness,
		&mut Projection,
		&mut Transform,
		&mut CameraInertia,
	)>,
	window: Single<&Window>,
	move_events: MessageReader<MoveCameraMessage>,
	zoom_events: MessageReader<ZoomCameraMessage>,
	time: Res<Time<Real>>,
) {
	let (mut harness, mut projection, mut camera_transform, mut inertia) = camera.into_inner();
	let viewport_size = window.size() * Vec2::new(1.0, 1.0 - VERTICAL_PADDING_FRACTION);

	let zoom_movement = zoom_movement_input(zoom_events);
	harness.update_zoom(&mut inertia, zoom_movement, time.delta_secs());
	// Calculate panning input after scale has been already updated
	let movement = pan_movement_input(move_events, harness.scale);
	harness.update_pan(&mut inertia, movement, viewport_size, time.delta_secs());

	let bounds = harness.camera_bounds();
	match projection.as_mut() {
		Projection::Orthographic(proj) => {
			proj.scaling_mode = ScalingMode::AutoMin {
				min_width: bounds.x,
				min_height: bounds.y,
			};
		}
		_ => {
			log::warn!("Camera has invalid (non-orthographic) projection.");
		}
	}

	camera_transform.translation.x = harness.center.x;
	camera_transform.translation.y = harness.center.y;
}
