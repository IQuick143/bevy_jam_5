//! Main rendering camera, its movement, and related effects

mod inputs;
pub mod parallax;

use crate::{
	game::{prelude::*, spawn::SpawnLevel},
	graphics::*,
	screen::{DoScreenTransition, Screen},
	AppSet,
};
use bevy::{
	math::bounding::{Aabb2d, BoundingVolume},
	render::camera::ScalingMode,
};
use inputs::{MoveCameraEvent, ZoomCameraEvent};

pub(super) fn plugin(app: &mut App) {
	app.add_plugins((inputs::plugin, parallax::plugin))
		.add_systems(Startup, spawn_camera)
		.add_systems(
			Update,
			(
				set_camera_on_screen_transition.run_if(on_event::<DoScreenTransition>),
				set_camera_level_view.run_if(on_event::<SpawnLevel>),
				update_camera.after(AppSet::RecordInput),
			)
				.chain(),
		);
}

#[derive(Component, Clone, Copy)]
#[require(Camera, Camera2d, CameraIntertia)]
pub struct CameraHarness {
	/// Zooming factor, bigger value means more zoomed, minumum is `1.0`
	pub scale: f32,
	/// The area of the level, maximum area that is intended to be viewed.
	pub level_bounds: Aabb2d,
	/// The world-coordinate point the camera is looking at in the center of its view.
	pub center: Vec2,
}

/// Support component for [`CameraHarness`] that tracks
/// persistent camera state
#[derive(Component, Clone, Copy, Default)]
struct CameraIntertia {
	pub velocity: Vec2,
	pub zoom: f32,
}

impl Default for CameraHarness {
	fn default() -> Self {
		let bounds = Aabb2d::new(LEVEL_AREA_CENTER, LEVEL_AREA_WIDTH / 2.0);
		Self {
			scale: 1.0,
			level_bounds: bounds,
			center: LEVEL_AREA_CENTER,
		}
	}
}

fn spawn_camera(mut commands: Commands) {
	commands.spawn((
		Name::new("Camera"),
		Camera2d,
		CameraHarness::default(),
		Camera {
			clear_color: ClearColorConfig::Custom(Color::WHITE),
			..default()
		},
		Projection::Orthographic(OrthographicProjection {
			scaling_mode: ScalingMode::AutoMin {
				min_width: GAME_AREA.x,
				min_height: GAME_AREA.y,
			},
			..OrthographicProjection::default_2d()
		}),
		// Render all UI to this camera.
		// Not strictly necessary since we only use one camera,
		// but if we don't use this component, our UI will disappear as soon
		// as we add another camera. This includes indirect ways of adding cameras like using
		// [ui node outlines](https://bevyengine.org/news/bevy-0-14/#ui-node-outline-gizmos)
		// for debugging. So it's good to have this here for future-proofing.
		IsDefaultUiCamera,
	));
}

fn set_camera_on_screen_transition(
	mut camera: Single<&mut CameraHarness>,
	mut events: EventReader<DoScreenTransition>,
) {
	let mut reset_camera = false;
	for event in events.read() {
		if event.0 != Screen::Playing {
			reset_camera = true;
		}
	}
	if !reset_camera {
		return;
	}
	**camera = CameraHarness::default();
}

fn set_camera_level_view(
	mut camera: Single<&mut CameraHarness>,
	mut events: EventReader<SpawnLevel>,
	levels: Res<Assets<LevelData>>,
) {
	if let Some(level) = events
		.read()
		.filter_map(|SpawnLevel(handle, _)| levels.get(handle))
		.last()
	{
		**camera = CameraHarness {
			center: level.initial_camera_pos,
			level_bounds: level.bounding_box.grow(SPRITE_SIZE / 2.0),
			// If this is out of range (or NaN),
			// it is clamped in update_camera in the same frame
			scale: level.initial_zoom,
		};
	}
}

/// Fraction of inertia that is conserved after one second
const PAN_FRICTION: f32 = 0.0005;

/// World units covered per second when at 1:1 zoom
const PAN_SPEED: f32 = 600.0;

/// Fraction of inertia that is conserved after one second
const ZOOM_FRICTION: f32 = PAN_FRICTION;

/// Orders of magnitude of zoom covered per second
const ZOOM_SPEED: f32 = 1.0;

fn update_camera(
	camera: Single<(
		&mut CameraHarness,
		&mut Projection,
		&mut Transform,
		&mut CameraIntertia,
	)>,
	window: Single<&Window>,
	mut move_events: EventReader<MoveCameraEvent>,
	mut zoom_events: EventReader<ZoomCameraEvent>,
	time: Res<Time<Real>>,
) {
	let (mut harness, mut projection, mut transform, mut inertia) = camera.into_inner();

	let zoom_movement = zoom_events
		.read()
		.map(|event| match event {
			ZoomCameraEvent::In => 1.0,
			ZoomCameraEvent::Out => -1.0,
		})
		.sum::<f32>()
		* ZOOM_SPEED;

	let level_size = 2.0 * harness.level_bounds.half_size();
	// Allow zooming in to double g11 scale, or to see the whole level
	// if it is too small
	let maximum_zoom = (level_size / LEVEL_AREA_WIDTH * 2.0).max_element().max(1.0);
	// Allow zooming out to see the whole level,
	// or to g11 scale if the level is smaller than that
	let minimum_zoom = (level_size / LEVEL_AREA_WIDTH).max_element().min(1.0);

	if zoom_movement != 0.0 {
		// The zooming speed needed to cover a zoom ratio by inertia alone
		let zoom_terminal_speed = -ZOOM_FRICTION.ln();
		// Cap the zoom speed to the terminal speed to ease the movement near bounds
		let min_zoom_speed = (minimum_zoom / harness.scale).ln() * zoom_terminal_speed;
		let max_zoom_speed = (maximum_zoom / harness.scale).ln() * zoom_terminal_speed;
		inertia.zoom = zoom_movement.clamp(min_zoom_speed, max_zoom_speed);
	} else {
		inertia.zoom *= ZOOM_FRICTION.powf(time.delta_secs());
	}

	harness.scale *= 2f32.powf(inertia.zoom * time.delta_secs());
	// Still clamp the positions, as a failsafe
	harness.scale = harness.scale.max(minimum_zoom).min(maximum_zoom);

	let bounds =
		level_size / harness.scale * Vec2::new(1.0, 1.0 / (1.0 - VERTICAL_PADDING_FRACTION));

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
	let bb_half_size = harness.level_bounds.half_size();
	let vp_size = window.size() * Vec2::new(1.0, 1.0 - VERTICAL_PADDING_FRACTION);
	let mut expanded_bb_half_size = bb_half_size;
	if vp_size.x * bb_half_size.y > vp_size.y * bb_half_size.x {
		expanded_bb_half_size.x = bb_half_size.y * vp_size.x / vp_size.y;
	} else if vp_size.x * bb_half_size.y < vp_size.y * bb_half_size.x {
		expanded_bb_half_size.y = bb_half_size.x * vp_size.y / vp_size.x;
	}
	let movement_bounds = Aabb2d::new(
		Vec2::ZERO,
		(bb_half_size - expanded_bb_half_size / harness.scale).max(Vec2::ZERO),
	);

	let movement =
		move_events.read().map(|event| event.0).sum::<Vec2>() * PAN_SPEED / harness.scale;

	// The panning speed needed to cover a unit of distance by inertia alone
	let pan_terminal_speed = -PAN_FRICTION.ln();
	// Cap the pan speed to the terminal speed to ease the movement near bounds
	let min_pan_speed = (movement_bounds.min - harness.center) * pan_terminal_speed;
	let max_pan_speed = (movement_bounds.max - harness.center) * pan_terminal_speed;
	if movement.x != 0.0 {
		inertia.velocity.x = movement.x.clamp(min_pan_speed.x, max_pan_speed.x);
	} else {
		inertia.velocity.x *= PAN_FRICTION.powf(time.delta_secs());
	}
	if movement.y != 0.0 {
		inertia.velocity.y = movement.y.clamp(min_pan_speed.y, max_pan_speed.y);
	} else {
		inertia.velocity.y *= PAN_FRICTION.powf(time.delta_secs());
	}

	harness.center += inertia.velocity * time.delta_secs();
	// Still clamp the positions, as a failsafe
	harness.center = movement_bounds.closest_point(harness.center);

	transform.translation.x = harness.center.x;
	transform.translation.y = harness.center.y;
}
