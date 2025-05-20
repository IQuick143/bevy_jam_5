use bevy::{
	math::bounding::{Aabb2d, BoundingVolume},
	render::camera::ScalingMode,
};

use crate::{
	game::prelude::*,
	graphics::{
		GAME_AREA, LEVEL_AREA_CENTER, LEVEL_AREA_WIDTH, SPRITE_SIZE, VERTICAL_PADDING_FRACTION,
	},
	screen::{DoScreenTransition, Screen},
	AppSet,
};

use crate::game::spawn::SpawnLevel;

use super::inputs::{MoveCameraEvent, ZoomCameraEvent};

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

/// Allows turning off the parallax effect, because this seems exactly like
/// something that we want to be configurable
#[derive(Resource, Clone, Copy, PartialEq, Eq)]
pub struct EnableParallax(pub bool);

impl Default for EnableParallax {
	fn default() -> Self {
		// Parallax is enabled by default
		Self(true)
	}
}

/// This component applies a parallax effect to the [`Transform`] of an entity.
/// The value of parallax should be in [0, 1] (0 means no parallax,
/// 1 means the entity will follow the camera).
///
/// The parallaxed entity's [`Transform`] must not be changed
/// by other systems. Parallax only works on static entities.
///
/// Assumes there is only one camera. We can worry about the alternative
/// when we need another camera.
///
/// This cannot be applied to a camera entity, for obvious reasons.
#[derive(Component, Clone, Copy)]
pub struct Parallax(pub f32);

/// Support component for [`Parallax`], records the default position
/// of the entity before parallax was applied.
#[derive(Component, Clone, Copy)]
struct ParallaxBasis(Vec2);

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

pub fn plugin(app: &mut App) {
	app
		// Spawn the main camera.
		.add_systems(Startup, spawn_camera)
		.add_systems(
			Update,
			(
				init_parallax,
				(
					set_camera_on_screen_transition.run_if(on_event::<DoScreenTransition>),
					set_camera_level_view.run_if(on_event::<SpawnLevel>),
					update_camera.after(AppSet::RecordInput),
					apply_paralax.run_if(resource_equals(EnableParallax(true))),
				)
					.chain(),
			),
		)
		.init_resource::<EnableParallax>();
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
			center: level.bounding_box.center(),
			level_bounds: level.bounding_box.grow(SPRITE_SIZE / 2.0),
			scale: 1.0,
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

	// Margin tells how far from the bounds should we already stop
	// accepting input so we just get to it by inertia
	let zoom_margin = 2f32.powf(ZOOM_SPEED / ZOOM_FRICTION.ln());
	// Reject the input if we are past the margins
	let accelerate_zoom = (harness.scale > minimum_zoom / zoom_margin && zoom_movement < 0.0)
		|| (harness.scale < maximum_zoom * zoom_margin && zoom_movement > 0.0);
	if accelerate_zoom {
		inertia.zoom = zoom_movement;
	} else {
		inertia.zoom *= ZOOM_FRICTION.powf(time.delta_secs());
	}

	harness.scale *= 2f32.powf(inertia.zoom * time.delta_secs());
	// Still clamp the positions, as a failsafe
	harness.scale = harness.scale.clamp(minimum_zoom, maximum_zoom);

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

	let movement =
		move_events.read().map(|event| event.0).sum::<Vec2>() * PAN_SPEED / harness.scale;

	// Margin tells how far from the bounds should we already stop
	// accepting input so we just get to it by inertia
	let pan_margin = -PAN_SPEED / PAN_FRICTION.ln() / harness.scale;
	// Reject the input if we are past the margins
	let accelerate_x = (harness.level_bounds.min.x + pan_margin < harness.center.x
		&& movement.x < 0.0)
		|| (harness.level_bounds.max.x - pan_margin > harness.center.x && movement.x > 0.0);
	let accelerate_y = (harness.level_bounds.min.y + pan_margin < harness.center.y
		&& movement.y < 0.0)
		|| (harness.level_bounds.max.y - pan_margin > harness.center.y && movement.y > 0.0);
	if accelerate_x {
		inertia.velocity.x = movement.x;
	} else {
		inertia.velocity.x *= PAN_FRICTION.powf(time.delta_secs());
	}
	if accelerate_y {
		inertia.velocity.y = movement.y;
	} else {
		inertia.velocity.y *= PAN_FRICTION.powf(time.delta_secs());
	}

	harness.center += inertia.velocity * time.delta_secs();
	// Still clamp the positions, as a failsafe
	harness.center = harness.level_bounds.closest_point(harness.center);

	transform.translation.x = harness.center.x;
	transform.translation.y = harness.center.y;
}

fn init_parallax(mut commands: Commands, query: Query<(Entity, &Transform), Added<Parallax>>) {
	for (id, transform) in &query {
		commands
			.entity(id)
			.insert(ParallaxBasis(transform.translation.xy()));
	}
}

fn apply_paralax(
	mut query: Query<(&mut Transform, &Parallax, &ParallaxBasis), Without<Camera2d>>,
	camera_q: Query<&Transform, (With<Camera2d>, Changed<Transform>)>,
) {
	let Ok(camera_transform) = camera_q.single() else {
		return;
	};
	for (mut transform, Parallax(parallax), ParallaxBasis(basis)) in &mut query {
		let new_position = basis + camera_transform.translation.xy() * parallax;
		transform.translation.x = new_position.x;
		transform.translation.y = new_position.y;
	}
}
