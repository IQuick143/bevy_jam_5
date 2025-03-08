use bevy::{
	math::bounding::{Aabb2d, BoundingVolume},
	render::camera::ScalingMode,
};

use crate::{
	game::prelude::*,
	graphics::{
		GAME_AREA, LEVEL_AREA_CENTER, LEVEL_AREA_WIDTH, SPRITE_LENGTH, SPRITE_SIZE,
		VERTICAL_PADDING_FRACTION,
	},
	screen::{DoScreenTransition, Screen},
	AppSet,
};

use crate::game::spawn::SpawnLevel;

use super::inputs::{MoveCameraEvent, ZoomCameraEvent};

#[derive(Component, Clone, Copy)]
#[require(Camera, Camera2d)]
pub struct CameraHarness {
	/// Zooming factor, bigger value means more zoomed, minumum is `1.0`
	pub scale: f32,
	/// The area of the level, maximum area that is intended to be viewed.
	pub level_bounds: Aabb2d,
	/// The world-coordinate point the camera is looking at in the center of its view.
	pub center: Vec2,
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
				set_camera_on_screen_transition.run_if(on_event::<DoScreenTransition>),
				set_camera_level_view.run_if(on_event::<SpawnLevel>),
				update_camera.after(AppSet::RecordInput),
			)
				.chain(),
		);
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
		OrthographicProjection {
			scaling_mode: ScalingMode::AutoMin {
				min_width: GAME_AREA.x,
				min_height: GAME_AREA.y,
			},
			..OrthographicProjection::default_2d()
		},
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
	for SpawnLevel(level_handle, _) in events.read() {
		if let Some(level) = levels.get(level_handle) {
			**camera = CameraHarness {
				center: level.bounding_box.center(),
				level_bounds: level.bounding_box.grow(SPRITE_SIZE / 2.0),
				scale: 1.0,
			};
		}
	}
}

fn update_camera(
	camera: Single<(
		&mut CameraHarness,
		&mut OrthographicProjection,
		&mut Transform,
	)>,
	mut move_events: EventReader<MoveCameraEvent>,
	mut zoom_events: EventReader<ZoomCameraEvent>,
	time: Res<Time<Real>>,
) {
	let (mut harness, mut projection, mut transform) = camera.into_inner();

	harness.scale *= zoom_events.read().map(|event| event.0).product::<f32>();

	let level_size = 2.0 * harness.level_bounds.half_size();

	let maximum_zoom = level_size.max_element() / SPRITE_LENGTH;
	harness.scale = harness.scale.max(1.0).min(maximum_zoom);

	let bounds =
		level_size / harness.scale * Vec2::new(1.0, 1.0 / (1.0 - VERTICAL_PADDING_FRACTION));

	projection.scaling_mode = ScalingMode::AutoMin {
		min_width: bounds.x,
		min_height: bounds.y,
	};

	let movement = move_events
		.read()
		.map(|event| event.0)
		.reduce(<Vec2 as std::ops::Add>::add)
		.unwrap_or_default();

	harness.center += bounds * movement * time.delta_secs();

	harness.center = harness.level_bounds.closest_point(harness.center);

	transform.translation.x = harness.center.x;
	transform.translation.y = harness.center.y;
}
