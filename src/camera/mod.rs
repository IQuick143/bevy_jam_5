//! Main rendering camera, its movement, and related effects

mod inputs;
mod movement;
pub mod parallax;

use crate::{
	game::{prelude::*, spawn::SpawnLevel},
	graphics::*,
	screen::{DoScreenTransition, Screen},
};
use bevy::{
	math::bounding::{Aabb2d, BoundingVolume as _},
	render::camera::ScalingMode,
};
use movement::CameraInertia;

pub(super) fn plugin(app: &mut App) {
	app.add_plugins((inputs::plugin, movement::plugin, parallax::plugin))
		.add_systems(Startup, spawn_camera)
		.add_systems(
			Update,
			(
				set_camera_on_screen_transition.run_if(on_message::<DoScreenTransition>),
				set_camera_level_view.run_if(on_message::<SpawnLevel>),
			)
				.chain(),
		);
}

#[derive(Component, Clone, Copy)]
#[require(Camera, Camera2d, CameraInertia)]
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
	mut events: MessageReader<DoScreenTransition>,
) {
	// Reset the camera when switching to a screen other than Playing
	// Playing screen will set its own camera bounds with SpawnLevel
	if events.read().any(|e| e.0 != Screen::Playing) {
		**camera = CameraHarness::default();
	}
}

fn set_camera_level_view(
	mut camera: Single<&mut CameraHarness>,
	mut events: MessageReader<SpawnLevel>,
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
