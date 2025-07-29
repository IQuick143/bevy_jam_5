//! Animated background of the screen

use crate::{
	assets::{HandleMap, ImageKey},
	game::{camera::Parallax, logic::IsLevelCompleted},
	graphics::*,
	screen::Screen,
};
use bevy::{color::palettes, prelude::*};

pub fn plugin(app: &mut App) {
	app.add_systems(Startup, spawn_background)
		.add_systems(
			Update,
			(flip_background_on_level_completion, update_background_sweep),
		)
		.init_resource::<IsBackgroundHighlighted>()
		.init_resource::<BackgroundHighlightParameters>();
}

#[derive(Resource, Clone, Copy, PartialEq, Eq, Deref, DerefMut, Debug, Default)]
struct IsBackgroundHighlighted(bool);

#[derive(Resource, Clone, Copy, Debug, Default)]
struct BackgroundHighlightParameters {
	starting_sweep: f32,
	sweep_speed: f32,
}

/// Size of the mesh that is used to render the background, in world units
const MESH_SIZE: f32 = 8000.0;

fn spawn_background(
	mut commands: Commands,
	mut meshes: ResMut<Assets<Mesh>>,
	mut materials: ResMut<Assets<BackgroundMaterial>>,
	images: Res<HandleMap<ImageKey>>,
) {
	let material = BackgroundMaterial {
		texture: images[&ImageKey::Background].clone_weak(),
		params: BackgroundMaterialParams {
			scale: Vec2::splat(MESH_SIZE / BACKGROUND_TILING),
			speed: Vec2::from_angle(BACKGROUND_ROTATION)
				.rotate(BACKGROUND_VELOCITY / BACKGROUND_TILING),
			colors: [
				Srgba::hex("F5F8FB").unwrap().into(),
				LinearRgba::WHITE,
				palettes::tailwind::SLATE_200.into(),
				Srgba::hex("F5F8FB").unwrap().into(),
			],
			sweep_origin: Vec2::new(0.0, MESH_SIZE / BACKGROUND_TILING),
			sweep_direction: Vec2::from_angle(BACKGROUND_ROTATION).rotate(-Vec2::Y),
			sweep_position: 0.0,
			sweep_width: 1.0,
		},
	};
	commands.spawn((
		Mesh2d(meshes.add(Rectangle::from_length(MESH_SIZE).mesh())),
		MeshMaterial2d(materials.add(material)),
		Transform::from_translation(Vec3::Z * layers::BACKGROUND)
			.with_rotation(Quat::from_rotation_z(BACKGROUND_ROTATION)),
		Parallax(BACKGROUND_PARALLAX),
	));
}

/// Seconds it takes for the sweep animation to cover the height of the viewport
const SWEEP_TIME: f32 = 0.5;

fn flip_background_on_level_completion(
	mut is_highlighted: ResMut<IsBackgroundHighlighted>,
	mut highlight: ResMut<BackgroundHighlightParameters>,
	is_completed: Res<IsLevelCompleted>,
	screen: Res<State<Screen>>,
	camera: Single<(&Camera, &GlobalTransform)>,
) {
	let next_is_highlighted = is_completed.0 && **screen == Screen::Playing;
	// Only update if the value is different to take advantage of change detection
	if **is_highlighted != next_is_highlighted {
		**is_highlighted = next_is_highlighted;
		if next_is_highlighted {
			let (camera, camera_transform) = *camera;
			let Some(top) = camera.ndc_to_world(camera_transform, Vec3::new(0.0, 1.0, 1.0)) else {
				warn!("Could not convert camera bounds to world coordinates");
				return;
			};
			let Some(bottom) = camera.ndc_to_world(camera_transform, Vec3::new(0.0, -1.0, 1.0))
			else {
				warn!("Could not convert camera bounds to world coordinates");
				return;
			};
			// Top and bottom of the viewport, in world coordinates
			let top = top.y;
			let bottom = bottom.y;
			let viewport_height = top - bottom;
			// Calibrate the sweep parameters to start at the bottom of the screen
			// and reach the top after a fixed time
			highlight.starting_sweep = ((BACKGROUND_ROTATION.sin() + BACKGROUND_ROTATION.cos())
				* MESH_SIZE / 2.0
				+ bottom) / BACKGROUND_TILING;
			highlight.sweep_speed = viewport_height / BACKGROUND_TILING / SWEEP_TIME;
		}
	}
}

fn update_background_sweep(
	mut materials: ResMut<Assets<BackgroundMaterial>>,
	is_highlighted: Res<IsBackgroundHighlighted>,
	highlight: Res<BackgroundHighlightParameters>,
	time: Res<Time>,
) {
	if **is_highlighted {
		if is_highlighted.is_changed() {
			for (_, material) in materials.iter_mut() {
				material.params.sweep_position = highlight.starting_sweep;
			}
		} else {
			let delta_position = time.delta_secs() * highlight.sweep_speed;
			for (_, material) in materials.iter_mut() {
				material.params.sweep_position += delta_position;
			}
		}
	} else if is_highlighted.is_changed() {
		for (_, material) in materials.iter_mut() {
			material.params.sweep_position = 0.0;
		}
	}
}
