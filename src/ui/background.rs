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
		.init_resource::<IsBackgroundHighlighted>();
}

#[derive(Resource, Clone, Copy, PartialEq, Eq, Deref, DerefMut, Debug, Default)]
struct IsBackgroundHighlighted(bool);

fn spawn_background(
	mut commands: Commands,
	mut meshes: ResMut<Assets<Mesh>>,
	mut materials: ResMut<Assets<BackgroundMaterial>>,
	images: Res<HandleMap<ImageKey>>,
) {
	const MESH_SIZE: f32 = 8000.0;
	let material = BackgroundMaterial {
		scale: Vec2::splat(MESH_SIZE / BACKGROUND_TILING),
		speed: Vec2::from_angle(BACKGROUND_ROTATION)
			.rotate(BACKGROUND_VELOCITY / BACKGROUND_TILING),
		texture: images[&ImageKey::Background].clone_weak(),
		colors: [
			Srgba::hex("F5F8FB").unwrap().into(),
			LinearRgba::WHITE,
			palettes::tailwind::SLATE_200.into(),
			Srgba::hex("F5F8FB").unwrap().into(),
		],
		sweep_origin: Vec2::new(0.0, MESH_SIZE / BACKGROUND_TILING),
		sweep_direction: Vec2::from_angle(BACKGROUND_ROTATION).rotate(-Vec2::Y),
		sweep_position: 0.0,
		sweep_width: 2.0,
	};
	commands.spawn((
		Mesh2d(meshes.add(Rectangle::from_length(MESH_SIZE).mesh())),
		MeshMaterial2d(materials.add(material)),
		Transform::from_translation(Vec3::Z * layers::BACKGROUND)
			.with_rotation(Quat::from_rotation_z(BACKGROUND_ROTATION)),
		Parallax(BACKGROUND_PARALLAX),
	));
}

fn flip_background_on_level_completion(
	mut is_highlighted: ResMut<IsBackgroundHighlighted>,
	is_completed: Res<IsLevelCompleted>,
	screen: Res<State<Screen>>,
) {
	let next_is_highlighted = is_completed.0 && **screen == Screen::Playing;
	// Only update if the value is different to take advantage of change detection
	is_highlighted.set_if_neq(IsBackgroundHighlighted(next_is_highlighted));
}

fn update_background_sweep(
	mut materials: ResMut<Assets<BackgroundMaterial>>,
	is_highlighted: Res<IsBackgroundHighlighted>,
	time: Res<Time>,
) {
	const SWEEP_SPEED: f32 = 8.0;
	if **is_highlighted {
		if is_highlighted.is_changed() {
			for (_, material) in materials.iter_mut() {
				// TODO: This may not look the same on screens of different sizes
				material.sweep_position = 10.0;
			}
		} else {
			let delta_position = time.delta_secs() * SWEEP_SPEED;
			for (_, material) in materials.iter_mut() {
				material.sweep_position += delta_position;
			}
		}
	} else if is_highlighted.is_changed() {
		for (_, material) in materials.iter_mut() {
			material.sweep_position = 0.0;
		}
	}
}
