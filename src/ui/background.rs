use std::f32::consts::PI;

use crate::{
	assets::{HandleMap, ImageKey},
	game::camera::Parallax,
	graphics::*,
};
use bevy::{color::palettes, prelude::*};

use super::palette;

pub fn plugin(app: &mut App) {
	app.add_systems(Startup, spawn_background);
}

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
		sweep_width: 3.0,
	};
	commands.spawn((
		Mesh2d(meshes.add(Rectangle::from_length(MESH_SIZE).mesh())),
		MeshMaterial2d(materials.add(material)),
		Transform::from_translation(Vec3::Z * layers::BACKGROUND)
			.with_rotation(Quat::from_rotation_z(BACKGROUND_ROTATION)),
		Parallax(BACKGROUND_PARALLAX),
	));
}
