use std::f32::consts::PI;

use crate::{
	assets::{HandleMap, ImageKey},
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
	mut materials: ResMut<Assets<ScrollingTextureMaterial>>,
	images: Res<HandleMap<ImageKey>>,
) {
	const MESH_SIZE: f32 = 8000.0;
	let material = ScrollingTextureMaterial {
		scale: Vec2::splat(MESH_SIZE / BACKGROUND_TILING),
		speed: Vec2::from_angle(BACKGROUND_ROTATION)
			.rotate(BACKGROUND_VELOCITY / BACKGROUND_TILING),
		texture: images[&ImageKey::Background].clone_weak(),
	};
	commands.spawn((
		Mesh2d(meshes.add(Rectangle::from_length(MESH_SIZE).mesh())),
		MeshMaterial2d(materials.add(material)),
		Transform::from_translation(Vec3::Z * layers::BACKGROUND)
			.with_rotation(Quat::from_rotation_z(BACKGROUND_ROTATION)),
	));
}
