//! Animated backdrop pattern

use crate::{
	assets::{HandleMap, ImageKey},
	game::camera::Parallax,
	graphics::*,
};
use bevy::prelude::*;

pub fn plugin(app: &mut App) {
	app.init_resource::<BackgroundMaterialHandle>()
		.init_resource::<IsBackgroundEnabled>()
		.add_systems(Startup, spawn_background)
		.add_systems(
			Update,
			toggle_background_visibility.run_if(resource_changed::<IsBackgroundEnabled>),
		);
}

/// Determines whether the animated background should be rendered
#[derive(Resource, Clone, Copy, PartialEq, Eq, Debug, Deref, DerefMut)]
pub struct IsBackgroundEnabled(pub bool);

impl Default for IsBackgroundEnabled {
	fn default() -> Self {
		// Background should be rendered by default
		Self(true)
	}
}

#[derive(Resource, Clone, Debug, Deref, DerefMut)]
struct BackgroundMaterialHandle(Handle<ScrollingTextureMaterial>);

const MESH_SIZE: f32 = 8000.0;

impl FromWorld for BackgroundMaterialHandle {
	fn from_world(world: &mut World) -> Self {
		let images = world.resource::<HandleMap<ImageKey>>();
		let texture = images[&ImageKey::Background].clone_weak();
		let mut materials = world.resource_mut::<Assets<ScrollingTextureMaterial>>();
		let material = ScrollingTextureMaterial {
			scale: Vec2::splat(MESH_SIZE / BACKGROUND_TILING),
			speed: Vec2::from_angle(BACKGROUND_ROTATION)
				.rotate(BACKGROUND_VELOCITY / BACKGROUND_TILING),
			texture,
		};
		let handle = materials.add(material);
		Self(handle)
	}
}

/// Marker component that allows querying for the background mesh
#[derive(Component, Clone, Copy, PartialEq, Eq, Debug, Default)]
struct IsBackground;

fn spawn_background(
	mut commands: Commands,
	mut meshes: ResMut<Assets<Mesh>>,
	material: Res<BackgroundMaterialHandle>,
) {
	commands.spawn((
		Mesh2d(meshes.add(Rectangle::from_length(MESH_SIZE).mesh())),
		MeshMaterial2d(material.clone_weak()),
		Transform::from_translation(Vec3::Z * layers::BACKGROUND)
			.with_rotation(Quat::from_rotation_z(BACKGROUND_ROTATION)),
		Parallax(BACKGROUND_PARALLAX),
		IsBackground,
	));
}

fn toggle_background_visibility(
	mut query: Query<&mut Visibility, With<IsBackground>>,
	render_background: Res<IsBackgroundEnabled>,
) {
	let new_visibility = if **render_background {
		Visibility::default()
	} else {
		Visibility::Hidden
	};
	for mut visibility in &mut query {
		*visibility = new_visibility;
	}
}
