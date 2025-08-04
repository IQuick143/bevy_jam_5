//! Animated backdrop pattern

use crate::{
	assets::{HandleMap, ImageKey},
	game::camera::Parallax,
	graphics::*,
};
use bevy::prelude::*;

pub(super) fn plugin(app: &mut App) {
	app.init_resource::<BackgroundMaterialHandle>()
		.add_systems(Startup, spawn_background)
		.add_observer(set_background_mode);
}

/// Display modes that describe how background is rendered
///
/// This is also a trigger event that updates the display mode
#[derive(Event, Clone, Copy, PartialEq, Eq, Debug, Default)]
pub enum BackgroundMode {
	/// Background is not rendered
	None,
	/// Background is a static image
	Static,
	/// Background moves at a steady rate
	#[default]
	Animated,
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

fn set_background_mode(
	mode: Trigger<BackgroundMode>,
	material: Res<BackgroundMaterialHandle>,
	mut materials: ResMut<Assets<ScrollingTextureMaterial>>,
	mut query: Query<&mut Visibility, With<IsBackground>>,
) {
	let material = materials
		.get_mut(&**material)
		.expect("Background material should have been inserted");
	let new_visibility;
	match mode.event() {
		BackgroundMode::None => {
			new_visibility = Visibility::Hidden;
		}
		BackgroundMode::Static => {
			new_visibility = Visibility::default();
			material.speed = Vec2::ZERO;
		}
		BackgroundMode::Animated => {
			new_visibility = Visibility::default();
			material.speed = Vec2::from_angle(BACKGROUND_ROTATION)
				.rotate(BACKGROUND_VELOCITY / BACKGROUND_TILING);
		}
	}
	for mut visibility in &mut query {
		*visibility = new_visibility;
	}
}
