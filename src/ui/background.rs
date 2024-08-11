use std::f32::consts::PI;

use bevy::{color::palettes, prelude::*};

use crate::{
	assets::{HandleMap, ImageKey},
	graphics::{layers, BACKGROUND_TILING, BACKGROUND_VELOCITY},
};

use super::palette;

pub fn plugin(app: &mut App) {
	app.add_systems(Startup, spawn_background)
		.add_systems(Update, update_background);
}

/// Background visuals
#[derive(Component, Debug, Clone, Copy, Default, Reflect)]
struct Background;

fn spawn_background(mut commands: Commands, images: Res<HandleMap<ImageKey>>) {
	commands
		.spawn((
			TransformBundle::from_transform(Transform::from_rotation(Quat::from_rotation_z(
				PI / 5.0,
			))),
			VisibilityBundle::default(),
		))
		.with_children(|builder| {
			builder.spawn((
				SpriteBundle {
					sprite: Sprite {
						custom_size: Some(Vec2::splat(8000.0)),
						..default()
					},
					texture: images[&ImageKey::Background].clone_weak(),
					transform: Transform::from_translation(Vec2::ZERO.extend(layers::BACKGROUND)),
					visibility: Visibility::Visible,
					..Default::default()
				},
				ImageScaleMode::Tiled {
					tile_x: true,
					tile_y: true,
					stretch_value: BACKGROUND_TILING / 512.0,
				},
				Background,
			));
		});
}

fn update_background(
	mut background_query: Query<&mut Transform, With<Background>>,
	time: Res<Time<Real>>,
) {
	for mut transform in background_query.iter_mut() {
		transform.translation += (BACKGROUND_VELOCITY * time.delta_seconds()).extend(0.0);
		if transform.translation.x > BACKGROUND_TILING {
			transform.translation -= BACKGROUND_TILING * Vec3::X;
		}
		if transform.translation.x < -BACKGROUND_TILING {
			transform.translation -= -BACKGROUND_TILING * Vec3::X;
		}
		if transform.translation.y > BACKGROUND_TILING {
			transform.translation -= BACKGROUND_TILING * Vec3::Y;
		}
		if transform.translation.y < -BACKGROUND_TILING {
			transform.translation -= -BACKGROUND_TILING * Vec3::Y;
		}
	}
}
