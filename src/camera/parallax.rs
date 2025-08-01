//! Parallax effect on camera movement

use super::{movement::CameraPositionUpdate, CameraHarness};
use bevy::prelude::*;

pub(super) fn plugin(app: &mut App) {
	app.init_resource::<EnableParallax>().add_systems(
		Update,
		(
			init_parallax,
			apply_paralax
				.run_if(resource_equals(EnableParallax(true)))
				.after(CameraPositionUpdate),
		),
	);
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
/// and scale of the entity before parallax was applied.
#[derive(Component, Clone, Copy)]
struct ParallaxBasis {
	position: Vec2,
	scale: Vec2,
}

fn init_parallax(mut commands: Commands, query: Query<(Entity, &Transform), Added<Parallax>>) {
	for (id, transform) in &query {
		commands.entity(id).insert(ParallaxBasis {
			position: transform.translation.xy(),
			scale: transform.scale.xy(),
		});
	}
}

fn apply_paralax(
	mut query: Query<(&mut Transform, &Parallax, &ParallaxBasis), Without<Camera2d>>,
	camera_q: Query<(&Transform, &CameraHarness), (With<Camera2d>, Changed<Transform>)>,
) {
	let Ok((camera_transform, camera_harness)) = camera_q.single() else {
		return;
	};
	for (mut transform, Parallax(parallax), basis) in &mut query {
		let new_scale = basis.scale / camera_harness.scale.powf(*parallax);
		let new_position = basis.position + camera_transform.translation.xy() * parallax;
		transform.translation.x = new_position.x;
		transform.translation.y = new_position.y;
		transform.scale.x = new_scale.x;
		transform.scale.y = new_scale.y;
	}
}
