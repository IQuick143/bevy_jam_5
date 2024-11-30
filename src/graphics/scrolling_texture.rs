//! The animated scrolling texture material.

use bevy::{
	prelude::*,
	render::render_resource::{AsBindGroup, ShaderRef},
	sprite::{AlphaMode2d, Material2d, Material2dPlugin},
};

pub(super) fn plugin(app: &mut App) {
	app.add_plugins(Material2dPlugin::<ScrollingTextureMaterial>::default());
}

/// Material that renders a texture that repeats over a primitive
/// and moves steadily across the primitive.
#[derive(Asset, TypePath, AsBindGroup, Clone, Debug)]
pub struct ScrollingTextureMaterial {
	/// How many times the texture repeats over the size of the primitive
	#[uniform(0)]
	pub scale: Vec2,
	/// Speed of the movement of the texture,
	/// in sizes of the texture per second
	#[uniform(1)]
	pub speed: Vec2,
	/// The texture image
	#[texture(2)]
	#[sampler(3)]
	pub texture: Handle<Image>,
}

impl Default for ScrollingTextureMaterial {
	fn default() -> Self {
		Self {
			scale: Vec2::ONE,
			speed: Vec2::ZERO,
			texture: default(),
		}
	}
}

impl Material2d for ScrollingTextureMaterial {
	fn fragment_shader() -> ShaderRef {
		"shaders/scrolling_texture.wgsl".into()
	}

	fn alpha_mode(&self) -> AlphaMode2d {
		AlphaMode2d::Blend
	}
}
