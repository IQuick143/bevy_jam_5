//! Material used for the background image

use bevy::{
	prelude::*,
	render::render_resource::{AsBindGroup, ShaderRef},
	sprite::{Material2d, Material2dPlugin},
};

pub(super) fn plugin(app: &mut App) {
	app.add_plugins(Material2dPlugin::<BackgroundMaterial>::default());
}

/// Material that renders the background
/// - Scrolls along a direction
/// - Renders in two colors, those are configurable
/// - Smoothly transitions between color palettes
#[derive(Asset, TypePath, AsBindGroup, Clone, Debug)]
pub struct BackgroundMaterial {
	/// How many times the texture repeats over the size of the primitive
	#[uniform(0)]
	pub scale: Vec2,
	/// Speed of the movement of the texture,
	/// in sizes of the texture per second
	#[uniform(1)]
	pub speed: Vec2,
	/// The texture image
	///
	/// The actual material is rendered with two solid colors.
	/// - Red channel of the texture indicates the interpolation factor
	///   between those colors
	/// - Green channel determines the timing offsets during a palette change
	/// - Blue channel determines the time the palette change takes
	#[texture(2)]
	#[sampler(3)]
	pub texture: Handle<Image>,
	/// Colors that the material should render in
	///
	/// Primary foreground, primary background, secondary foreground, secondary background
	#[uniform(4)]
	pub colors: [LinearRgba; 4],
	/// Position on the primitive where the color transition boundary is emited from,
	/// in sizes of the texture
	///
	/// This will usualy be one of the corners of the primitive:
	/// `(x, y) \in {0, scale.x} * {0, scale.y}`
	#[uniform(5)]
	pub sweep_origin: Vec2,
	/// Direction in which the color transition propagates
	///
	/// Should be a [`Dir2`], but that does not implement [`bevy::render::render_resource::ShaderType`]
	#[uniform(6)]
	pub sweep_direction: Vec2,
	/// Distance of the start of the sweep boundary from [`Self::sweep_origin`],
	/// in sizes of the texture
	#[uniform(7)]
	pub sweep_position: f32,
	/// Width of the color transition boundary, in sizes of the texture
	#[uniform(8)]
	pub sweep_width: f32,
}

impl Material2d for BackgroundMaterial {
	fn fragment_shader() -> ShaderRef {
		"shaders/background.wgsl".into()
	}
}
