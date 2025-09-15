//! Custom primitives for mesh generation

use bevy::{
	mesh::{Indices, PrimitiveTopology},
	prelude::*,
};
use std::f32::consts::{PI, TAU};

/// A rectangle with rounded corners
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct RoundedRectangle {
	/// Half of the height and width of the rectangle
	pub half_size: Vec2,
	/// Radius of the rounded corners
	pub corner_radius: f32,
}

/// A pentagon arrow with rounded corners.
/// The arrow points upwards.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct RoundedPentagonArrow {
	/// Half of the height and width of the main rectangle body
	pub half_size: Vec2,
	/// Radius of the rounded corners
	pub corner_radius: f32,
	/// Height of the triangular tip
	pub tip_length: f32,
}

/// Builder for building a [`Mesh`] from a [`RoundedRectangle`]
#[derive(Clone, Copy, Debug)]
pub struct RoundedRectangleMeshBuilder {
	/// The prmitive to be meshed
	pub primitive: RoundedRectangle,
	/// How many vertices should be used for each
	/// of the primitive's rounded corners (default 8)
	pub resolution: usize,
}

/// Builder for building a [`Mesh`] from a [`RoundedPentagonArrow`]
#[derive(Clone, Copy, Debug)]
pub struct RoundedPentagonArrowMeshBuilder {
	/// The primitive to be meshed
	pub primitive: RoundedPentagonArrow,
	/// How many vertices should be used for 90 degrees
	/// of the primitive's rounded corner (default 8)
	///
	/// The two square corners will have this many vertices.
	/// The rest will have a variable amount, depending on
	/// their respective angles.
	pub resolution: usize,
}

impl Primitive2d for RoundedRectangle {}
impl Primitive2d for RoundedPentagonArrow {}

impl From<Rectangle> for RoundedRectangle {
	fn from(value: Rectangle) -> Self {
		Self {
			half_size: value.half_size,
			corner_radius: 0.0,
		}
	}
}

impl From<RoundedRectangle> for RoundedPentagonArrow {
	fn from(value: RoundedRectangle) -> Self {
		Self {
			half_size: value.half_size,
			corner_radius: value.corner_radius,
			tip_length: 0.0,
		}
	}
}

impl From<Rectangle> for RoundedPentagonArrow {
	fn from(value: Rectangle) -> Self {
		RoundedRectangle::from(value).into()
	}
}

impl Meshable for RoundedRectangle {
	type Output = RoundedRectangleMeshBuilder;
	fn mesh(&self) -> Self::Output {
		Self::Output::new(*self, 8)
	}
}

impl Meshable for RoundedPentagonArrow {
	type Output = RoundedPentagonArrowMeshBuilder;
	fn mesh(&self) -> Self::Output {
		Self::Output::new(*self, 8)
	}
}

impl RoundedRectangle {
	pub fn corner_radius(mut self, corner_radius: f32) -> Self {
		self.corner_radius = corner_radius;
		self
	}
}

impl RoundedPentagonArrow {
	pub fn corner_radius(mut self, corner_radius: f32) -> Self {
		self.corner_radius = corner_radius;
		self
	}

	pub fn tip_length(mut self, tip_length: f32) -> Self {
		self.tip_length = tip_length;
		self
	}
}

impl RoundedRectangleMeshBuilder {
	pub fn new(primitive: RoundedRectangle, resolution: usize) -> Self {
		Self {
			primitive,
			resolution,
		}
	}

	pub fn resolution(mut self, resolution: usize) -> Self {
		self.resolution = resolution;
		self
	}
}

impl RoundedPentagonArrowMeshBuilder {
	pub fn new(primitive: RoundedPentagonArrow, resolution: usize) -> Self {
		Self {
			primitive,
			resolution,
		}
	}

	pub fn resolution(mut self, resolution: usize) -> Self {
		self.resolution = resolution;
		self
	}
}

impl MeshBuilder for RoundedRectangleMeshBuilder {
	fn build(&self) -> Mesh {
		let radius = self.primitive.corner_radius;
		let half_size = self.primitive.half_size;
		let inner_half_size = half_size - Vec2::splat(radius);

		let resolution = self.resolution as u32;
		// Each corner gets `resolution` outer vertices and one inner (center) vertex
		let vertex_count = 4 * (self.resolution + 1);
		// Each corner gets (`resolution` - 1) polygons, and 6 more are needed for the fill
		let index_count = 3 * (4 * (self.resolution - 1) + 6);

		let mut positions = Vec::with_capacity(vertex_count);
		let mut indices = Vec::with_capacity(index_count);

		let quadrants = [[1.0, 1.0], [-1.0, 1.0], [-1.0, -1.0], [1.0, -1.0]];
		let angle_step = PI / 2.0 / (resolution - 1) as f32;

		for i in 0..4 {
			let [x_sign, y_sign] = quadrants[i as usize];
			let quadrant_start_index = i * (resolution + 1);

			// Inner vertex
			positions.push([x_sign * inner_half_size.x, y_sign * inner_half_size.y, 0.0]);

			// Outer vertices, in counterclockwise order
			for j in 0..resolution {
				let theta = i as f32 * PI / 2.0 + angle_step * j as f32;
				let (sin, cos) = theta.sin_cos();
				let x = x_sign * inner_half_size.x + cos * radius;
				let y = y_sign * inner_half_size.y + sin * radius;
				positions.push([x, y, 0.0]);

				if j > 0 {
					indices.extend_from_slice(&[
						quadrant_start_index,
						quadrant_start_index + j,
						quadrant_start_index + j + 1,
					]);
				}
			}
		}

		// Indices for the non-corner polygons
		// Just... trust me on this one
		indices.extend_from_slice(&[
			4 * (resolution + 1) - 1,
			1,
			2 * (resolution + 1) - 1,
			4 * (resolution + 1) - 1,
			2 * (resolution + 1) - 1,
			2 * (resolution + 1) + 1,
			0,
			resolution,
			resolution + 2,
			0,
			resolution + 2,
			resolution + 1,
			3 * (resolution + 1) + 1,
			3 * (resolution + 1),
			2 * (resolution + 1),
			3 * (resolution + 1) + 1,
			2 * (resolution + 1),
			3 * (resolution + 1) - 1,
		]);

		#[cfg(debug_assertions)]
		{
			assert_eq!(positions.len(), vertex_count);
			assert_eq!(indices.len(), index_count);
		}

		let texture_size = half_size.max_element() * 2.0;
		let uvs = positions
			.iter()
			.map(|&[x, y, _]| [0.5 + x * texture_size, 0.5 - y * texture_size])
			.collect::<Vec<_>>();
		let normals = vec![[0.0, 0.0, 1.0]; vertex_count];

		Mesh::new(PrimitiveTopology::TriangleList, default())
			.with_inserted_attribute(Mesh::ATTRIBUTE_POSITION, positions)
			.with_inserted_attribute(Mesh::ATTRIBUTE_NORMAL, normals)
			.with_inserted_attribute(Mesh::ATTRIBUTE_UV_0, uvs)
			.with_inserted_indices(Indices::U32(indices))
	}
}

impl MeshBuilder for RoundedPentagonArrowMeshBuilder {
	fn build(&self) -> Mesh {
		let radius = self.primitive.corner_radius;
		let half_size = self.primitive.half_size;
		let inner_half_size = half_size - Vec2::splat(radius);
		let tip_length = self.primitive.tip_length;
		let tip_half_angle = (half_size.x - radius).atan2(tip_length);

		let resolution = self.resolution as u32;
		let bend_resolution = (resolution as f32 * tip_half_angle / PI * 2.0)
			.round()
			.max(2.0) as u32;
		let tip_resolution = (resolution as f32 * (PI - tip_half_angle * 2.0) / PI * 2.0)
			.round()
			.max(2.0) as u32;
		// 2 90-degree turns, 2 bend turns, 1 tip turn, plus 1 inner vertex each
		let vertex_count = (2 * resolution + 2 * bend_resolution + tip_resolution + 5) as usize;
		// Same numbers, plus 9 triangles for fill
		let index_count = 3
			* (2 * (resolution - 1) + 2 * (bend_resolution - 1) + tip_resolution - 1 + 9) as usize;

		let mut positions = Vec::with_capacity(vertex_count);
		let mut indices = Vec::with_capacity(index_count);

		let corners = [
			// Square (back) corners
			(
				-inner_half_size.x,
				-inner_half_size.y,
				PI,
				PI * 1.5,
				resolution,
			),
			(
				inner_half_size.x,
				-inner_half_size.y,
				PI * 1.5,
				TAU,
				resolution,
			),
			// Bend (front) corners
			(
				inner_half_size.x,
				inner_half_size.y,
				0.0,
				tip_half_angle,
				bend_resolution,
			),
			(
				-inner_half_size.x,
				inner_half_size.y,
				PI - tip_half_angle,
				PI,
				bend_resolution,
			),
			// Tip corner
			(
				0.0,
				inner_half_size.y + tip_length,
				tip_half_angle,
				PI - tip_half_angle,
				tip_resolution,
			),
		];
		for (x, y, start_angle, end_angle, resolution) in corners {
			let quadrant_start_index = positions.len() as u32;

			// Inner vertex
			positions.push([x, y, 0.0]);

			for j in 0..resolution {
				let theta = start_angle.lerp(end_angle, j as f32 / (resolution - 1) as f32);
				let (sin, cos) = theta.sin_cos();
				let x = x + cos * radius;
				let y = y + sin * radius;
				positions.push([x, y, 0.0]);

				if j > 0 {
					indices.extend_from_slice(&[
						quadrant_start_index,
						quadrant_start_index + j,
						quadrant_start_index + j + 1,
					]);
				}
			}
		}

		// Indices for the non-corner polygons
		// Just... trust me on this one
		indices.extend_from_slice(&[
			1,
			2 * (resolution + 1) - 1,
			2 * (resolution + bend_resolution + 2) - 1,
			2 * (resolution + 1) - 1,
			2 * (resolution + 1) + 1,
			2 * (resolution + bend_resolution + 2) - 1,
			0,
			resolution,
			resolution + 2,
			0,
			resolution + 2,
			resolution + 1,
			2 * (resolution + 1),
			2 * (resolution + bend_resolution + 2) + tip_resolution,
			2 * (resolution + 1) + bend_resolution + 1,
			2 * (resolution + 1),
			2 * (resolution + bend_resolution + 2),
			2 * (resolution + bend_resolution + 2) + tip_resolution,
			2 * (resolution + 1) + bend_resolution + 1,
			2 * (resolution + bend_resolution + 2) + tip_resolution,
			2 * (resolution + 1) + bend_resolution + 2,
			2 * (resolution + 1),
			2 * (resolution + bend_resolution + 2) + 1,
			2 * (resolution + bend_resolution + 2),
			2 * (resolution + 1),
			2 * (resolution + 1) + bend_resolution,
			2 * (resolution + bend_resolution + 2) + 1,
		]);

		#[cfg(debug_assertions)]
		{
			assert_eq!(positions.len(), vertex_count);
			assert_eq!(indices.len(), index_count);
		}

		let texture_size = (half_size.x * 2.0).max(half_size.y * 2.0 + tip_length);
		let texture_y_offset = (half_size.y + tip_length) / (half_size.y * 2.0 + tip_length);
		let uvs = positions
			.iter()
			.map(|&[x, y, _]| [0.5 + x * texture_size, texture_y_offset - y * texture_size])
			.collect::<Vec<_>>();
		let normals = vec![[0.0, 0.0, 1.0]; vertex_count];

		Mesh::new(PrimitiveTopology::TriangleList, default())
			.with_inserted_attribute(Mesh::ATTRIBUTE_POSITION, positions)
			.with_inserted_attribute(Mesh::ATTRIBUTE_NORMAL, normals)
			.with_inserted_attribute(Mesh::ATTRIBUTE_UV_0, uvs)
			.with_inserted_indices(Indices::U32(indices))
	}
}
