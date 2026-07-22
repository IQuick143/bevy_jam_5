//! Geometry calculations for level definitions

use super::level::{CycleData, CycleShape, LevelData, VertexData};
use crate::graphics::EDGE_DETECTOR_MARGIN;
use bevy::prelude::*;
use itertools::Itertools as _;
use std::f32::consts::TAU;

/// Proxy over a [`CycleData`] that also holds vertices and allows
/// calculating geometry over irregular shapes
#[derive(Clone, Copy, Deref)]
pub struct CycleView<'a> {
	vertices: &'a [VertexData],
	#[deref]
	cycle: &'a CycleData,
}

impl LevelData {
	pub fn cycle_view(&self, cycle_id: usize) -> Option<CycleView<'_>> {
		Some(CycleView {
			vertices: &self.vertices,
			cycle: self.cycles.get(cycle_id)?,
		})
	}
}

impl CycleData {
	/// Gets the index within [`CycleData::vertex_indices`] of the vertex
	/// that comes after a given offset [0, 1] on the cycle
	fn get_vertex_index_after(&self, t: f32) -> Option<usize> {
		self.vertex_positions
			.iter()
			.copied()
			.enumerate()
			.filter(|(_, p)| *p > t)
			.reduce(
				|(i1, p1), (i2, p2)| {
					if p1 < p2 { (i1, p1) } else { (i2, p2) }
				},
			)
			.map(|(i, _)| i)
	}
}

impl CycleView<'_> {
	/// Sample the cycle at an offset [0, 1],
	/// corrected for margins on an open cycle
	pub fn sample(&self, t: f32) -> Vec2 {
		if self.cycle.placement.is_closed() {
			self.closed_sample(t)
		} else {
			self.closed_sample(CycleShape::offset_open_to_flat(t))
		}
	}

	/// Sample the cycle at an offset [0, 1], anything out of range
	/// will go to the margins on open cycles
	pub fn closed_sample(&self, t: f32) -> Vec2 {
		match self.placement {
			CycleShape::Circle { center, radius } => center + radius * Vec2::from_angle(t * TAU),
			CycleShape::Line { start, end } => end.lerp(start, t),
			CycleShape::ClosedFreeform => {
				if self.vertex_positions.is_empty() {
					return Vec2::ZERO;
				}
				let ((t_before, p_before), (t_after, p_after)) =
					self.lookup_closed_freeform_segment(t);
				let t_adjusted = (t - t_after).rem_euclid(1.0) / (t_before - t_after);
				p_after.lerp(p_before, t_adjusted)
			}
			CycleShape::OpenFreeform { start, end } => {
				let ((t_before, p_before), (t_after, p_after)) =
					self.lookup_open_freeform_segment(start, end, t);
				let t_adjusted = (t - t_after) / (t_before - t_after);
				p_after.lerp(p_before, t_adjusted)
			}
		}
	}

	/// Length of the cycle's perimeter, in world units,
	/// corrected for margins on an open cycle
	pub fn length(&self) -> f32 {
		if self.placement.is_closed() {
			self.closed_length()
		} else {
			self.closed_length() * 2.0
		}
	}

	/// Length of the cycle's perimeter, in world units
	pub fn closed_length(&self) -> f32 {
		match self.placement {
			CycleShape::Circle { radius, .. } => TAU * radius,
			CycleShape::Line { start, end } => start.distance(end),
			CycleShape::ClosedFreeform => self
				.vertex_indices
				.iter()
				.copied()
				.map(|i| self.vertices[i].position)
				.circular_tuple_windows()
				.map(|(a, b)| a.distance(b))
				.sum(),
			CycleShape::OpenFreeform { start, end } => std::iter::once(start)
				.chain(
					self.vertex_indices
						.iter()
						.copied()
						.map(|i| self.vertices[i].position),
				)
				.chain(std::iter::once(end))
				.tuple_windows()
				.map(|(a, b)| a.distance(b))
				.sum(),
		}
	}

	/// Get the normal to the cycle's perimeter at a given offset
	pub fn normal(&self, t: f32) -> Dir2 {
		match self.placement {
			CycleShape::Circle { .. } => {
				Dir2::new(Vec2::from_angle(t * TAU)).unwrap_or(Dir2::NEG_X)
			}
			CycleShape::Line { start, end } => {
				Dir2::new((end - start).perp()).unwrap_or(Dir2::NEG_X)
			}
			CycleShape::ClosedFreeform => {
				// Empty freeform cycle should be invalid, but check, just in case
				if self.vertex_positions.is_empty() {
					return Dir2::NEG_X;
				}
				let ((_, p_before), (_, p_after)) = self.lookup_closed_freeform_segment(t);
				Dir2::new((p_after - p_before).perp()).unwrap_or(Dir2::NEG_X)
			}
			CycleShape::OpenFreeform { start, end } => {
				let ((_, p_before), (_, p_after)) = self.lookup_open_freeform_segment(
					start,
					end,
					CycleShape::offset_open_to_flat(t),
				);
				Dir2::new((p_after - p_before).perp()).unwrap_or(Dir2::NEG_X)
			}
		}
	}

	/// Relative position of a detector located at a given index, [0, 1]
	pub fn detector_position_at(&self, after_vertex_id: usize) -> f32 {
		if self.vertex_indices.is_empty() {
			// Should not happen (level description like that is invalid),
			// but just in case
			return 0.0;
		}

		let is_closed = self.placement.is_closed();
		if !is_closed && after_vertex_id == 0 {
			// Before start of open cycle
			let first_vertex = self
				.vertex_positions
				.first()
				.expect("We just checked it is not empty");
			let flat_pos = CycleShape::offset_open_to_flat(*first_vertex)
				+ EDGE_DETECTOR_MARGIN / self.closed_length();
			CycleShape::offset_flat_to_open(flat_pos)
		} else if !is_closed && after_vertex_id == self.vertex_indices.len() {
			// After end of open cycle
			let last_vertex = self
				.vertex_positions
				.last()
				.expect("We just checked it is not empty");
			let flat_pos = CycleShape::offset_open_to_flat(*last_vertex)
				- EDGE_DETECTOR_MARGIN / self.closed_length();
			CycleShape::offset_flat_to_open(flat_pos)
		} else {
			// If the detector is not at the end of an open cycle,
			// interpolate between the two vertices
			self.default_detector_position_at(after_vertex_id)
		}
	}

	/// The default relative position of a detector located at a given index, [0, 1]
	fn default_detector_position_at(&self, after_vertex_id: usize) -> f32 {
		let n_vertices = self.vertex_positions.len();
		let index_before = (after_vertex_id + n_vertices - 1) % n_vertices;
		let index_after = after_vertex_id % n_vertices;

		let detector_position = if index_before == index_after {
			let prev_vertex_position = self.vertex_positions[index_before];
			prev_vertex_position + 0.5
		} else {
			// Get the path-based [0, 1] position of both neighboring vertices
			let prev_vertex_position = self.vertex_positions[index_before];
			let mut next_vertex_position = self.vertex_positions[index_after];
			if prev_vertex_position < next_vertex_position {
				// Wrap the position around the zeroth vertex
				next_vertex_position += 1.0;
			}
			// Draw the detector halfway between the vertices
			(prev_vertex_position + next_vertex_position) / 2.0
		};

		detector_position % 1.0
	}

	/// Samples the cycle as a closed freeform to locate the segment at a given offset.
	///
	/// Parameter `t` is offset along the cycle.
	///
	/// Returns positions and offsets of the two vertices surrounding `t`.
	/// The offset of the first one may be greater than one,
	/// and is always greater than the offset of the second one.
	fn lookup_closed_freeform_segment(&self, t: f32) -> ((f32, Vec2), (f32, Vec2)) {
		let vertex_before = self
			.get_vertex_index_after(t.rem_euclid(1.0))
			.unwrap_or(self.vertex_positions.len() - 1);
		let vertex_after = (vertex_before + 1) % self.vertex_positions.len();
		let p_before = self.vertices[self.vertex_indices[vertex_before]].position;
		let p_after = self.vertices[self.vertex_indices[vertex_after]].position;
		let t_before = self.vertex_positions[vertex_before];
		let t_after = self.vertex_positions[vertex_after];
		// Normalize the offsets in case they wrap
		let t_before = if t_before < t_after || vertex_before == vertex_after {
			t_before + 1.0
		} else {
			t_before
		};
		((t_before, p_before), (t_after, p_after))
	}

	/// Samples the cycle as an open freeform to locate the segment at a given offset.
	///
	/// Parameter `t` is flat offset along the cycle.
	///
	/// Returns positions and flat offsets of the two vertices surrounding `t`,
	/// or the provided end points if there is no vertex to either side of `t`.
	fn lookup_open_freeform_segment(
		&self,
		start: Vec2,
		end: Vec2,
		t: f32,
	) -> ((f32, Vec2), (f32, Vec2)) {
		let vertex_before = self.get_vertex_index_after(CycleShape::offset_flat_to_open(t));
		let p_before;
		let p_after;
		let t_before;
		let t_after;
		if let Some(vertex_before) = vertex_before {
			p_before = self.vertices[self.vertex_indices[vertex_before]].position;
			t_before = CycleShape::offset_open_to_flat(self.vertex_positions[vertex_before]);
		} else {
			p_before = start;
			t_before = 1.0;
		}
		if vertex_before == Some(self.vertex_indices.len() - 1) {
			p_after = end;
			t_after = 0.0;
		} else {
			let vertex_after = vertex_before.map_or(0, |i| i + 1);
			p_after = self.vertices[self.vertex_indices[vertex_after]].position;
			t_after = CycleShape::offset_open_to_flat(self.vertex_positions[vertex_after]);
		}
		((t_before, p_before), (t_after, p_after))
	}
}
