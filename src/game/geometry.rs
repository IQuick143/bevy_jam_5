//! Geometry calculations for level definitions

use super::level::CycleData;
use crate::graphics::EDGE_DETECTOR_MARGIN;

impl CycleData {
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
			first_vertex + EDGE_DETECTOR_MARGIN / self.placement.length()
		} else if !is_closed && after_vertex_id == self.vertex_indices.len() {
			// After end of open cycle
			let last_vertex = self
				.vertex_positions
				.last()
				.expect("We just checked it is not empty");
			last_vertex - EDGE_DETECTOR_MARGIN / self.placement.length()
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
}
