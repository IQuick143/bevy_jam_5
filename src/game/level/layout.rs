use std::f32::consts::PI;

use super::*;

/// Declarative specification of the placement of a lone cycle
#[derive(Clone, Debug)]
pub struct DeclaredCyclePlacement {
	/// Index of the cycle being laid out
	pub cycle_index: usize,
	/// Position of the center point of the cycle
	pub position: Vec2,
	/// Radius of the cycle
	pub radius: f32,
	/// Indices of vertices that should prefer their second
	/// option for placement instead of the first
	pub double_intersection_hints: Vec<usize>,
}

/// Declarative specification of the placement of a vertex
/// that does not lie in the intersection of multiple cycles
#[derive(Clone, Copy, Debug)]
pub struct DeclaredVertexPlacement {
	/// Index of the vertex being laid out
	pub vertex_index: usize,
	/// Angle of the vector from the centerpoint of the cycle that
	/// owns the vertex to the vertex
	pub relative_angle: f32,
}

/// Declarative specification of the placement of a game object
#[derive(Clone, Debug)]
pub enum DeclaredPlacement {
	/// Place a cycle that does not intersects any already-placed cycles
	Cycle(DeclaredCyclePlacement),
	/// Place a vertex that belongs to exactly one already-placed cycle
	/// and does not yet have a fixed position
	Vertex(DeclaredVertexPlacement),
}

/// Computed placement of a cycle
#[derive(Clone, Copy, PartialEq, Debug)]
pub struct CyclePlacement {
	/// Position of the center point of the cycle
	pub position: Vec2,
	/// Radius of the cycle
	pub radius: f32,
}

/// Describes the layout of all objects in a level
#[derive(Debug)]
pub struct LevelLayout {
	/// Positions of vertices in the level
	pub vertices: Vec<Vec2>,
	/// Placements of cycles in the level
	pub cycles: Vec<CyclePlacement>,
}

/// Helper object for constructing a [`LevelLayout`]
#[derive(Debug)]
pub struct LevelLayoutBuilder<'w> {
	level: &'w ValidLevelData,
	/// Placements of vertices, if they have been placed yet
	vertices: Vec<IntermediateVertexPosition>,
	/// Placements of cycles, if they have been placed yet
	cycles: Vec<Option<CyclePlacement>>,
}

/// Error data for [`LevelLayoutError::CycleDoesNotContainVertex`]
#[derive(Clone, Copy, Debug)]
pub struct CycleDoesNotContainVertexError {
	/// Index of the cycle whose being attempted placement failed
	placed_cycle: usize,
	/// Placement that was requested for the cycle
	requested_placement: CyclePlacement,
	/// Index of the vertex with fixed position
	/// that would not lie on the cycle as placed
	failing_vertex: usize,
	/// Position of the failing vertex
	vertex_position: Vec2,
}

/// Error data for [`LevelLayoutError::CyclesDoNotIntersect`]
#[derive(Clone, Copy, Debug)]
pub struct CyclesDoNotIntersectError {
	/// Index of the cycle whose being attempted placement failed
	placed_cycle: usize,
	/// Placement that was requested for the cycle
	requested_placement: CyclePlacement,
	/// Index of the already-placed cycle that shared a vertex
	/// with the one being placed
	existing_cycle: usize,
	/// Placement of the already-placed cycle
	existing_placement: CyclePlacement,
	/// Index of the vertex that the cycles share
	/// that could not be placed because the cycles do not intersect
	failing_vertex: usize,
}

/// Error data for [`LevelLayoutError::CyclesDoNotIntersectTwice`]
#[derive(Clone, Copy, Debug)]
pub struct CyclesDoNotIntersectTwiceError {
	/// Index of the cycle whose being attempted placement failed
	placed_cycle: usize,
	/// Placement that was requested for the cycle
	requested_placement: CyclePlacement,
	/// Index of the already-placed cycle that shared
	/// two vertices with the one being placed
	existing_cycle: usize,
	/// Placement of the already-placed cycle
	existing_placement: CyclePlacement,
	/// Index of the vertex that has already been placed at the only
	/// intersection between the cycles
	existing_vertex: usize,
	/// Position of the intersection (and the already-placed vertex)
	vertex_position: Vec2,
	/// Index of the vertex that the cycles share
	/// that could not be placed because the cycles only intersect once
	failing_vertex: usize,
}

#[derive(Clone, Copy, Debug)]
pub enum LevelLayoutError {
	/// An out-of-range index was used to reference a cycle
	CycleIndexOutOfRange(usize),
	/// An out-of-range index was used to reference a vertex
	VertexIndexOutOfRange(usize),
	/// [`build`](LevelLayoutBuilder::build) was called
	/// while a cycle had not been placed yet
	UnplacedCycle(usize),
	/// [`add_placement`](LevelLayoutBuilder::add_placement) was called
	/// on a cycle that had already been placed
	CycleAlreadyPlaced(usize),
	/// [`add_placement`](LevelLayoutBuilder::add_placement) was called
	/// on a cycle that contains a vertex that has already been definitively placed,
	/// and it would not lie on the cycle as being placed
	CycleDoesNotContainVertex(CycleDoesNotContainVertexError),
	/// Cycles that share a vertex have been placed in a way that they do not intersect
	CyclesDoNotIntersect(CyclesDoNotIntersectError),
	/// Cycles that share two vertices have been placed in a way that they
	/// only intersect tangentially (only enough space for one shared vertex)
	CyclesDoNotIntersectTwice(CyclesDoNotIntersectTwiceError),
	/// [`place_partial_vertex`](LevelLayoutBuilder::place_partial_vertex) was called
	/// on a vertex that already has a fixed placement
	VertexAlreadyPlaced(usize),
	/// [`place_partial_vertex`](LevelLayoutBuilder::place_partial_vertex) was called
	/// on a vertex that already has not yet been partially placed
	VertexNotPartiallyPlaced(usize),
	/// [`add_placement`](LevelLayoutBuilder::add_placement) was called
	/// in a way that would place vertices around a cycle out of their rotation order
	VertexOrderViolationOnCycle(usize),
}

/// Placement of a vertex while the layout is being built
#[derive(Clone, Copy, PartialEq, Debug)]
enum IntermediateVertexPosition {
	/// The vertex has not been placed yet
	Free,
	/// The vertex has a fixed position
	Fixed(Vec2),
	/// Exactly one cycle that owns the vertex has been placed
	/// and the vertex may still be moved along it
	Partial(PartiallyBoundVertexPosition),
}

/// Placement of a vertex that belongs to a placed cycle,
/// but does not yet have a definite placement
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
struct PartiallyBoundVertexPosition {
	/// Index of the cycle that owns the vertex
	owner_cycle: usize,
	/// Index of the vertex within the owner cycle's vertex list
	index_in_owner: usize,
}

/// Container that holds one or two of something
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum OneTwo<T> {
	One(T),
	Two(T, T),
}
use OneTwo::*;

impl IntermediateVertexPosition {
	fn get_fixed(&self) -> Option<Vec2> {
		match self {
			Self::Fixed(pos) => Some(*pos),
			_ => None,
		}
	}
}

impl<T> OneTwo<T> {
	fn add_to_one(self, x: T) -> Result<Self, (T, T, T)> {
		match self {
			One(a) => Ok(Two(a, x)),
			Two(a, b) => Err((a, b, x)),
		}
	}

	fn first(self) -> T {
		match self {
			One(x) | Two(x, _) => x,
		}
	}

	fn last(self) -> T {
		match self {
			One(x) | Two(_, x) => x,
		}
	}
}

impl<T> IntoIterator for OneTwo<T> {
	type Item = T;
	type IntoIter = <Vec<T> as IntoIterator>::IntoIter;
	fn into_iter(self) -> Self::IntoIter {
		match self {
			One(a) => vec![a].into_iter(),
			Two(a, b) => vec![a, b].into_iter(),
		}
	}
}

impl<'w> LevelLayoutBuilder<'w> {
	pub fn new(level: &'w ValidLevelData) -> Self {
		Self {
			level,
			vertices: vec![IntermediateVertexPosition::Free; level.vertices.len()],
			cycles: vec![None; level.cycles.len()],
		}
	}

	pub fn add_placement(&mut self, placement: DeclaredPlacement) -> Result<(), LevelLayoutError> {
		match placement {
			DeclaredPlacement::Cycle(p) => self.place_cycle(
				p.cycle_index,
				p.position,
				p.radius,
				&p.double_intersection_hints,
			),
			DeclaredPlacement::Vertex(p) => self.place_vertex(p.vertex_index, p.relative_angle),
		}
	}

	pub fn place_cycle(
		&mut self,
		target_cycle: usize,
		center: Vec2,
		radius: f32,
		double_intersection_hints: &[usize],
	) -> Result<(), LevelLayoutError> {
		if target_cycle >= self.cycles.len() {
			return Err(LevelLayoutError::CycleIndexOutOfRange(target_cycle));
		}
		if self.cycles[target_cycle].is_some() {
			return Err(LevelLayoutError::CycleAlreadyPlaced(target_cycle));
		}
		let placement = CyclePlacement {
			position: center,
			radius,
		};
		// This will be filled with placements of all vertices after the cycle is placed
		let mut placements_after = Vec::new();
		// This will be filled with vertices that are already partially placed
		let mut new_fixed_points = std::collections::BTreeMap::new();
		let vertex_indices = &self.level.cycles[target_cycle].vertex_indices;
		for (j, &i) in vertex_indices.iter().enumerate() {
			match self.vertices[i] {
				IntermediateVertexPosition::Fixed(pos) => {
					// Fixed vertex cannot be moved, we can only proceed if
					// it already lies on the cycle being placed
					if !approx_eq(center.distance_squared(pos), radius.powi(2)) {
						return Err(LevelLayoutError::CycleDoesNotContainVertex(
							CycleDoesNotContainVertexError {
								placed_cycle: target_cycle,
								requested_placement: placement,
								failing_vertex: i,
								vertex_position: pos,
							},
						));
					}
					placements_after.push(IntermediateVertexPosition::Fixed(pos));
				}
				IntermediateVertexPosition::Partial(p) => {
					// Partially placed vertex will be fixed at an intersection of the two cycles
					let owner_placement = self.cycles[p.owner_cycle]
						.expect("Owner cycle of a partially placed vertex should also be placed");
					// Find one intersection of the two cycles, if it exists
					let intersection = intersect_circles(
						owner_placement.position,
						center,
						owner_placement.radius,
						radius,
					);
					match intersection {
						Some(One(new_pos)) => {
							// Cycles intersect tangentially, there is one intersection
							let replaced =
								new_fixed_points.insert(p.owner_cycle, One((i, new_pos)));
							// Fail if there is already a vertex there
							if let Some(existing_vertices) = replaced {
								return Err(LevelLayoutError::CyclesDoNotIntersectTwice(
									CyclesDoNotIntersectTwiceError {
										placed_cycle: target_cycle,
										requested_placement: placement,
										existing_cycle: p.owner_cycle,
										existing_placement: owner_placement,
										existing_vertex: existing_vertices.first().0,
										vertex_position: new_pos,
										failing_vertex: i,
									},
								));
							}
							placements_after.push(IntermediateVertexPosition::Fixed(new_pos));
						}
						Some(Two(default_pos, alt_pos)) => {
							// If there are two intersections, we must choose one of them
							let new_pos = new_fixed_points.entry(p.owner_cycle)
								.and_modify(|val| {
									let new_pos = if double_intersection_hints.contains(&val.first().0) {
										default_pos
									}
									else {
										alt_pos
									};
									*val = val.add_to_one((i, new_pos))
										.expect("More than two vertices in intersection of two cycles, should have been caught by sanitizer")
								})
								.or_insert_with(|| {
									let new_pos = if double_intersection_hints.contains(&i) {
										alt_pos
									}
									else {
										default_pos
									};
									One((i, new_pos))
								})
								.last().1;
							placements_after.push(IntermediateVertexPosition::Fixed(new_pos));
						}
						None => {
							// Fail if the cycles do not intersect
							return Err(LevelLayoutError::CyclesDoNotIntersect(
								CyclesDoNotIntersectError {
									placed_cycle: target_cycle,
									requested_placement: placement,
									existing_cycle: p.owner_cycle,
									existing_placement: owner_placement,
									failing_vertex: i,
								},
							));
						}
					}
				}
				IntermediateVertexPosition::Free => {
					// Free vertex can be partially placed without further complication
					placements_after.push(IntermediateVertexPosition::Partial(
						PartiallyBoundVertexPosition {
							owner_cycle: target_cycle,
							index_in_owner: j,
						},
					));
				}
			}
		}
		// All fixed points on the cycle must be in cycle order
		let fixed_points_after = placements_after.iter().filter_map(|p| p.get_fixed());
		if !Self::are_points_in_cyclic_order(center, fixed_points_after) {
			return Err(LevelLayoutError::VertexOrderViolationOnCycle(target_cycle));
		}
		// Newly fixed points on other cycles must also work with those cycles
		for (cycle_index, placement) in new_fixed_points {
			if !self.verify_materialization_against_cycle(cycle_index, placement.into_iter()) {
				return Err(LevelLayoutError::VertexOrderViolationOnCycle(cycle_index));
			}
		}
		// We are done verifying that the placement is valid, now we can commit to it
		for (&i, new_pos) in vertex_indices.iter().zip_eq(placements_after) {
			self.vertices[i] = new_pos;
		}
		self.cycles[target_cycle] = Some(placement);
		Ok(())
	}

	pub fn place_vertex(
		&mut self,
		target_vertex: usize,
		relative_angle: f32,
	) -> Result<(), LevelLayoutError> {
		if target_vertex >= self.vertices.len() {
			return Err(LevelLayoutError::VertexIndexOutOfRange(target_vertex));
		}
		match self.vertices[target_vertex] {
			IntermediateVertexPosition::Free => {
				Err(LevelLayoutError::VertexNotPartiallyPlaced(target_vertex))
			}
			IntermediateVertexPosition::Fixed(_) => {
				Err(LevelLayoutError::VertexAlreadyPlaced(target_vertex))
			}
			IntermediateVertexPosition::Partial(p) => {
				let owner_placement = self.cycles[p.owner_cycle]
					.expect("Owner cycle of a partially placed vertex should also be placed");
				let new_pos = owner_placement.position
					+ owner_placement.radius * Vec2::from_angle(relative_angle);
				if !self.verify_materialization_against_cycle(
					p.owner_cycle,
					std::iter::once((target_vertex, new_pos)),
				) {
					return Err(LevelLayoutError::VertexOrderViolationOnCycle(p.owner_cycle));
				}
				self.vertices[target_vertex] = IntermediateVertexPosition::Fixed(new_pos);
				Ok(())
			}
		}
	}

	/// Checks that the level layout is complete and assembles it
	pub fn build(mut self) -> Result<LevelLayout, LevelLayoutError> {
		let cycles = self
			.cycles
			.iter()
			.enumerate()
			.map(|(i, placement)| placement.ok_or(LevelLayoutError::UnplacedCycle(i)))
			.collect::<Result<_, _>>()?;
		self.materialize_partial_vertex_placements();
		let vertices = self
			.vertices
			.into_iter()
			.map(|placement| match placement {
				IntermediateVertexPosition::Fixed(pos) => pos,
				// This is technically possible, since a vertex can belong to no cycle
				IntermediateVertexPosition::Free => Vec2::ZERO,
				// Prevented by [`materialize_all_partial_vertex_placements`]
				IntermediateVertexPosition::Partial(_) => {
					panic!("Partially placed vertex in build phase, should have been materialized")
				}
			})
			.collect();
		Ok(LevelLayout { vertices, cycles })
	}

	/// Iterates through all cycles and materializes all vertices
	/// that are currently in [`Partial`](IntermediateVertexPosition::Partial) placement,
	/// turning them into [`Fixed`](IntermediateVertexPosition::Fixed).
	/// Vertices that are not a part of a placed cycle remain in
	/// [`Free`](IntermediateVertexPosition::Free) placement.
	fn materialize_partial_vertex_placements(&mut self) {
		// Materialize all vertices, one placed cycle at a time
		let cycles = self
			.cycles
			.iter()
			.enumerate()
			.zip_eq(&self.level.cycles)
			.filter_map(|((i, placement), data)| {
				placement.as_ref().map(|placement| (i, placement, data))
			});
		for (cycle_index, cycle_placement, cycle_data) in cycles {
			// Vertices that already have fixed placement split the cycle
			// into segments. Each segment will be handled separately
			let mut fixed_vertices = cycle_data
				.vertex_indices
				.iter()
				.enumerate()
				.filter_map(|(i, &j)| self.vertices[j].get_fixed().map(|pos| (i, pos)))
				// Collect and cast back into iterator so we can modify [`self.vertices`]
				.collect::<Vec<_>>()
				.into_iter();
			if let Some((first_fixed_vertex, first_fixed_pos)) = fixed_vertices.next() {
				let first_relative_pos = first_fixed_pos - cycle_placement.position;
				let mut current_fixed_vertex = first_fixed_vertex;
				let mut current_relative_pos = first_relative_pos;
				while let Some((next_fixed_vertex, next_fixed_pos)) = fixed_vertices.next() {
					// Materialize all vertices between the marked ones
					let next_relative_pos = next_fixed_pos - cycle_placement.position;
					let vertex_count = next_fixed_vertex - current_fixed_vertex;
					let mut segment_angle = current_relative_pos.angle_between(next_relative_pos);
					if segment_angle <= 0.0 {
						segment_angle += 2.0 * PI;
					}
					// Distribute the partially-placed vertices uniformly between the fixed ones
					for (j, i) in ((current_fixed_vertex + 1)..next_fixed_vertex).enumerate() {
						let target_vertex = cycle_data.vertex_indices[i];
						let new_pos = cycle_placement.position
							+ current_relative_pos.rotate(Vec2::from_angle(
								segment_angle * (j + 1) as f32 / vertex_count as f32,
							));
						Self::checked_materialize(
							&mut self.vertices[target_vertex],
							new_pos,
							cycle_index,
							i,
						);
					}
					// Move to the next segment
					current_fixed_vertex = next_fixed_vertex;
					current_relative_pos = next_relative_pos;
				}
				// Close the loop; materialize the segment between last and first fixed vertex
				// Mind the special case when exactly one vertex is fixed
				// and this segment covers the whole cycle
				let vertex_count =
					first_fixed_vertex + cycle_data.vertex_indices.len() - current_fixed_vertex;
				let mut segment_angle = current_relative_pos.angle_between(first_relative_pos);
				if first_fixed_vertex == current_fixed_vertex {
					// I do not trust floats, so set this value explicitly
					segment_angle = 2.0 * PI;
				} else if segment_angle <= 0.0 {
					segment_angle += 2.0 * PI;
				}
				// Distribute the partially-placed vertices uniformly between the fixed ones
				for (j, i) in ((current_fixed_vertex + 1)..cycle_data.vertex_indices.len())
					.chain(0..first_fixed_vertex)
					.enumerate()
				{
					let target_vertex = cycle_data.vertex_indices[i];
					let new_pos = cycle_placement.position
						+ current_relative_pos.rotate(Vec2::from_angle(
							segment_angle * (j + 1) as f32 / vertex_count as f32,
						));
					Self::checked_materialize(
						&mut self.vertices[target_vertex],
						new_pos,
						cycle_index,
						i,
					);
				}
			} else {
				// If none of the vertices have fixed placement, distribute them uniformly
				let vertex_count = cycle_data.vertex_indices.len();
				for (i, &j) in cycle_data.vertex_indices.iter().enumerate() {
					let new_pos = cycle_placement.position
						+ cycle_placement.radius
							* Vec2::from_angle(2.0 * PI * i as f32 / vertex_count as f32);
					Self::checked_materialize(&mut self.vertices[j], new_pos, cycle_index, i);
				}
			}
		}
	}

	/// Sets a vertex to a fixed placement and checks that it belonged to a particular cycle
	/// ## Panics
	/// Panics if the vertex was not previously in [`Partial`](IntermediateVertexPosition::Partial)
	/// placement owned by the specified cycle
	fn checked_materialize(
		pos: &mut IntermediateVertexPosition,
		new_pos: Vec2,
		owner_cycle: usize,
		index_in_owner: usize,
	) {
		assert_eq!(
			*pos,
			IntermediateVertexPosition::Partial(PartiallyBoundVertexPosition {
				owner_cycle,
				index_in_owner
			}),
			"Vertices that belong to a placed cycle must be partially placed and owned by that cycle"
		);
		*pos = IntermediateVertexPosition::Fixed(new_pos);
	}

	/// Checks whether a materialization can be done or if it breaks
	/// cycle order of a particular cycle
	/// ## Notes
	/// - The cycle to test against must already be placed
	/// - Materialized points are intentionally passed as an iterator.
	/// The list is searched linearly for every vertex of the cycle,
	/// as the list is expected to be very small.
	/// Use a fast implementation of iterator
	/// - Only the cycle order is verified, radius of the cycle is not checked
	fn verify_materialization_against_cycle(
		&self,
		cycle_index: usize,
		points_to_materialize: impl Iterator<Item = (usize, Vec2)> + Clone,
	) -> bool {
		let cycle_center = self.cycles[cycle_index]
			.expect("Partial materialization checks can only be run on placed cycles")
			.position;
		let fixed_points = self.level.cycles[cycle_index]
			.vertex_indices
			.iter()
			.filter_map(|&i| {
				points_to_materialize
					.clone()
					.find_map(|(j, p)| if i == j { Some(p) } else { None })
					.or_else(|| self.vertices[i].get_fixed())
			});
		Self::are_points_in_cyclic_order(cycle_center, fixed_points)
	}

	/// Checks whether a sequence of points is ordered in order of
	/// counterclockwise navigation around a center point
	fn are_points_in_cyclic_order(center: Vec2, points: impl Iterator<Item = Vec2>) -> bool {
		let mut points = points.map(|p| p - center);
		let mut total_angle = 0.0;
		if let Some(mut current) = points.next() {
			while let Some(next) = points.next() {
				let mut angle = current.angle_between(next);
				if angle < 0.0 {
					// Recalculate angles to [0, 2 * pi]
					// so that we stay in counterclockwise movement
					angle += 2.0 * PI;
				}
				total_angle += angle;
				current = next;
			}
			total_angle < 2.0 * PI
		} else {
			true
		}
	}
}

impl std::fmt::Display for CyclePlacement {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(
			f,
			"[x={} y={} r={}]",
			self.position.x, self.position.y, self.radius
		)
	}
}

impl std::error::Error for LevelLayoutError {}

impl std::fmt::Display for LevelLayoutError {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::VertexIndexOutOfRange(i) => write!(f, "Cannot place vertex {i} because there are not that many vertices."),
			Self::CycleIndexOutOfRange(i) => write!(f, "Cannot place cycle {i} because there are not that many cycles."),
			Self::UnplacedCycle(i) => write!(f, "Cannot finish layout because cycle {i} has not yet been placed."),
			Self::CycleAlreadyPlaced(i) => write!(f, "Cannot place cycle {i} because it has already been placed."),
			Self::VertexAlreadyPlaced(i) => write!(f, "Cannot place vertex {i} because it has already been (possibly implicitly) placed."),
			Self::VertexNotPartiallyPlaced(i) => write!(f, "Cannot place vertex {i} because it does not lie on any placed cycle."),
			Self::CycleDoesNotContainVertex(e) => write!(
				f,
				"Cycle {} cannot be placed at {} because it contains vertex {} which has already been placed at {}.",
				e.placed_cycle,
				e.requested_placement,
				e.failing_vertex,
				e.vertex_position
			),
			Self::CyclesDoNotIntersect(e) => write!(
				f,
				"Cycle {} cannot be placed at {} because it shares vertex {} with cycle {} at {} and the cycles would not intersect.",
				e.placed_cycle,
				e.requested_placement,
				e.failing_vertex,
				e.existing_cycle,
				e.existing_placement
			),
			Self::CyclesDoNotIntersectTwice(e) => write!(
				f,
				"Cycle {} cannot be placed at {} because it shares vertices {} and {} with cycle {} at {} and the cycles would only intersect once at {}.",
				e.placed_cycle,
				e.requested_placement,
				e.existing_vertex,
				e.failing_vertex,
				e.existing_cycle,
				e.existing_placement,
				e.vertex_position
			),
			Self::VertexOrderViolationOnCycle(i) => write!(f, "Placement is not valid because vertices around cycle {i} would be out of order.")
		}
	}
}

fn intersect_circles(c1: Vec2, c2: Vec2, r1: f32, r2: f32) -> Option<OneTwo<Vec2>> {
	let d_sq = c1.distance_squared(c2);
	// Do not even try if the circles share a center point
	const CENTER_OVERLAP_THRESHOLD: f32 = 0.001;
	if d_sq < CENTER_OVERLAP_THRESHOLD {
		return None;
	}
	// First we find the projection of the intersections onto the line that connects center points
	// This is the distance of that point along that line, as a multiple of distance between the centers
	let rel_midpoint = (r1.powi(2) - r2.powi(2)) / 2.0 / d_sq + 0.5;
	let midpoint = c1.lerp(c2, rel_midpoint);
	// Now we try to construct the actual intersection points, they will lie at the intersections
	// of the normal at rel_midpoint with one of the circles
	let normal_offset_sq = r1.powi(2) - rel_midpoint.powi(2) * d_sq;
	// Round here, we want to cacth a single intersection with a margin of error
	const TANGENTIAL_INTERSECTION_THRESHOLD: f32 = 0.001;
	if normal_offset_sq.abs() < TANGENTIAL_INTERSECTION_THRESHOLD {
		Some(One(midpoint))
	} else if normal_offset_sq > 0.0 {
		let normal_offset = normal_offset_sq.sqrt();
		let normal_unit = (c2 - c1).normalize().perp();
		let offset = normal_offset * normal_unit;
		Some(Two(midpoint + offset, midpoint - offset))
	} else {
		None
	}
}

fn approx_eq(a: f32, b: f32) -> bool {
	// Threshold is intentionally chosen to be fairly large
	// so as to make up for rounding errors in arithmetics
	// and/or human inputs
	const THRESHOLD: f32 = 0.001;
	(a - b).abs() < THRESHOLD * a.max(b)
}
