use std::f32::consts::PI;

use super::*;

/// Specification of radius of the representation of a cycle
#[derive(Clone, Copy, Debug)]
pub enum DeclaredCycleRadius {
	/// Exact radius expressed in the global units
	WorldUnits(f32),
	/// Preferred radius of a cycle with a given vertex count.
	/// Non-integer numbers are allowed, since the conversion
	/// function happens to be continuous
	DefaultForVertexCount(f32),
	/// Preferred radius of the targeted cycle
	DefaultForCycle,
}

/// Declarative specification of the placement of a lone cycle
#[derive(Clone, Copy, Debug)]
pub struct FirstCycleDeclaredPlacement {
	/// Index of the cycle being laid out
	pub cycle_index: usize,
	/// Position of the center point of the cycle
	pub position: Vec2,
	/// Radius of the cycle
	pub radius: DeclaredCycleRadius,
}

/// Declarative specification of the placement of a cycle
/// that intersects one cycle that has already been laid out
#[derive(Clone, Copy, Debug)]
pub struct JoinedCycleDeclaredPlacement {
	/// Index of the cycle being laid out
	pub cycle_index: usize,
	/// Index of the already-laid-out cycle that the target cycle intersects
	pub existing_cycle_index: usize,
	/// Radius of the cycle
	pub radius: DeclaredCycleRadius,
	/// Angle of the vector from the centerpoint
	/// of the existing cycle to the centerpoint of the current cycle.
	/// If omitted, it is laid out to distribute vertices of the
	/// existing cycle as uniformly as possible
	pub relative_angle: Option<f32>,
}

/// Enumerates possible ways of specifying the placement
/// of a cycle relative to two other cycles
#[derive(Clone, Copy, Debug)]
pub enum DoubleJoinPlacementMetric {
	/// Specify the radius of the cycle being laid out
	Radius(DeclaredCycleRadius),
	/// Specify the angle of the vector from the centerpoint
	/// of first of the existing cycles to the centerpoint
	/// of the new cycle
	RelativeAngle(f32),
}

/// Declarative specification of the placement of a cycle
/// that intersects two cycles that have already been laid out
#[derive(Clone, Copy, Debug)]
pub struct DoublyJoinedCycleDeclaredPlacement {
	/// Index of the cycle being laid out
	pub cycle_index: usize,
	/// Index of the already-laid-out cycles that the target cycle intersects
	pub existing_cycle_indices: [usize; 2],
	/// Optionally specify the placement of the new cycle.
	/// If omitted, it is laid out to distribute vertices of the
	/// existing cycles as uniformly as possible
	pub metric: Option<DoubleJoinPlacementMetric>,
}

/// Declarative specification of the placement of a cycle
/// that intersects three cycles that have already been laid out
#[derive(Clone, Copy, Debug)]
pub struct TriplyJoinedCycleDeclaredPlacement {
	/// Index of the cycle being laid out
	pub cycle_index: usize,
	/// Index of the already-laid-out cycles that the target cycle intersects
	pub existing_cycle_indices: [usize; 3],
}

/// Declarative specification of the placement of a vertex
/// that does not lie in the intersection of multiple cycles
#[derive(Clone, Copy, Debug)]
pub struct VertexDeclaredPlacement {
	/// Index of the vertex being laid out
	pub vertex_index: usize,
	/// Angle of the vector from the centerpoint of the cycle that
	/// owns the vertex to the vertex
	pub relative_angle: f32,
}

/// Declarative specification of the placement of a game object
#[derive(Clone, Copy, Debug)]
pub enum DeclaredPlacement {
	/// Place a cycle that does not intersects any already-placed cycles
	FirstCycle(FirstCycleDeclaredPlacement),
	/// Place a cycle that intersects one already-placed cycle
	Join(JoinedCycleDeclaredPlacement),
	/// Place a cycle that intersects two already-placed cycles
	Join2(DoublyJoinedCycleDeclaredPlacement),
	/// Place a cycle that intersects three already-placed cycles
	Join3(TriplyJoinedCycleDeclaredPlacement),
	/// Place a vertex that belongs to exactly one already-placed cycle
	/// and does not yet have a fixed position
	Vertex(VertexDeclaredPlacement),
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
	/// Map from pairs of cycles to the vertices where they intersect.
	/// First index in the key is always the lower one
	intersections: std::collections::BTreeMap<(usize, usize), CycleIntersection>,
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
	/// on a cycle that contains a vertex that has already been definitively placed
	///
	/// **parameters**: The cycle, the vertex
	ConflictingPlacedVertex(usize, usize),
	/// [`add_placement`](LevelLayoutBuilder::add_placement) was called
	/// on a cycle that contains a vertex that has already been partially
	/// placed, and is not owned by one of the cycles that the placement expects
	///
	/// **parameters**: The cycle, the vertex, the owning cycle
	ConflictingPartiallyPlacedVertex(usize, usize, usize),
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

/// Represents the vertices on the intersection of two cycles
#[derive(Clone, Copy, Debug)]
enum CycleIntersection {
	/// Cycles intersect at one vertex
	One(usize),
	/// Cycles intersect at two vertices
	Two(usize, usize),
}

impl DeclaredCycleRadius {
	pub fn computed_value(self, cycle_vertex_count: usize) -> f32 {
		match self {
			Self::WorldUnits(x) => x,
			Self::DefaultForVertexCount(x) => Self::default_size_for_vertex_count(x),
			Self::DefaultForCycle => Self::default_size_for_vertex_count(cycle_vertex_count as f32),
		}
	}

	const DEFAULT_VERTEX_SPACING: f32 = 100.0;

	fn default_size_for_vertex_count(vertices: f32) -> f32 {
		// We want a circle that will layout the vertices
		// at a constant distance when they are spaced evenly
		// https://math.stackexchange.com/a/2589
		// I was too lazy to derive it myself, do not judge me
		Self::DEFAULT_VERTEX_SPACING / 2.0 / (PI / vertices).sin()
	}
}

impl IntermediateVertexPosition {
	fn get_fixed(&self) -> Option<Vec2> {
		match self {
			Self::Fixed(pos) => Some(*pos),
			_ => None,
		}
	}
}

impl<'w> LevelLayoutBuilder<'w> {
	pub fn new(level: &'w ValidLevelData) -> Self {
		let vertices_per_cycle = level
			.cycles
			.iter()
			.map(|c| std::collections::BTreeSet::from_iter(c.vertex_indices.iter().copied()))
			.collect::<Vec<_>>();
		let intersections = vertices_per_cycle.iter()
			.enumerate()
			.tuple_combinations()
			.filter_map(|((i, a), (j, b))| {
				let mut intersection = None;
				for &vertex in a.intersection(b) {
					intersection = match intersection {
						None => Some(CycleIntersection::One(vertex)),
						Some(CycleIntersection::One(old_vertex)) => Some(CycleIntersection::Two(old_vertex, vertex)),
						// [`ValidLevelData`] ensures level data integrity
						Some(CycleIntersection::Two(..)) => panic!("More than two vertices in intersection of two cycles, should have been caught by sanitizer")
					}
				}
				intersection.map(|x| ((i, j), x))
			})
			.collect();
		Self {
			level,
			vertices: vec![IntermediateVertexPosition::Free; level.vertices.len()],
			cycles: vec![None; level.cycles.len()],
			intersections,
		}
	}

	pub fn add_placement(&mut self, placement: DeclaredPlacement) -> Result<(), LevelLayoutError> {
		match placement {
			DeclaredPlacement::FirstCycle(p) => self.place_cycle(p),
			DeclaredPlacement::Join(p) => self.place_cycle_joined(p),
			DeclaredPlacement::Join2(p) => self.place_cycle_joined_2(p),
			DeclaredPlacement::Join3(p) => self.place_cycle_joined_3(p),
			DeclaredPlacement::Vertex(p) => self.place_partial_vertex(p),
		}
	}

	pub fn place_cycle(
		&mut self,
		placement: FirstCycleDeclaredPlacement,
	) -> Result<(), LevelLayoutError> {
		if placement.cycle_index >= self.cycles.len() {
			return Err(LevelLayoutError::CycleIndexOutOfRange(
				placement.cycle_index,
			));
		}
		if self.cycles[placement.cycle_index].is_some() {
			return Err(LevelLayoutError::CycleAlreadyPlaced(placement.cycle_index));
		}
		self.checked_reposition_cycle_vertices(
			placement.cycle_index,
			placement.position,
			|i, pos| match pos {
				IntermediateVertexPosition::Fixed(_) => Err(LevelLayoutError::ConflictingPlacedVertex(
					placement.cycle_index,
					i,
				)),
				IntermediateVertexPosition::Partial(p) => Err(LevelLayoutError::ConflictingPartiallyPlacedVertex(
					placement.cycle_index,
					i,
					p.owner_cycle,
				)),
				IntermediateVertexPosition::Free => Ok(IntermediateVertexPosition::Partial(PartiallyBoundVertexPosition {
					owner_cycle: placement.cycle_index,
					index_in_owner: i,
				}))
			}
		)?;
		let cycle_vertex_count = self.level.cycles[placement.cycle_index]
			.vertex_indices
			.len();
		self.cycles[placement.cycle_index] = Some(CyclePlacement {
			position: placement.position,
			radius: placement.radius.computed_value(cycle_vertex_count),
		});
		Ok(())
	}

	pub fn place_cycle_joined(
		&mut self,
		_placement: JoinedCycleDeclaredPlacement,
	) -> Result<(), LevelLayoutError> {
		todo!();
	}

	pub fn place_cycle_joined_2(
		&mut self,
		_placement: DoublyJoinedCycleDeclaredPlacement,
	) -> Result<(), LevelLayoutError> {
		todo!();
	}

	pub fn place_cycle_joined_3(
		&mut self,
		_placement: TriplyJoinedCycleDeclaredPlacement,
	) -> Result<(), LevelLayoutError> {
		todo!();
	}

	pub fn place_partial_vertex(
		&mut self,
		placement: VertexDeclaredPlacement,
	) -> Result<(), LevelLayoutError> {
		if placement.vertex_index >= self.vertices.len() {
			return Err(LevelLayoutError::VertexIndexOutOfRange(
				placement.vertex_index,
			));
		}
		match self.vertices[placement.vertex_index] {
			IntermediateVertexPosition::Free => Err(LevelLayoutError::VertexNotPartiallyPlaced(
				placement.vertex_index,
			)),
			IntermediateVertexPosition::Fixed(_) => Err(LevelLayoutError::VertexAlreadyPlaced(
				placement.vertex_index,
			)),
			IntermediateVertexPosition::Partial(p) => {
				let owner_placement = self.cycles[p.owner_cycle]
					.expect("Owner cycle of a partially placed vertex should also be placed");
				let new_pos = owner_placement.position
					+ owner_placement.radius * Vec2::from_angle(placement.relative_angle);
				self.checked_reposition_cycle_vertices(
					p.owner_cycle,
					owner_placement.position,
					|i, pos| if i == placement.vertex_index {
						Ok(IntermediateVertexPosition::Fixed(new_pos))
					} else {
						Ok(pos)
					}
				)
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

	/// Iterates over vertices of a cycle and applies a fallible transformation to each,
	/// then checks whether the new layout respects cycle order and commits it
	/// ## Side Effects
	/// If any invocation of the transformation fails, nothing happens.
	/// If the transformed placements do not respect cycle order, nothing happens.
	/// In both cases, this is indicated by an error value being returned
	fn checked_reposition_cycle_vertices(
		&mut self,
		cycle_index: usize,
		cycle_center: Vec2,
		mut f: impl FnMut(
			usize,
			IntermediateVertexPosition,
		) -> Result<IntermediateVertexPosition, LevelLayoutError>,
	) -> Result<(), LevelLayoutError> {
		let vertex_indices = &self.level.cycles[cycle_index].vertex_indices;
		let new_placements = vertex_indices.iter()
			.map(|&i| f(i, self.vertices[i]))
			.collect::<Result<Vec<_>, _>>()?;
		let fixed_placements = new_placements.iter()
			.filter_map(|p| p.get_fixed());
		if !Self::are_points_in_cyclic_order(cycle_center, fixed_placements) {
			return Err(LevelLayoutError::VertexOrderViolationOnCycle(cycle_index));
		}
		for (i, new_placement) in new_placements.into_iter().enumerate() {
			self.vertices[vertex_indices[i]] = new_placement;
		}
		Ok(())
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

impl std::error::Error for LevelLayoutError {}

impl std::fmt::Display for LevelLayoutError {
	fn fmt(&self, _f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		todo!()
	}
}
