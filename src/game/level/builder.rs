use std::f32::consts::PI;

use bevy::math::bounding::{Aabb2d, BoundingVolume};
use itertools::Itertools as _;

use super::*;
use crate::graphics::{LEVEL_AREA_CENTER, LEVEL_AREA_WIDTH};

/// Helper object for constructing a valid [`LevelData`]
#[derive(Debug)]
pub struct LevelBuilder {
	/// Name of the level, if already set
	name: Option<String>,
	/// Hint or comment on the level, if already set
	hint: Option<String>,
	/// Placements of vertices, if they have been placed yet
	vertices: Vec<IntermediateVertexData>,
	/// Placements of cycles, if they have been placed yet
	cycles: Vec<IntermediateCycleData>,
	/// Cycle links that have been explicitly added (no symmetry or transitivity)
	declared_links: Vec<DeclaredLinkData>,
}

/// Error data for [`LevelBuilderError::CycleDoesNotContainVertex`]
#[derive(Clone, Copy, PartialEq, Debug)]
pub struct CycleDoesNotContainVertexError {
	/// Index of the cycle whose attempted placement failed
	placed_cycle: usize,
	/// Placement that was requested for the cycle
	requested_placement: CyclePlacement,
	/// Index of the vertex with fixed position
	/// that would not lie on the cycle as placed
	failing_vertex: usize,
	/// Position of the failing vertex
	vertex_position: Vec2,
}

/// Error data for [`LevelBuilderError::CyclesDoNotIntersect`]
#[derive(Clone, Copy, PartialEq, Debug)]
pub struct CyclesDoNotIntersectError {
	/// Index of the cycle whose attempted placement failed
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

/// Error data for [`LevelBuilderError::CyclesDoNotIntersectTwice`]
#[derive(Clone, Copy, PartialEq, Debug)]
pub struct CyclesDoNotIntersectTwiceError {
	/// Index of the cycle whose attempted placement failed
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

/// Error data for [`LevelBuilderError::TooManyVerticesInCycleIntersection`]
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct TooManyVerticesInCycleIntersectionError {
	/// Index of the cycle whose attempted placement failed
	placed_cycle: usize,
	/// Index of the already-placed cycle that shared
	/// several vertices with the one being placed
	existing_cycle: usize,
	/// Indices of three of the vertices that are shared by the cycles
	shared_vertices: [usize; 3],
}

/// Error data for [`LevelBuilderError::OverlappedLinkedCycles`]
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct OverlappedLinkedCyclesError {
	/// Index of the cycle where a (possibly transitive) link starts
	source_cycle: usize,
	/// Index of the cycle where a link ends
	dest_cycle: usize,
	/// Index of the vertex shared by the two cycles
	shared_vertex: usize,
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum LevelBuilderError {
	/// [`set_level_name`](LevelBuilder::set_level_name)
	/// was called more than once
	LevelNameAlreadySet,
	/// [`set_level_hint`](LevelBuilder::set_level_hint)
	/// was called more than once
	LevelHintAlreadySet,
	/// An out-of-range index was used to reference a cycle
	CycleIndexOutOfRange(usize),
	/// An out-of-range index was used to reference a vertex
	VertexIndexOutOfRange(usize),
	/// A vertex shows up multiple times in the same cycle
	RepeatingVertexInCycle(usize),
	/// [`add_object`](LevelBuilder::add_object) was called
	/// more than once on the same vertex
	VertexAlreadyHasObject(usize),
	/// [`add_glyph`](LevelBuilder::add_glyph) was called
	/// more than once on the same vertex
	VertexAlreadyHasGlyph(usize),
	/// A cycle has been explicitly assigned negative radius
	CycleRadiusNotPositive(usize, f32),
	/// [`build`](LevelBuilder::build) was called
	/// while a cycle had not been placed yet
	UnplacedCycle(usize),
	/// [`place_cycle`](LevelBuilder::place_cycle) was called
	/// on a cycle that had already been placed
	CycleAlreadyPlaced(usize),
	/// [`place_cycle`](LevelBuilder::place_cycle) was called
	/// on a cycle that contains a vertex that has already been definitively placed,
	/// and it would not lie on the cycle as being placed
	CycleDoesNotContainVertex(CycleDoesNotContainVertexError),
	/// Cycles that share a vertex have been placed in a way that they do not intersect
	CyclesDoNotIntersect(CyclesDoNotIntersectError),
	/// Cycles that share two vertices have been placed in a way that they
	/// only intersect tangentially (only enough space for one shared vertex)
	CyclesDoNotIntersectTwice(CyclesDoNotIntersectTwiceError),
	/// Two cycles share more than two vertices
	TooManyVerticesInCycleIntersection(TooManyVerticesInCycleIntersectionError),
	/// [`place_vertex`](LevelBuilder::place_vertex) was called
	/// on a vertex that already has a fixed placement
	VertexAlreadyPlaced(usize),
	/// [`place_vertex`](LevelBuilder::place_vertex) was called
	/// on a vertex that already has not yet been partially placed
	VertexNotPartiallyPlaced(usize),
	/// [`place_cycle`](LevelBuilder::place_cycle) or
	/// [`place_vertex`](LevelBuilder::place_vertex) was called
	/// in a way that would place vertices around a cycle out of their rotation order
	VertexOrderViolationOnCycle(usize),
	/// Two cycles are linked, but they share a vertex
	OverlappedLinkedCycles(OverlappedLinkedCyclesError),
	/// There is a loop in cycle links, and their directions are contradicting
	CycleLinkageConflict(usize, usize),
}

#[derive(Clone, Copy, Debug)]
struct IntermediateVertexData {
	/// Position of the vertex, if it has been placed yet
	pub position: IntermediateVertexPosition,
	/// Object that lies on the vertex, if any
	pub object: Option<ObjectData>,
	/// Glyph that lies on the vertex, if any
	pub glyph: Option<GlyphData>,
}

#[derive(Clone, Debug)]
struct IntermediateCycleData {
	/// Placement of the cycle, if it has been placed yet
	pub placement: Option<CyclePlacement>,
	/// Indices into [`LevelData::vertices`]
	/// that identify the vertices that lie on the cycle, in clockwise order
	pub vertex_indices: Vec<usize>,
	/// When the cycle can be turned
	pub turnability: CycleTurnability,
	/// How other cycles turn together with this one
	pub link_matrix: Vec<Option<LinkedCycleDirection>>,
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

impl LevelBuilder {
	pub const PLACEHOLDER_LEVEL_NAME: &'static str = "NAME_MISSING";

	pub fn new() -> Self {
		Self {
			name: None,
			hint: None,
			vertices: Vec::new(),
			cycles: Vec::new(),
			declared_links: Vec::new(),
		}
	}

	pub fn set_level_name(&mut self, name: String) -> Result<(), LevelBuilderError> {
		if self.name.is_some() {
			return Err(LevelBuilderError::LevelNameAlreadySet);
		}
		self.name = Some(name);
		Ok(())
	}

	pub fn set_level_hint(&mut self, hint: String) -> Result<(), LevelBuilderError> {
		if self.hint.is_some() {
			return Err(LevelBuilderError::LevelHintAlreadySet);
		}
		self.hint = Some(hint);
		Ok(())
	}

	pub fn add_vertex(&mut self) -> Result<usize, LevelBuilderError> {
		self.vertices.push(IntermediateVertexData {
			position: IntermediateVertexPosition::Free,
			object: None,
			glyph: None,
		});
		Ok(self.vertices.len() - 1)
	}

	pub fn add_cycle(
		&mut self,
		turnability: CycleTurnability,
		vertex_indices: impl IntoIterator<Item = usize>,
	) -> Result<usize, LevelBuilderError> {
		let vertex_indices = vertex_indices.into_iter().collect::<Vec<_>>();
		if let Some(i) = vertex_indices.iter().duplicates().next() {
			return Err(LevelBuilderError::RepeatingVertexInCycle(*i));
		}
		if let Some(i) = vertex_indices
			.iter()
			.copied()
			.find(|&i| i >= self.vertices.len())
		{
			return Err(LevelBuilderError::VertexIndexOutOfRange(i));
		}
		// Add the cycle entry
		self.cycles.push(IntermediateCycleData {
			placement: None,
			vertex_indices,
			turnability,
			link_matrix: vec![None; self.cycles.len()],
		});
		// Extend all existing link matrices, they must cross-reference the new cycle
		for cycle in self.cycles.iter_mut() {
			cycle.link_matrix.push(None);
		}
		// The link matrix has to be reflective by definition
		// It is our responsibility to fill in that one new disgonal tile
		let new_reflective = self
			.cycles
			.last_mut()
			.expect("A cycle has just been inserted")
			.link_matrix
			.last_mut()
			.expect("The link matrix has just been extended");
		*new_reflective = Some(LinkedCycleDirection::Coincident);
		Ok(self.cycles.len() - 1)
	}

	pub fn add_object(
		&mut self,
		vertex_index: usize,
		object: ObjectData,
	) -> Result<(), LevelBuilderError> {
		if vertex_index >= self.vertices.len() {
			return Err(LevelBuilderError::VertexIndexOutOfRange(vertex_index));
		}
		if self.vertices[vertex_index].object.is_some() {
			return Err(LevelBuilderError::VertexAlreadyHasObject(vertex_index));
		}
		self.vertices[vertex_index].object = Some(object);
		Ok(())
	}

	pub fn add_glyph(
		&mut self,
		vertex_index: usize,
		glyph: GlyphData,
	) -> Result<(), LevelBuilderError> {
		if vertex_index >= self.vertices.len() {
			return Err(LevelBuilderError::VertexIndexOutOfRange(vertex_index));
		}
		if self.vertices[vertex_index].glyph.is_some() {
			return Err(LevelBuilderError::VertexAlreadyHasGlyph(vertex_index));
		}
		self.vertices[vertex_index].glyph = Some(glyph);
		Ok(())
	}

	/// Links two cycles. The link is both symmetric and transitive.
	/// ## Notes
	/// This function only has basic safety guarantee.
	/// If it fails, the two cycles may become partially linked.
	/// The builder as a whole should remain in a valid state.
	pub fn link_cycles(
		&mut self,
		source_cycle: usize,
		dest_cycle: usize,
		direction: LinkedCycleDirection,
	) -> Result<(), LevelBuilderError> {
		if source_cycle >= self.cycles.len() {
			return Err(LevelBuilderError::CycleIndexOutOfRange(source_cycle));
		}
		if dest_cycle >= self.cycles.len() {
			return Err(LevelBuilderError::CycleIndexOutOfRange(dest_cycle));
		}
		// Add the link symmetrically, in both directions
		self.add_asymmetric_cycle_link(source_cycle, dest_cycle, direction)?;
		self.add_asymmetric_cycle_link(dest_cycle, source_cycle, direction)?;
		self.declared_links.push(DeclaredLinkData {
			source_cycle,
			dest_cycle,
			direction,
		});
		Ok(())
	}

	pub fn place_cycle(
		&mut self,
		target_cycle: usize,
		center: Vec2,
		radius: f32,
		double_intersection_hints: &[usize],
	) -> Result<(), LevelBuilderError> {
		if target_cycle >= self.cycles.len() {
			return Err(LevelBuilderError::CycleIndexOutOfRange(target_cycle));
		}
		if self.cycles[target_cycle].placement.is_some() {
			return Err(LevelBuilderError::CycleAlreadyPlaced(target_cycle));
		}
		if !(radius > 0.0) {
			return Err(LevelBuilderError::CycleRadiusNotPositive(
				target_cycle,
				radius,
			));
		}
		let placement = CyclePlacement {
			position: center,
			radius,
		};
		// This will be filled with placements of all vertices after the cycle is placed
		let mut placements_after = Vec::new();
		// This will be filled with vertices that are already partially placed
		let mut new_fixed_points = std::collections::BTreeMap::new();
		let vertex_indices = &self.cycles[target_cycle].vertex_indices;
		for (j, &i) in vertex_indices.iter().enumerate() {
			match self.vertices[i].position {
				IntermediateVertexPosition::Fixed(pos) => {
					// Fixed vertex cannot be moved, we can only proceed if
					// it already lies on the cycle being placed
					if !approx_eq(center.distance_squared(pos), radius.powi(2)) {
						return Err(LevelBuilderError::CycleDoesNotContainVertex(
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
						.placement
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
								return Err(LevelBuilderError::CyclesDoNotIntersectTwice(
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
						Some(Two(alt_pos, default_pos)) => {
							// If there are two intersections, we must choose one of them
							//
							// Since the cycles are counterclockwise, the usual best choice
							// for a vertex is the second intersection, then the first
							// intersection for the potential second vertex shared with the same other cycle.
							// If the opposite is desired (for example, on the off chance
							// that the two shared vertices are split over the start/end of the
							// vertex list as declared), `double_intersection_hints` can be used
							// to express this intent
							let new_pos = match new_fixed_points.entry(p.owner_cycle) {
								std::collections::btree_map::Entry::Occupied(mut e) => {
									let val = e.get_mut();
									let new_pos =
										if double_intersection_hints.contains(&val.first().0) {
											default_pos
										} else {
											alt_pos
										};
									*val = val.add_to_one((i, new_pos)).map_err(
										|((a, _), (b, _), (c, _))| {
											LevelBuilderError::TooManyVerticesInCycleIntersection(
												TooManyVerticesInCycleIntersectionError {
													placed_cycle: target_cycle,
													existing_cycle: p.owner_cycle,
													shared_vertices: [a, b, c],
												},
											)
										},
									)?;
									new_pos
								}
								std::collections::btree_map::Entry::Vacant(e) => {
									let new_pos = if double_intersection_hints.contains(&i) {
										alt_pos
									} else {
										default_pos
									};
									e.insert(One((i, new_pos)));
									new_pos
								}
							};
							placements_after.push(IntermediateVertexPosition::Fixed(new_pos));
						}
						None => {
							// Fail if the cycles do not intersect
							return Err(LevelBuilderError::CyclesDoNotIntersect(
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
			return Err(LevelBuilderError::VertexOrderViolationOnCycle(target_cycle));
		}
		// Newly fixed points on other cycles must also work with those cycles
		for (cycle_index, placement) in new_fixed_points {
			if !self.verify_materialization_against_cycle(cycle_index, placement.into_iter()) {
				return Err(LevelBuilderError::VertexOrderViolationOnCycle(cycle_index));
			}
		}
		// We are done verifying that the placement is valid, now we can commit to it
		for (&i, new_pos) in vertex_indices.iter().zip_eq(placements_after) {
			self.vertices[i].position = new_pos;
		}
		self.cycles[target_cycle].placement = Some(placement);
		Ok(())
	}

	/// Assigns a fixed placement to a partially placed vertex
	/// ## Parameters
	/// - `target_vertex` - Index of the vertex to place
	/// - `clock_angle` - Clock angle between the center of the owning
	///   cycle and the vertex (zero is up, positive is clockwise)
	pub fn place_vertex(
		&mut self,
		target_vertex: usize,
		clock_angle: f32,
	) -> Result<(), LevelBuilderError> {
		if target_vertex >= self.vertices.len() {
			return Err(LevelBuilderError::VertexIndexOutOfRange(target_vertex));
		}
		match self.vertices[target_vertex].position {
			IntermediateVertexPosition::Free => {
				Err(LevelBuilderError::VertexNotPartiallyPlaced(target_vertex))
			}
			IntermediateVertexPosition::Fixed(_) => {
				Err(LevelBuilderError::VertexAlreadyPlaced(target_vertex))
			}
			IntermediateVertexPosition::Partial(p) => {
				let owner_placement = self.cycles[p.owner_cycle]
					.placement
					.expect("Owner cycle of a partially placed vertex should also be placed");
				// Recalculate from clock angle to angle of the actual vertor space
				let real_angle = PI / 2.0 - clock_angle;
				let new_pos = owner_placement.position
					+ owner_placement.radius * Vec2::from_angle(real_angle);
				if !self.verify_materialization_against_cycle(
					p.owner_cycle,
					std::iter::once((target_vertex, new_pos)),
				) {
					return Err(LevelBuilderError::VertexOrderViolationOnCycle(
						p.owner_cycle,
					));
				}
				self.vertices[target_vertex].position = IntermediateVertexPosition::Fixed(new_pos);
				Ok(())
			}
		}
	}

	/// Checks that the level data is complete and assembles it
	pub fn build(mut self) -> Result<LevelData, LevelBuilderError> {
		self.validate_before_build()?;
		self.materialize_partial_vertex_placements();
		self.fit_to_viewport(Aabb2d::new(LEVEL_AREA_CENTER, LEVEL_AREA_WIDTH / 2.0));
		let cycles = self
			.cycles
			.into_iter()
			.map(Self::build_cycle_data)
			.collect();
		let vertices = self
			.vertices
			.into_iter()
			.map(Self::build_vertex_data)
			.collect();
		Ok(LevelData {
			name: self
				.name
				.unwrap_or_else(|| Self::PLACEHOLDER_LEVEL_NAME.to_owned()),
			hint: self.hint,
			vertices,
			cycles,
			declared_links: self.declared_links,
		})
	}

	/// Asserts that a vertex data object is complete and assembles it
	/// ## Panics
	/// Panics if the vertex is partially placed
	fn build_vertex_data(intermediate: IntermediateVertexData) -> VertexData {
		let position = match intermediate.position {
			IntermediateVertexPosition::Fixed(pos) => pos,
			// This is technically possible, since a vertex can belong to no cycle
			IntermediateVertexPosition::Free => Vec2::ZERO,
			// Prevented by [`materialize_all_partial_vertex_placements`]
			IntermediateVertexPosition::Partial(_) => {
				panic!("Partially placed vertex in build phase, should have been materialized")
			}
		};
		VertexData {
			position,
			object: intermediate.object,
			glyph: intermediate.glyph,
		}
	}

	/// Asserts that a cycle data object is complete and assembles it
	/// ## Panics
	/// Panics if the cycle has not been placed
	fn build_cycle_data(intermediate: IntermediateCycleData) -> CycleData {
		let placement = intermediate
			.placement
			.expect("Unplaced cycle in build phase, should have been detected earlier");
		let link_closure = intermediate
			.link_matrix
			.into_iter()
			.enumerate()
			.filter_map(|(i, dir)| dir.map(|dir| (i, dir)))
			.collect();
		CycleData {
			placement,
			vertex_indices: intermediate.vertex_indices,
			turnability: intermediate.turnability,
			link_closure,
		}
	}

	/// Verifies that the level data is complete and ready to be built without an error
	fn validate_before_build(&self) -> Result<(), LevelBuilderError> {
		// All cycles must be placed
		for (i, cycle) in self.cycles.iter().enumerate() {
			if cycle.placement.is_none() {
				return Err(LevelBuilderError::UnplacedCycle(i));
			}
		}
		Ok(())
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
			.zip_eq(&self.cycles)
			.filter_map(|((i, cycle), data)| {
				cycle
					.placement
					.as_ref()
					.map(|placement| (i, placement, data))
			});
		for (cycle_index, cycle_placement, cycle_data) in cycles {
			// Vertices that already have fixed placement split the cycle
			// into segments. Each segment will be handled separately
			let mut fixed_vertices = cycle_data
				.vertex_indices
				.iter()
				.enumerate()
				.filter_map(|(i, &j)| self.vertices[j].position.get_fixed().map(|pos| (i, pos)))
				// Collect and cast back into iterator so we can modify [`self.vertices`]
				.collect::<Vec<_>>()
				.into_iter();
			if let Some((first_fixed_vertex, first_fixed_pos)) = fixed_vertices.next() {
				let first_relative_pos = first_fixed_pos - cycle_placement.position;
				let mut current_fixed_vertex = first_fixed_vertex;
				let mut current_relative_pos = first_relative_pos;
				for (next_fixed_vertex, next_fixed_pos) in fixed_vertices {
					// Materialize all vertices between the marked ones
					let next_relative_pos = next_fixed_pos - cycle_placement.position;
					let vertex_count = next_fixed_vertex - current_fixed_vertex;
					let mut segment_angle = next_relative_pos.angle_between(current_relative_pos);
					if segment_angle <= 0.0 {
						segment_angle += 2.0 * PI;
					}
					// Distribute the partially-placed vertices uniformly between the fixed ones
					for (j, i) in ((current_fixed_vertex + 1)..next_fixed_vertex).enumerate() {
						let target_vertex = cycle_data.vertex_indices[i];
						let new_pos = cycle_placement.position
							+ current_relative_pos.rotate(Vec2::from_angle(
								-segment_angle * (j + 1) as f32 / vertex_count as f32,
							));
						Self::checked_materialize(
							&mut self.vertices[target_vertex].position,
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
				let mut segment_angle = first_relative_pos.angle_between(current_relative_pos);
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
							-segment_angle * (j + 1) as f32 / vertex_count as f32,
						));
					Self::checked_materialize(
						&mut self.vertices[target_vertex].position,
						new_pos,
						cycle_index,
						i,
					);
				}
			} else {
				// If none of the vertices have fixed placement, distribute them uniformly in clock order
				let vertex_count = cycle_data.vertex_indices.len();
				for (i, &j) in cycle_data.vertex_indices.iter().enumerate() {
					let new_pos = cycle_placement.position
						+ cycle_placement.radius
							* Vec2::from_angle(
								PI / 2.0 - 2.0 * PI * i as f32 / vertex_count as f32,
							);
					Self::checked_materialize(
						&mut self.vertices[j].position,
						new_pos,
						cycle_index,
						i,
					);
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
	///   The list is searched linearly for every vertex of the cycle,
	///   as the list is expected to be very small.
	///   Use a fast implementation of iterator
	/// - Only the cycle order is verified, radius of the cycle is not checked
	fn verify_materialization_against_cycle(
		&self,
		cycle_index: usize,
		points_to_materialize: impl Iterator<Item = (usize, Vec2)> + Clone,
	) -> bool {
		let cycle_center = self.cycles[cycle_index]
			.placement
			.expect("Partial materialization checks can only be run on placed cycles")
			.position;
		let fixed_points = self.cycles[cycle_index]
			.vertex_indices
			.iter()
			.filter_map(|&i| {
				points_to_materialize
					.clone()
					.find_map(|(j, p)| if i == j { Some(p) } else { None })
					.or_else(|| self.vertices[i].position.get_fixed())
			});
		Self::are_points_in_cyclic_order(cycle_center, fixed_points)
	}

	/// Checks whether a sequence of points is ordered in order of
	/// clockwise navigation around a center point
	fn are_points_in_cyclic_order(center: Vec2, points: impl Iterator<Item = Vec2>) -> bool {
		let mut points = points.map(|p| p - center);
		let mut total_angle = 0.0;
		if let Some(mut current) = points.next() {
			for next in points {
				let mut angle = next.angle_between(current);
				if angle < 0.0 {
					// Recalculate angles to [0, 2 * pi]
					// so that we stay in clockwise movement
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

	/// Links two cycles asymmetrically (the link only goes from source to destination)
	/// The link is transitive.
	fn add_asymmetric_cycle_link(
		&mut self,
		source_cycle: usize,
		dest_cycle: usize,
		direction: LinkedCycleDirection,
	) -> Result<(), LevelBuilderError> {
		for i in 0..self.cycles.len() {
			// If a cycle is linked to the source, it is now by transitivity also linked to destination
			if let Some(old_link_direction) = self.cycles[i].link_matrix[source_cycle] {
				self.add_nontransitive_cycle_link(i, dest_cycle, old_link_direction * direction)?;
			}
			// If destination is linked to a cycle, source is now by transitivity also linked to that cycle
			if let Some(old_link_direction) = self.cycles[dest_cycle].link_matrix[i] {
				self.add_nontransitive_cycle_link(source_cycle, i, old_link_direction * direction)?;
			}
		}
		Ok(())
	}

	/// Links two cycles atomically. Only these two cycles will be linked,
	/// it is not symmetric or transitive.
	fn add_nontransitive_cycle_link(
		&mut self,
		source_cycle: usize,
		dest_cycle: usize,
		direction: LinkedCycleDirection,
	) -> Result<(), LevelBuilderError> {
		if let Some(existing_link_direction) = self.cycles[source_cycle].link_matrix[dest_cycle] {
			// The cycles are already linked
			// We must verify that they have the same direction
			if existing_link_direction != direction {
				return Err(LevelBuilderError::CycleLinkageConflict(
					source_cycle,
					dest_cycle,
				));
			}
		} else {
			// The cycles are not yet linked
			// We must verify that they do not have a vertex in common
			let maybe_shared_vertex = self.cycles[source_cycle]
				.vertex_indices
				.iter()
				.chain(self.cycles[dest_cycle].vertex_indices.iter())
				.duplicates()
				.copied()
				.next();
			if let Some(shared_vertex) = maybe_shared_vertex {
				return Err(LevelBuilderError::OverlappedLinkedCycles(
					OverlappedLinkedCyclesError {
						source_cycle,
						dest_cycle,
						shared_vertex,
					},
				));
			}
			// Now link them together
			self.cycles[source_cycle].link_matrix[dest_cycle] = Some(direction);
		}
		Ok(())
	}

	/// Calculates the bounding box of all currently placed cycles
	fn get_bounding_box(&self) -> Aabb2d {
		let min = self
			.cycles
			.iter()
			.filter_map(|cycle| cycle.placement.map(|p| p.position - Vec2::splat(p.radius)))
			.fold(Vec2::INFINITY, Vec2::min);
		let max = self
			.cycles
			.iter()
			.filter_map(|cycle| cycle.placement.map(|p| p.position + Vec2::splat(p.radius)))
			.fold(Vec2::NEG_INFINITY, Vec2::max);
		let center = (max + min) / 2.0;
		let mut half = (max - min) / 2.0;
		// Prevent zero-size bounding boxes
		if half.x <= 0.0 {
			half.x = 1.0;
			log::warn!("Level bounding box has zero width.");
		}
		if half.y <= 0.0 {
			half.y = 1.0;
			log::warn!("Level bounding box has zero height.");
		}
		Aabb2d::new(center, half)
	}

	/// Resizes all currently placed objects to fit a bounding box
	fn fit_to_viewport(&mut self, viewport: Aabb2d) {
		let bounds = self.get_bounding_box();
		let scale = viewport.half_size() / bounds.half_size();
		// Scaling must be equal in both directions
		let scale = scale.x.min(scale.y);
		let viewport_center = viewport.center();
		let bounds_center = bounds.center();

		for vertex in &mut self.vertices {
			if let IntermediateVertexPosition::Fixed(p) = &mut vertex.position {
				*p = (*p - bounds_center) * scale + viewport_center;
			}
		}
		for cycle in &mut self.cycles {
			if let Some(p) = &mut cycle.placement {
				p.position = (p.position - bounds_center) * scale + viewport_center;
				p.radius *= scale;
			}
		}
	}
}

impl std::error::Error for LevelBuilderError {}

impl std::fmt::Display for LevelBuilderError {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::LevelNameAlreadySet => write!(f, "Level name has been set multiple times."),
			Self::LevelHintAlreadySet => write!(f, "Level hint has been set multiple times."),
			Self::VertexIndexOutOfRange(i) => write!(f, "Vertex {i} has been referenced, but there are not that many vertices."),
			Self::CycleIndexOutOfRange(i) => write!(f, "Cycle {i} has been referenced, but there are not that many cycles."),
			Self::RepeatingVertexInCycle(i) => write!(f, "Cannot create cycle that contains vertex {i} multiple times."),
			Self::VertexAlreadyHasObject(i) => write!(f, "Cannot place object at vertex {i} because an object is already there."),
			Self::VertexAlreadyHasGlyph(i) => write!(f, "Cannot place glyph at vertex {i} because a glyph is already there."),
			Self::CycleRadiusNotPositive(i, r) => write!(f, "Radius of cycle {i} is not positive ({r})"),
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
			Self::TooManyVerticesInCycleIntersection(e) => write!(
				f,
				"Cycles {} and {} cannot be placed because they sharemore than two vertices: {}, {}, {}.",
				e.placed_cycle,
				e.existing_cycle,
				e.shared_vertices[0],
				e.shared_vertices[1],
				e.shared_vertices[2],
			),
			Self::VertexOrderViolationOnCycle(i) => write!(f, "Placement is not valid because vertices around cycle {i} would be out of order."),
			Self::OverlappedLinkedCycles(e) => write!(f, "Cycles {} and {} cannot be linked because they share vertex {}.", e.source_cycle, e.dest_cycle, e.shared_vertex),
			Self::CycleLinkageConflict(a, b) => write!(f, "Cycles {a} and {b} cannot be linked because they are already linked in the opposite direction."),
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
