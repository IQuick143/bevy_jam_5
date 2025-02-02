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
	/// One way links that have been explicitly added (no transitivity)
	declared_one_way_links: Vec<DeclaredLinkData>,
}

/// Enumerates the possible sets of positions
/// for automatic cycle-bound placement of labels for colored buttons
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum CycleBoundColorLabelPositionSet {
	/// The label can be placed to the left or right of the button
	LeftRight,
	/// The label can be placed above or below the button
	AboveBelow,
	/// The label can be placed in any cardinal direction from the button
	CardinalDirections,
	/// The label can be placed in any direction from the button,
	/// and it will be aligned laterally
	AllDirections,
	/// Thelabel can be placed in any direction from the button,
	/// and it will be rotated
	AllDirectionsRotated,
}

/// Error data for [`LevelBuilderError::CycleDoesNotContainVertex`]
#[derive(Clone, Copy, PartialEq, Debug)]
pub struct CycleDoesNotContainVertexError {
	/// Index of the cycle whose attempted placement failed
	pub placed_cycle: usize,
	/// Placement that was requested for the cycle
	pub requested_placement: CyclePlacement,
	/// Index of the vertex with fixed position
	/// that would not lie on the cycle as placed
	pub failing_vertex: usize,
	/// Position of the failing vertex
	pub vertex_position: Vec2,
}

/// Error data for [`LevelBuilderError::CyclesDoNotIntersect`]
#[derive(Clone, Copy, PartialEq, Debug)]
pub struct CyclesDoNotIntersectError {
	/// Index of the cycle whose attempted placement failed
	pub placed_cycle: usize,
	/// Placement that was requested for the cycle
	pub requested_placement: CyclePlacement,
	/// Index of the already-placed cycle that shared a vertex
	/// with the one being placed
	pub existing_cycle: usize,
	/// Placement of the already-placed cycle
	pub existing_placement: CyclePlacement,
	/// Index of the vertex that the cycles share
	/// that could not be placed because the cycles do not intersect
	pub failing_vertex: usize,
}

/// Error data for [`LevelBuilderError::CyclesDoNotIntersectTwice`]
#[derive(Clone, Copy, PartialEq, Debug)]
pub struct CyclesDoNotIntersectTwiceError {
	/// Index of the cycle whose attempted placement failed
	pub placed_cycle: usize,
	/// Placement that was requested for the cycle
	pub requested_placement: CyclePlacement,
	/// Index of the already-placed cycle that shared
	/// two vertices with the one being placed
	pub existing_cycle: usize,
	/// Placement of the already-placed cycle
	pub existing_placement: CyclePlacement,
	/// Index of the vertex that has already been placed at the only
	/// intersection between the cycles
	pub existing_vertex: usize,
	/// Position of the intersection (and the already-placed vertex)
	pub vertex_position: Vec2,
	/// Index of the vertex that the cycles share
	/// that could not be placed because the cycles only intersect once
	pub failing_vertex: usize,
}

/// Error data for [`LevelBuilderError::TooManyVerticesInCycleIntersection`]
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct TooManyVerticesInCycleIntersectionError {
	/// Index of the cycle whose attempted placement failed
	pub placed_cycle: usize,
	/// Index of the already-placed cycle that shared
	/// several vertices with the one being placed
	pub existing_cycle: usize,
	/// Indices of three of the vertices that are shared by the cycles
	pub shared_vertices: [usize; 3],
}

/// Error data for [`LevelBuilderError::OverlappedLinkedCycles`]
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct OverlappedLinkedCyclesError {
	/// Index of the cycle where a (possibly transitive) link starts
	pub source_cycle: usize,
	/// Index of the cycle where a link ends
	pub dest_cycle: usize,
	/// Index of the vertex shared by the two cycles
	pub shared_vertex: usize,
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
	/// A cycle has been explicitly assigned negative radius
	CycleRadiusNotPositive(usize, f32),
	/// [`build`](LevelBuilder::build) was called
	/// while a cycle had not been placed yet
	UnplacedCycle(usize),
	/// [`place_cycle`](LevelBuilder::place_cycle) was called
	/// on a cycle that had already been placed
	CycleAlreadyPlaced(usize),
	/// A vertex or cycle positioning operation was called
	/// in a way that would cause a vertex to not lie on its cycle.
	/// ## Causes
	/// - [`place_cycle`](LevelBuilder::place_cycle) called on a cycle
	///   that contains a vertex that already has fixed placement
	///   and it would not lie on the vertex
	/// - [`place_vertex`](LevelBuilder::place_vertex) called on a vertex
	///   that lies on a placed cycle with a position that does not lie
	///   on the cycle
	CycleDoesNotContainVertex(CycleDoesNotContainVertexError),
	/// Cycles that share a vertex have been placed in a way that they do not intersect
	CyclesDoNotIntersect(CyclesDoNotIntersectError),
	/// Cycles that share two vertices have been placed in a way that they
	/// only intersect tangentially (only enough space for one shared vertex)
	CyclesDoNotIntersectTwice(CyclesDoNotIntersectTwiceError),
	/// Two cycles share more than two vertices
	TooManyVerticesInCycleIntersection(TooManyVerticesInCycleIntersectionError),
	/// A vertex positioning operation was called on a vertex that
	/// already has a placement too specific to perform the operation
	/// ## Causes
	/// - [`place_vertex`](LevelBuilder::place_vertex) or
	///   [`place_vertex_at_angle`](LevelBuilder::place_vertex_at_angle)
	///   called on a fully placed vertex
	VertexAlreadyPlaced(usize),
	/// [`place_vertex_at_angle`](LevelBuilder::place_vertex) was called
	/// on a vertex that already has not yet been partially placed
	VertexNotPartiallyPlaced(usize),
	/// A placement operation was called in a way that would place vertices
	/// around a cycle out of their rotation order
	/// ## Causes
	/// - [`place_cycle`](LevelBuilder::place_cycle) called on a cycle that
	///   shares vertices with a cycle that is already placed, and their
	///   shared vertices (which would gain fixed placement by this operation)
	///   would be out-of-order on one of them
	/// - [`place_vertex`](LevelBuilder::place_vertex) or
	///   [`place_vertex_at_angle`](LevelBuilder::place_vertex_at_angle)
	///   called on a vertex on a placed cycle in a way that would place
	///   the target vertex out of the correct order
	VertexOrderViolationOnCycle(usize),
	/// Two cycles are linked, but they share a vertex
	OverlappedLinkedCycles(OverlappedLinkedCyclesError),
	/// There is a loop in cycle links, and their directions are contradicting
	CycleLinkageConflict(usize, usize),
	/// There is a loop in one-way cycle links
	OneWayLinkLoop,
}

#[derive(Clone, Copy, Debug)]
struct IntermediateVertexData {
	/// Position of the vertex, if it has been placed yet
	pub position: IntermediateVertexPosition,
	/// Object that lies on the vertex, if any
	pub object: Option<ObjectData>,
	/// Glyph that lies on the vertex, if any
	pub glyph: Option<GlyphData>,
	/// Appearence of the button color label,
	/// if it is bound to a parent cycle and set to be resolved at build time
	pub color_label_appearence: Option<CycleBoundButtonColorLabelAppearence>,
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
	pub linked_cycle: IntermediateLinkStatus,
	/// Outgoing oneways
	pub outgoing_one_way_links: Vec<OneWayIntermediateData>,
}

#[derive(Clone, Copy, Debug)]
enum IntermediateLinkStatus {
	None,
	Cycle(usize, LinkedCycleDirection),
	Group(usize, LinkedCycleDirection),
}

#[derive(Clone, Copy, Debug)]
struct OneWayIntermediateData {
	target_cycle: usize,
	direction: LinkedCycleDirection,
	// /// Information on whether this link interacts with detectors, if that information is known already
	// has_detectors: Option<bool>
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

/// Placeholder appearence for a button color label that will be
/// styled with regard to the cycle it is placed on
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
struct CycleBoundButtonColorLabelAppearence {
	/// Index of the cycle relative to which the label will be positioned
	owner_cycle: usize,
	/// Whether the labels should be oriented away from the cycle instead of towards it
	place_outside_cycle: bool,
	/// Whether the labels should be pentagon arrows instead of square
	has_arrow_tip: bool,
	/// Set of directions from which the final angle can be picked
	positions: CycleBoundColorLabelPositionSet,
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
			declared_one_way_links: Vec::new(),
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
			color_label_appearence: None,
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
			linked_cycle: IntermediateLinkStatus::None,
			outgoing_one_way_links: Vec::new(),
		});
		Ok(self.cycles.len() - 1)
	}

	pub fn set_object(
		&mut self,
		vertex_index: usize,
		object: ObjectData,
	) -> Result<(), LevelBuilderError> {
		if vertex_index >= self.vertices.len() {
			return Err(LevelBuilderError::VertexIndexOutOfRange(vertex_index));
		}
		self.vertices[vertex_index].object = Some(object);
		Ok(())
	}

	pub fn set_glyph(
		&mut self,
		vertex_index: usize,
		glyph: GlyphData,
	) -> Result<(), LevelBuilderError> {
		if vertex_index >= self.vertices.len() {
			return Err(LevelBuilderError::VertexIndexOutOfRange(vertex_index));
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
		self.add_cycle_link(source_cycle, dest_cycle, direction)?;
		self.declared_links.push(DeclaredLinkData {
			source_cycle,
			dest_cycle,
			direction,
		});
		Ok(())
	}

	/// Links two cycles by a one-way.
	pub fn one_way_link_cycles(
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
		if source_cycle == dest_cycle {
			// TODO: consider a better error.
			return Err(LevelBuilderError::CycleLinkageConflict(
				source_cycle,
				dest_cycle,
			));
		}
		self.cycles[source_cycle]
			.outgoing_one_way_links
			.push(OneWayIntermediateData {
				target_cycle: dest_cycle,
				direction,
			});
		self.declared_one_way_links.push(DeclaredLinkData {
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
		if radius.is_nan() || radius <= 0.0 {
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
	pub fn place_vertex_at_angle(
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

	/// Assigns a fixed placement to a vertex
	/// ## Parameters
	/// - `target_vertex` - Index of the vertex to place
	/// - `position` - Actual position where the vertex should be placed
	pub fn place_vertex(
		&mut self,
		target_vertex: usize,
		position: Vec2,
	) -> Result<(), LevelBuilderError> {
		if target_vertex >= self.vertices.len() {
			return Err(LevelBuilderError::VertexIndexOutOfRange(target_vertex));
		}
		match self.vertices[target_vertex].position {
			IntermediateVertexPosition::Fixed(_) => {
				Err(LevelBuilderError::VertexAlreadyPlaced(target_vertex))
			}
			IntermediateVertexPosition::Free => {
				self.vertices[target_vertex].position = IntermediateVertexPosition::Fixed(position);
				Ok(())
			}
			IntermediateVertexPosition::Partial(p) => {
				let owner_placement = self.cycles[p.owner_cycle]
					.placement
					.expect("Owner cycle of a partially placed vertex should also be placed");
				// The vertex being placed must lie on its owner cycle
				let distance_sq_from_center = owner_placement.position.distance_squared(position);
				if (distance_sq_from_center - owner_placement.radius.powi(2)).abs()
					> Self::PLACEMENT_VALIDATION_TOLERANCE
				{
					return Err(LevelBuilderError::CycleDoesNotContainVertex(
						CycleDoesNotContainVertexError {
							placed_cycle: p.owner_cycle,
							requested_placement: owner_placement,
							failing_vertex: target_vertex,
							vertex_position: position,
						},
					));
				}
				// Check that the placement does not violate the cycle order, then proceed
				if !self.verify_materialization_against_cycle(
					p.owner_cycle,
					std::iter::once((target_vertex, position)),
				) {
					return Err(LevelBuilderError::VertexOrderViolationOnCycle(
						p.owner_cycle,
					));
				}
				self.vertices[target_vertex].position = IntermediateVertexPosition::Fixed(position);
				Ok(())
			}
		}
	}

	pub fn set_color_label_appearences_for_cycle(
		&mut self,
		target_cycle: usize,
		position_set: CycleBoundColorLabelPositionSet,
		place_outside_cycle: bool,
		has_arrow_tip: bool,
	) -> Result<(), LevelBuilderError> {
		if target_cycle >= self.cycles.len() {
			return Err(LevelBuilderError::CycleIndexOutOfRange(target_cycle));
		}
		for target_vertex in &self.cycles[target_cycle].vertex_indices {
			let vertex = &mut self.vertices[*target_vertex];
			if matches!(vertex.glyph, Some(GlyphData::Button(Some(_)))) {
				vertex.color_label_appearence = Some(CycleBoundButtonColorLabelAppearence {
					owner_cycle: target_cycle,
					place_outside_cycle,
					has_arrow_tip,
					positions: position_set,
				})
			}
		}
		Ok(())
	}

	/// Checks that the level data is complete and assembles it
	pub fn build(mut self) -> Result<LevelData, LevelBuilderError> {
		let groups = self.compute_groups()?;
		let forbidden_group_pairs = self.compute_forbidden_groups()?;
		self.validate_before_build()?;
		self.materialize_partial_vertex_placements();
		self.apply_color_label_appearences_to_buttons();
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
			declared_one_way_links: self.declared_one_way_links,
			groups,
			forbidden_group_pairs,
		})
	}

	/// Tolerance in validation of manual placements
	const PLACEMENT_VALIDATION_TOLERANCE: f32 = 0.001;

	/// Computes and creates the GroupData objects
	/// Also assigns all cycles into their groups
	fn compute_groups(&mut self) -> Result<Vec<GroupData>, LevelBuilderError> {
		let mut groups = Vec::new();
		for cycle in self.cycles.iter_mut() {
			if let IntermediateLinkStatus::None = cycle.linked_cycle {
				groups.push(GroupData {
					cycles: Vec::new(),
					linked_groups: Vec::new(),
				});
				cycle.linked_cycle = IntermediateLinkStatus::Group(
					groups.len() - 1,
					LinkedCycleDirection::Coincident,
				);
			}
		}
		// Technically we could've done this in the previous loop,
		// but I do not trust either of the two doofi working on
		// this codebase to not break the invariant that
		// a cycle always points to a lower cycle or a group,
		// so I will not assume it.
		for cycle in 0..self.cycles.len() {
			let (root_cycle, direction_1) = self.find_group_root(cycle);
			let IntermediateLinkStatus::Group(group, direction_2) =
				self.cycles[root_cycle].linked_cycle
			else {
				panic!("Some doofus done doofed up the for loop above this one. (or the one inside find_group_root)");
			};
			let direction = direction_1 * direction_2;
			groups[group]
				.cycles
				.push((cycle, direction_1 * direction_2));
			self.cycles[cycle].linked_cycle = IntermediateLinkStatus::Group(group, direction);
		}
		// A third pass for aggregating group one-ways
		for source_cycle in 0..self.cycles.len() {
			let IntermediateLinkStatus::Group(source_group, direction_1) =
				self.cycles[source_cycle].linked_cycle
			else {
				panic!("Some doofus done doofed up the for loop above this one.");
			};
			for link in self.cycles[source_cycle].outgoing_one_way_links.iter() {
				let IntermediateLinkStatus::Group(target_group, direction_2) =
					self.cycles[link.target_cycle].linked_cycle
				else {
					panic!("Some doofus done doofed up the for loop above this one.");
				};
				groups[source_group].linked_groups.push(OneWayLinkData {
					source_cycle_data: Some(source_cycle),
					target_cycle_data: Some(link.target_cycle),
					target_group,
					direction: link.direction * direction_1 * direction_2,
					multiplicity: 1,
				});
			}
		}
		// TOPOLOGICAL SORT
		// THIS BLOCK INVALIDATES OR CHANGES ALL GROUP INDICES BY RESHUFFLING THE `groups` VECTOR.
		// TODO: use a better structure for the links between cycles, to deduplicate mutliple equivalent dependencies
		{
			let group_target_indices = {
				let mut sorted_order = vec![0usize; groups.len()];
				let mut next_order_index = 0;
				let mut sorted_mark = vec![false; groups.len()];
				let mut currently_visited_mark = vec![false; groups.len()];
				let mut stack = Vec::new();

				for i in 0..groups.len() {
					// Node is already in the ordering, skip it
					if sorted_mark[i] {
						continue;
					}
					stack.push(i);
					while !stack.is_empty() {
						let node = *stack.last().unwrap();
						// Node is already in the ordering, skip it
						// This removes duplicates from the stack
						if sorted_mark[node] {
							stack.pop();
							continue;
						}
						currently_visited_mark[node] = true;
						let mut blocked = false;
						for dependency in groups[node].linked_groups.iter() {
							let dependency_node = dependency.target_group;
							if currently_visited_mark[dependency_node] {
								// TODO: Better errors, but I really could not be bothered.
								return Err(LevelBuilderError::OneWayLinkLoop);
							}
							// If this node depends on nodes that have not yet been put into the topological ordering
							// We need to first handle those and block this node
							if !sorted_mark[dependency_node] {
								blocked = true;
								stack.push(dependency_node);
							}
						}
						if !blocked {
							// Remove our node (it has to be at the top because node is the last element)
							// And we have not pushed to the stack
							stack.pop();
							// Place the node into the ordering giving it the next available index
							sorted_mark[node] = true;
							currently_visited_mark[node] = false;
							sorted_order[node] = next_order_index;
							next_order_index += 1;
						}
					}
				}
				// Reverse the ordering, so sources come before targets
				for index in sorted_order.iter_mut() {
					*index = groups.len() - 1 - *index;
				}
				sorted_order
			};
			#[cfg(debug_assertions)]
			{
				// Check that every group index occurs exactly once
				let mut counter = vec![0; groups.len()];
				for index in group_target_indices.iter() {
					counter[*index] += 1;
				}
				assert_eq!(
					counter,
					vec![1; groups.len()],
					"Index map is not a bijection"
				);
			}
			// Update ***EVERY*** group index.

			// Update cycle -> group links
			for source_cycle in 0..self.cycles.len() {
				let IntermediateLinkStatus::Group(source_group, direction) =
					self.cycles[source_cycle].linked_cycle
				else {
					panic!("Some doofus done doofed up the for loop above this one.");
				};
				self.cycles[source_cycle].linked_cycle =
					IntermediateLinkStatus::Group(group_target_indices[source_group], direction);
			}

			// Update group -> group links
			for group in groups.iter_mut() {
				for link in group.linked_groups.iter_mut() {
					link.target_group = group_target_indices[link.target_group];
				}
			}

			// Shuffle groups
			let n_groups = groups.len();
			let old_groups = std::mem::replace(
				&mut groups,
				vec![
					GroupData {
						cycles: Vec::new(),
						linked_groups: Vec::new()
					};
					n_groups
				],
			);
			for (old_index, group) in old_groups.into_iter().enumerate() {
				let _ = std::mem::replace(&mut groups[group_target_indices[old_index]], group);
			}
			#[cfg(debug_assertions)]
			{
				// Check that every group is linked only to groups of higher index
				for (source_group_id, group) in groups.iter().enumerate() {
					for link in group.linked_groups.iter() {
						assert!(
							link.target_group > source_group_id,
							"Group {} links to a lower (or equal) index group {}",
							source_group_id,
							link.target_group
						);
					}
				}
			}
		}

		// Throw away cycle data on links that don't need it
		for group in groups.iter_mut() {
			for link in group.linked_groups.iter_mut() {
				// TODO: take into account detectors
				link.source_cycle_data = None;
				link.target_cycle_data = None;
			}
		}

		// Deduplicate link data whenever possible
		for group in groups.iter_mut() {
			if group.linked_groups.is_empty() {
				continue;
			}
			group.linked_groups.sort_by(OneWayLinkData::compare);
			let old_links = std::mem::take(&mut group.linked_groups);
			// At least one link must exist because we checked for the vector being empty earlier
			group.linked_groups.push(old_links[0]);
			for link in old_links.into_iter().skip(1) {
				match OneWayLinkData::try_merge(&link, group.linked_groups.last().unwrap()) {
					Some(new_link) => *group.linked_groups.last_mut().unwrap() = new_link,
					None => group.linked_groups.push(link),
				}
			}
		}
		Ok(groups)
	}

	/// Computes which pairs of groups can not be rotated in sync
	fn compute_forbidden_groups(&self) -> Result<Vec<(usize, usize)>, LevelBuilderError> {
		let mut forbid = Vec::new();
		// TODO: Use a better algorithm, like come on O(n^4) ???
		for cycle_a in 0..self.cycles.len() {
			for cycle_b in (cycle_a + 1)..self.cycles.len() {
				'inner: for &vertex_a in self.cycles[cycle_a].vertex_indices.iter() {
					for &vertex_b in self.cycles[cycle_b].vertex_indices.iter() {
						if vertex_a == vertex_b {
							let IntermediateLinkStatus::Group(group_a, _) =
								self.cycles[cycle_a].linked_cycle
							else {
								panic!("Cycle in build phase doesn't have a link pointer, should've been resolved in [`compute_groups`]");
							};
							let IntermediateLinkStatus::Group(group_b, _) =
								self.cycles[cycle_b].linked_cycle
							else {
								panic!("Cycle in build phase doesn't have a link pointer, should've been resolved in [`compute_groups`]");
							};
							if group_a == group_b {
								return Err(LevelBuilderError::OverlappedLinkedCycles(
									OverlappedLinkedCyclesError {
										dest_cycle: cycle_a,
										source_cycle: cycle_b,
										shared_vertex: vertex_a,
									},
								));
							}
							if group_a < group_b {
								forbid.push((group_a, group_b));
							} else {
								forbid.push((group_b, group_a));
							}
							break 'inner;
						}
					}
				}
			}
		}
		forbid.sort_by(|a, b| match a.0.cmp(&b.0) {
			std::cmp::Ordering::Equal => a.1.cmp(&b.1),
			ord => ord,
		});
		Ok(forbid)
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
		let IntermediateLinkStatus::Group(group, relative_direction) = intermediate.linked_cycle
		else {
			panic!("Cycle in build phase doesn't have a link pointer, should've been resolved in [`compute_groups`]");
		};
		CycleData {
			placement,
			vertex_indices: intermediate.vertex_indices,
			turnability: intermediate.turnability,
			group,
			orientation_within_group: relative_direction,
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

	/// Iterates through all vertices and applies their intermediate
	/// color label position to button objects on them if present.
	///
	/// This method expects all vertices to be materialized when it is called.
	fn apply_color_label_appearences_to_buttons(&mut self) {
		for vertex in &mut self.vertices {
			if let Some(GlyphData::Button(Some((_, appearence)))) = &mut vertex.glyph {
				if let Some(p) = vertex.color_label_appearence {
					let vertex_position = vertex.position.get_fixed()
						.expect("Color label appearences cannot be applied before all vertices that belong to a cycle are placed");
					let owner_cycle_position = self.cycles[p.owner_cycle].placement
						.expect("Color label appearences cannot be applied before all cycles are placed")
						.position;
					let angle_from_owner =
						-Vec2::Y.angle_to(vertex_position - owner_cycle_position);
					// Flip target angle if we want the labels inside the cycle
					let target_angle = if p.place_outside_cycle {
						angle_from_owner
					} else {
						angle_from_owner + PI
					}
					.rem_euclid(2.0 * PI);
					let position = match p.positions {
						CycleBoundColorLabelPositionSet::LeftRight => match target_angle / PI {
							0.0..1.0 => ButtonColorLabelPosition::AnglePlaced(PI * 0.5),
							1.0..=2.0 => ButtonColorLabelPosition::AnglePlaced(PI * 1.5),
							_ => unreachable!(),
						},
						CycleBoundColorLabelPositionSet::AboveBelow => match target_angle / PI {
							0.0..0.5 | 1.5..=2.0 => ButtonColorLabelPosition::AnglePlaced(0.0),
							0.5..1.5 => ButtonColorLabelPosition::AnglePlaced(PI),
							_ => unreachable!(),
						},
						CycleBoundColorLabelPositionSet::CardinalDirections => {
							match target_angle / PI {
								0.0..0.25 | 1.75..=2.0 => {
									ButtonColorLabelPosition::AnglePlaced(0.0)
								}
								0.25..0.75 => ButtonColorLabelPosition::AnglePlaced(PI * 0.5),
								0.75..1.25 => ButtonColorLabelPosition::AnglePlaced(PI),
								1.25..1.75 => ButtonColorLabelPosition::AnglePlaced(PI * 1.5),
								_ => unreachable!(),
							}
						}
						CycleBoundColorLabelPositionSet::AllDirections => {
							ButtonColorLabelPosition::AnglePlaced(target_angle)
						}
						CycleBoundColorLabelPositionSet::AllDirectionsRotated => {
							ButtonColorLabelPosition::AngleRotated(target_angle)
						}
					};
					*appearence = ButtonColorLabelAppearence {
						position,
						has_arrow_tip: p.has_arrow_tip,
					};
				}
			}
		}
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
					let mut segment_angle = next_relative_pos.angle_to(current_relative_pos);
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
				let mut segment_angle = first_relative_pos.angle_to(current_relative_pos);
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
				let mut angle = next.angle_to(current);
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

	/// Finds the root cycle (cycle that has no link to another parent) that this cycle's links point to and
	/// what is the relative direction of rotation, going through the links to there.
	fn find_group_root(&self, mut cycle: usize) -> (usize, LinkedCycleDirection) {
		let mut relative_direction = LinkedCycleDirection::Coincident;
		while let IntermediateLinkStatus::Cycle(lower, direction) = self.cycles[cycle].linked_cycle
		{
			// Todo: Optimise by shortening the path as we traverse it.
			cycle = lower;
			relative_direction *= direction;
		}
		(cycle, relative_direction)
	}

	/// Links two cycles asymmetrically (the link only goes from source to destination)
	/// The link is transitive.
	fn add_cycle_link(
		&mut self,
		cycle_a: usize,
		cycle_b: usize,
		direction: LinkedCycleDirection,
	) -> Result<(), LevelBuilderError> {
		let (cycle_1, rel_dir_a) = self.find_group_root(cycle_a);
		let (cycle_2, rel_dir_b) = self.find_group_root(cycle_b);
		let link_direction = direction * rel_dir_a * rel_dir_b;

		// There is no link to be created if we converged to the same node twice
		// However we must check that the relative direction is correct, otherwise we would be requiring that cycles
		// in the group turn opposite to their own direction.
		if cycle_1 == cycle_2 {
			return if link_direction != LinkedCycleDirection::Coincident {
				Err(LevelBuilderError::CycleLinkageConflict(cycle_a, cycle_b))
			} else {
				Ok(())
			};
		}

		// For niceness we always link higher numbers to lower numbers.
		let (source_cycle, target_cycle) = if cycle_1 > cycle_2 {
			(cycle_1, cycle_2)
		} else {
			(cycle_2, cycle_1)
		};
		// We have to take into account all the swaps that occured between
		self.cycles[source_cycle].linked_cycle =
			IntermediateLinkStatus::Cycle(target_cycle, link_direction);
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
			Self::CycleRadiusNotPositive(i, r) => write!(f, "Radius of cycle {i} is not positive ({r})"),
			Self::UnplacedCycle(i) => write!(f, "Cannot finish layout because cycle {i} has not yet been placed."),
			Self::CycleAlreadyPlaced(i) => write!(f, "Cannot place cycle {i} because it has already been placed."),
			Self::VertexAlreadyPlaced(i) => write!(f, "Cannot place vertex {i} because it has already been (possibly implicitly) placed."),
			Self::VertexNotPartiallyPlaced(i) => write!(f, "Cannot place vertex {i} because it does not lie on any placed cycle."),
			Self::CycleDoesNotContainVertex(e) => write!(
				f,
				"Placement is not valid because cycle {} placed at {} would not contain vertex {} placed at {}.",
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
			Self::OneWayLinkLoop => write!(f, "One way links cannot form a cycle.")
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
