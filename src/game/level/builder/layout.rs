use super::error::*;
use super::*;

use crate::graphics::{LEVEL_AREA_CENTER, LEVEL_AREA_WIDTH};
use std::f32::consts::PI;

use bevy::math::bounding::{Aabb2d, BoundingVolume};
use itertools::Itertools as _;

impl LevelBuilder {
	/// Tolerance in validation of manual placements
	const PLACEMENT_VALIDATION_TOLERANCE: f32 = 0.001;

	pub(super) fn build_layout(&mut self) {
		self.materialize_partial_vertex_placements();
		self.materialize_cycle_center_placements();
		self.apply_color_label_appearences_to_buttons();
		self.fit_to_viewport(Aabb2d::new(LEVEL_AREA_CENTER, LEVEL_AREA_WIDTH / 2.0));
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
			shape: CycleShape::Circle(radius),
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
					match owner_placement.shape {
						CycleShape::Circle(owner_radius) => {
							// Find one intersection of the two cycles, if it exists
							let intersection = intersect_circles(
								owner_placement.position,
								center,
								owner_radius,
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
									placements_after
										.push(IntermediateVertexPosition::Fixed(new_pos));
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
											let new_pos = if double_intersection_hints
												.contains(&val.first().0)
											{
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
											let new_pos = if double_intersection_hints.contains(&i)
											{
												alt_pos
											} else {
												default_pos
											};
											e.insert(One((i, new_pos)));
											new_pos
										}
									};
									placements_after
										.push(IntermediateVertexPosition::Fixed(new_pos));
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

	/// Explicitly sets the position of a cycle's center indicator
	/// or makes it invisible
	pub fn place_cycle_center(
		&mut self,
		target_cycle: usize,
		position: Option<Vec2>,
	) -> Result<(), LevelBuilderError> {
		if target_cycle >= self.cycles.len() {
			return Err(LevelBuilderError::CycleIndexOutOfRange(target_cycle));
		}
		self.cycles[target_cycle].center_sprite_position = Some(position);
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
				match owner_placement.shape {
					CycleShape::Circle(owner_radius) => {
						// Recalculate from clock angle to angle of the actual vertor space
						let real_angle = PI / 2.0 - clock_angle;
						let new_pos =
							owner_placement.position + owner_radius * Vec2::from_angle(real_angle);
						if !self.verify_materialization_against_cycle(
							p.owner_cycle,
							std::iter::once((target_vertex, new_pos)),
						) {
							return Err(LevelBuilderError::VertexOrderViolationOnCycle(
								p.owner_cycle,
							));
						}
						self.vertices[target_vertex].position =
							IntermediateVertexPosition::Fixed(new_pos);
						Ok(())
					}
				}
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
				match owner_placement.shape {
					CycleShape::Circle(owner_radius) => {
						// The vertex being placed must lie on its owner cycle
						let distance_sq_from_center =
							owner_placement.position.distance_squared(position);
						if (distance_sq_from_center - owner_radius.powi(2)).abs()
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
						self.vertices[target_vertex].position =
							IntermediateVertexPosition::Fixed(position);
						Ok(())
					}
				}
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

	/// Iterates through all vertices and applies their intermediate
	/// color label position to button objects on them if present.
	///
	/// This method expects all vertices to be materialized when it is called.
	fn apply_color_label_appearences_to_buttons(&mut self) {
		for vertex in &mut self.vertices {
			if let Some(GlyphData::Button(Some((_, appearence)))) = &mut vertex.glyph {
				if let Some(p) = vertex.color_label_appearence {
					let Some(vertex_position) = vertex.position.get_fixed() else {
						warn!("Color label appearences cannot be applied before all vertices that belong to a cycle are placed");
						continue;
					};
					let Some(placement) = self.cycles[p.owner_cycle].placement else {
						warn!("Color label appearences cannot be applied before all cycles are placed");
						continue;
					};
					let owner_cycle_position = placement.position;
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
						CycleBoundColorLabelPositionSet::LeftRight => {
							ButtonColorLabelPosition::AnglePlaced(match target_angle / PI {
								0.0..1.0 => PI * 0.5,
								1.0..=2.0 => PI * 1.5,
								// Theoretically unreachable but just in case (because floats) we use a default value
								_ => PI * 0.5,
							})
						}
						CycleBoundColorLabelPositionSet::AboveBelow => {
							ButtonColorLabelPosition::AnglePlaced(match target_angle / PI {
								0.0..0.5 | 1.5..=2.0 => 0.0,
								0.5..1.5 => PI,
								// Theoretically unreachable but just in case (because floats) we use a default value
								_ => 0.0,
							})
						}
						CycleBoundColorLabelPositionSet::CardinalDirections => {
							ButtonColorLabelPosition::AnglePlaced(match target_angle / PI {
								0.0..0.25 | 1.75..=2.0 => 0.0,
								0.25..0.75 => PI * 0.5,
								0.75..1.25 => PI,
								1.25..1.75 => PI * 1.5,
								// Theoretically unreachable but just in case (because floats) we use a default value
								_ => 0.0,
							})
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
			match cycle_placement.shape {
				CycleShape::Circle(cycle_radius) => {
					if let Some((first_fixed_vertex, first_fixed_pos)) = fixed_vertices.next() {
						let first_relative_pos = first_fixed_pos - cycle_placement.position;
						let mut current_fixed_vertex = first_fixed_vertex;
						let mut current_relative_pos = first_relative_pos;
						for (next_fixed_vertex, next_fixed_pos) in fixed_vertices {
							// Materialize all vertices between the marked ones
							let next_relative_pos = next_fixed_pos - cycle_placement.position;
							let vertex_count = next_fixed_vertex - current_fixed_vertex;
							let mut segment_angle =
								next_relative_pos.angle_to(current_relative_pos);
							if segment_angle <= 0.0 {
								segment_angle += 2.0 * PI;
							}
							// Distribute the partially-placed vertices uniformly between the fixed ones
							for (j, i) in
								((current_fixed_vertex + 1)..next_fixed_vertex).enumerate()
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
							// Move to the next segment
							current_fixed_vertex = next_fixed_vertex;
							current_relative_pos = next_relative_pos;
						}
						// Close the loop; materialize the segment between last and first fixed vertex
						// Mind the special case when exactly one vertex is fixed
						// and this segment covers the whole cycle
						let vertex_count = first_fixed_vertex + cycle_data.vertex_indices.len()
							- current_fixed_vertex;
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
								+ cycle_radius
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
		}
	}

	/// Iterates through all cycles and sets their center indicator placements
	/// to a default position for their placement shape if they have not
	/// been placed explicitly
	///
	/// All vertices that are a part of a cycle must have a fixed placement by now
	fn materialize_cycle_center_placements(&mut self) {
		for cycle_data in &mut self.cycles {
			let Some(placement) = cycle_data.placement else {
				continue;
			};
			if cycle_data.center_sprite_position.is_none() {
				// If center sprite has not been explicitly positioned
				// or disabled, put it in the cycle center
				cycle_data.center_sprite_position = Some(Some(placement.position));
			}
		}
	}

	/// Sets a vertex to a fixed placement and checks that it belonged to a particular cycle
	/// ## Panics
	/// May panic if the vertex was not previously in [`Partial`](IntermediateVertexPosition::Partial)
	/// placement owned by the specified cycle
	fn checked_materialize(
		pos: &mut IntermediateVertexPosition,
		new_pos: Vec2,
		owner_cycle: usize,
		index_in_owner: usize,
	) {
		#[cfg(any(test, debug_assertions))]
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
	/// - If the cycle is not placed, it will default to an invalid placement
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
		let Some(cycle_placement) = self.cycles[cycle_index].placement else {
			warn!("Partial materialization checks can only be run on placed cycles");
			return false;
		};
		// Positions of all checked vertices, in cyclic order
		// Vertices that were already fixed-placed are included
		// Vertices that are in points_to_materialize are also included
		// Other vertices are ignored
		let fixed_points = self.cycles[cycle_index]
			.vertex_indices
			.iter()
			.filter_map(|&i| {
				points_to_materialize
					.clone()
					.find_map(|(j, p)| if i == j { Some(p) } else { None })
					.or_else(|| self.vertices[i].position.get_fixed())
			});
		// What constitues cycle order depends on the shape of the cycle
		match cycle_placement.shape {
			CycleShape::Circle(_) => {
				Self::are_points_in_cyclic_order(cycle_placement.position, fixed_points)
			}
		}
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
	/// Calculates the bounding box of all currently placed cycles and their center sprites
	fn get_bounding_box(&self) -> Aabb2d {
		let min = self
			.cycles
			.iter()
			.map(|cycle| {
				cycle
					.placement
					.map(|p| Self::get_bounding_box_for_cycle(p).min)
					.unwrap_or(Vec2::INFINITY)
					.min(
						cycle
							.center_sprite_position
							.and_then(|x| x)
							.unwrap_or(Vec2::INFINITY),
					)
			})
			.fold(Vec2::INFINITY, Vec2::min);
		let max = self
			.cycles
			.iter()
			.map(|cycle| {
				cycle
					.placement
					.map(|p| Self::get_bounding_box_for_cycle(p).max)
					.unwrap_or(Vec2::NEG_INFINITY)
					.max(
						cycle
							.center_sprite_position
							.and_then(|x| x)
							.unwrap_or(Vec2::NEG_INFINITY),
					)
			})
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

	/// Computes the bounding box for a given cycle shape
	fn get_bounding_box_for_cycle(placement: CyclePlacement) -> Aabb2d {
		match placement.shape {
			CycleShape::Circle(radius) => Aabb2d::new(placement.position, Vec2::splat(radius)),
		}
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
				match &mut p.shape {
					CycleShape::Circle(radius) => *radius *= scale,
				}
			}
			if let Some(Some(p)) = &mut cycle.center_sprite_position {
				*p = (*p - bounds_center) * scale + viewport_center;
			}
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
