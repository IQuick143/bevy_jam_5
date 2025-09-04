use super::error::*;
use super::*;

use crate::graphics::{LEVEL_AREA_CENTER, LEVEL_AREA_WIDTH};
use std::f32::consts::PI;

use bevy::math::bounding::{Aabb2d, BoundingVolume};
use itertools::Itertools as _;
use smallvec::SmallVec;

impl LevelBuilder {
	/// Tolerance in validation of manual placements
	pub const PLACEMENT_VALIDATION_TOLERANCE: f32 = 0.001;

	pub(super) fn build_layout(&mut self) {
		self.solve_vertex_placements();
		self.materialize_cycle_center_placements();
		self.apply_color_label_appearences_to_buttons();
		self.fit_to_viewport(Aabb2d::new(LEVEL_AREA_CENTER, LEVEL_AREA_WIDTH / 2.0));
	}

	pub fn place_circle(
		&mut self,
		target_cycle: usize,
		center: Vec2,
		radius: f32,
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
		self.cycles[target_cycle].placement = Some(CyclePlacement {
			position: center,
			shape: CycleShape::Circle(radius),
		});
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

	/// Sets the scale override to the level.
	/// Argument is a float converting epilang units to world units
	pub fn set_level_scale(&mut self, mut scale: f32) {
		if !scale.is_finite() {
			scale = 1.0;
		}
		self.scale_override = Some(scale.max(0.0));
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
						log::warn!("Color label appearences cannot be applied before all vertices that belong to a cycle are placed");
						continue;
					};
					let Some(placement) = self.cycles[p.owner_cycle].placement else {
						log::warn!("Color label appearences cannot be applied before all cycles are placed");
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

	/// Computes the placements of unplaced vertices based on the geometric constraints or gives up.
	fn solve_vertex_placements(&mut self) {
		let (vertex_to_cycle, absolute_precision_limit) = {
			let mut absolute_precision_limit = f32::EPSILON;
			let mut vertex_to_cycle: Vec<SmallVec<[usize; 2]>> =
				vec![SmallVec::new(); self.vertices.len()];
			for (id, cycle) in self.cycles.iter().enumerate() {
				for &vertex in cycle.vertex_indices.iter() {
					debug_assert!(
						vertex < self.vertices.len(),
						"Cycle contains vertex that is out of range."
					);
					vertex_to_cycle[vertex].push(id);
				}

				match cycle.placement {
					Some(CyclePlacement { position, shape }) => {
						let size = match shape {
							CycleShape::Circle(radius) => radius.abs(),
						};
						let distance = position.length();
						absolute_precision_limit = absolute_precision_limit.max(distance + size);
					}
					None => {}
				}
			}
			for (vertex_id, vert) in vertex_to_cycle.iter().enumerate() {
				debug_assert!(!vert.is_empty(), "Vertex {vertex_id} has no cycles!");
			}
			(
				vertex_to_cycle,
				absolute_precision_limit * Self::PLACEMENT_VALIDATION_TOLERANCE,
			)
		};
		// TODO: Verify that fixed vertices are placed on the cycles they belong to
		// TODO: End early if all vertices are fixed

		struct AdditionalCycleData {
			problematic_cycles: Vec<usize>,
			points_on_cycle: Vec<Vec<usize>>,
		}

		struct PointData {
			points: Vec<Vec2>,
			vertices_interested_in_point: Vec<SmallVec<[usize; 1]>>,
			vertex_constraints: Vec<IntersectionPointSet<usize>>,
		}

		enum VertexSolverError {
			VertexHasNoPointsAvailable { vertex: usize },
			TwoVerticesCollide { vertex_a: usize, vertex_b: usize },
		}

		impl PointData {
			/// Goes through vertices associated with the given point, if there is a vertex associated to it via `Single`
			fn propagate_constraint(&mut self, point: usize) -> Result<(), VertexSolverError> {
				let mut stack = vec![point];
				while let Some(point) = stack.pop() {
					let mut owner_vertex = None;
					for &vertex in self.vertices_interested_in_point[point].iter() {
						match self.vertex_constraints[vertex] {
							IntersectionPointSet::Single(_) => match owner_vertex {
								Some(vertex_b) => {
									return Err(VertexSolverError::TwoVerticesCollide {
										vertex_a: vertex,
										vertex_b,
									})
								}
								None => owner_vertex = Some(vertex),
							},
							_ => {
								continue;
							}
						}
					}
					if let Some(owner_vertex) = owner_vertex {
						for vertex in self.vertices_interested_in_point[point].clone() {
							if vertex == owner_vertex {
								continue;
							}
							match self.vertex_constraints[vertex] {
								IntersectionPointSet::Pair(a, b) => {
									let other_point = match (a == point, b == point) {
										(true, true) => todo!(), // shouldn't happen
										(true, false) => b,
										(false, true) => a,
										(false, false) => todo!(), // shouldn't happen either
									};
									self.place_vertex_internal(vertex, other_point);
									stack.push(other_point);
								}
								_ => {
									continue;
								}
							}
						}
					}
				}
				Ok(())
			}

			fn propagate_twin_constraint(
				&mut self,
				vertex_1: usize,
				vertex_2: usize,
			) -> Result<(), VertexSolverError> {
				match (
					self.vertex_constraints[vertex_1],
					self.vertex_constraints[vertex_2],
				) {
					(IntersectionPointSet::Pair(a1, b1), IntersectionPointSet::Pair(a2, b2)) => {
						if (a1 != b1) && ((a1 == a2 && b1 == b2) || (a1 == b2 && b1 == a2)) {
							let mut evicted_vertices: Vec<usize> = self
								.vertices_interested_in_point[a1]
								.iter()
								.chain(self.vertices_interested_in_point[b1].iter())
								.copied()
								.filter(|&vertex| vertex != vertex_1 && vertex != vertex_2)
								.collect();
							evicted_vertices.sort();
							evicted_vertices.dedup();
							for &vertex in evicted_vertices.iter() {
								match self.vertex_constraints[vertex] {
									IntersectionPointSet::Pair(option_a, option_b) => {
										match (
											option_a == a1 || option_a == b1,
											option_b == a1 || option_b == b1,
										) {
											(true, true) => {
												debug_assert!(false, "`propagate_twin_constraint` encountered vertex {vertex} interested in a point ({a1} or {b1}) which it actually wasn't. Desired points: ({option_a}, {option_b}).");
											}
											(true, false) => {
												self.place_vertex(vertex, option_b)?;
											}
											(false, true) => {
												self.place_vertex(vertex, option_a)?;
											}
											(false, false) => {
												todo!();
											}
										}
									}
									IntersectionPointSet::Single(option) => {
										if option == a1 || option == b1 {
											todo!();
										}
									}
									_ => {
										// Also shouldn't happen lol
										debug_assert!(false, "`propagate_twin_constraint` encountered vertex {vertex} interested in points which weren't actually constrained to points");
									}
								}
							}
						} else {
							// shouldn't happen either
							debug_assert!(false, "`propagate_twin_constraint` called on a twin pair that is not actually a twin pair v1:({vertex_1} -> {a1},{b1}) v2:({vertex_2} -> {a2},{b2})");
						}
					}
					_ => {
						// this shouldn't really happen
						debug_assert!(false, "`propagate_twin_constraint` called on a twin pair that is not actually a twin pair v1:({vertex_1}) v2:({vertex_2})");
					}
				}
				Ok(())
			}

			fn place_vertex(
				&mut self,
				vertex: usize,
				point: usize,
			) -> Result<(), VertexSolverError> {
				self.place_vertex_internal(vertex, point);
				self.propagate_constraint(point)
			}

			fn place_vertex_internal(&mut self, vertex: usize, point: usize) {
				match self.vertex_constraints[vertex] {
					IntersectionPointSet::Pair(a, b) => {
						self.remove_interest(vertex, a);
						self.remove_interest(vertex, b);
					}
					IntersectionPointSet::Single(a) => self.remove_interest(vertex, a),
					_ => {}
				}
				self.vertex_constraints[vertex] = IntersectionPointSet::Single(point);
				self.vertices_interested_in_point[point].push(vertex);
			}

			fn remove_interest(&mut self, vertex: usize, point: usize) {
				if let Some(index) = self.vertices_interested_in_point[point]
					.iter()
					.position(|&x| x == vertex)
				{
					self.vertices_interested_in_point[point].remove(index);
				}
			}

			/// Finds a "twin" vertex for a given `Pair` vertex, that is
			/// a second `Pair` vertex which has the same target points
			fn find_twin(&self, vertex: usize) -> Option<usize> {
				match self.vertex_constraints[vertex] {
					IntersectionPointSet::Pair(point_a, point_b) => {
						for &potential_vertex in self.vertices_interested_in_point[point_a]
							.iter()
							.filter(|&&potential_vertex| {
								potential_vertex != vertex
									&& self.vertices_interested_in_point[point_b]
										.contains(&potential_vertex)
							}) {
							match self.vertex_constraints[potential_vertex] {
								IntersectionPointSet::Pair(point_a2, point_b2) => {
									if (point_a == point_a2 && point_b == point_b2)
										|| (point_a == point_b2 && point_b == point_a2)
									{
										return Some(potential_vertex);
									}
								}
								_ => {}
							}
						}
						None
					}
					_ => None,
				}
			}
		}

		// Determine geometric constraints and place uniquely determined points.
		// Find cycles that have ambiguous points.
		// Points are deduplicated and indexed, facilitating equality comparisons.
		let (mut point_data, cycle_data): (PointData, AdditionalCycleData) = {
			// Array holding all the cycles which require
			let mut problematic_cycles: Vec<usize> = Vec::new();
			// Array holding all the points under consideration. Only points from cycles in [`problematic_cycles`] are guaranteed to be present.
			let mut points: Vec<Vec2> = Vec::new();
			// Array of links from point to a list of vertices who point to this point.
			let mut vertices_interested_in_point: Vec<SmallVec<[usize; 1]>> = Vec::new();
			// Mapping from cycles to list of indices to [`points`] representing the points, only initialised for cycles in [`problematic_cycles`].
			let mut points_on_cycle: Vec<Vec<usize>> = vec![Vec::new(); self.cycles.len()];
			// Array storing the geometric constraints placed on a vertex.
			let mut vertex_constraints: Vec<IntersectionPointSet<usize>> =
				vec![IntersectionPointSet::Unconstrained; self.vertices.len()];

			let insert_point_if_needed =
				|points: &mut Vec<Vec2>,
				 vertices_interested_in_point: &mut Vec<SmallVec<[usize; 1]>>,
				 points_on_cycle: &Vec<Vec<usize>>,
				 point: Vec2,
				 vertex: usize,
				 parent_cycle: usize|
				 -> usize {
					for &id in points_on_cycle[parent_cycle].iter() {
						if approx_eq_points(point, points[id], absolute_precision_limit) {
							vertices_interested_in_point[id].push(vertex);
							return id;
						}
					}
					vertices_interested_in_point.push(SmallVec::from_buf([vertex]));
					points.push(point);
					points.len() - 1
				};

			// TODO: Check that there is no [`IntermediateVertexPosition::Empty`], indicating an overconstrained vertex.
			// TODO: Check that there is no [`IntermediateVertexPosition::Unconstrained`], indicating a vertex that requires being placed manually but wasn't.
			for (vertex, cycles) in vertex_to_cycle.iter().enumerate() {
				let parent_cycle = *cycles
					.first()
					.expect("Vertex needs at least one parent cycle");
				vertex_constraints[vertex] = match self.vertices[vertex].position {
					IntermediateVertexPosition::Fixed(position) => {
						// TODO: come up with a way to avoid deduplicating these points maybe
						let point = insert_point_if_needed(
							&mut points,
							&mut vertices_interested_in_point,
							&points_on_cycle,
							position,
							vertex,
							parent_cycle,
						);
						for &cycle in cycles.iter() {
							points_on_cycle[cycle].push(point);
						}
						IntersectionPointSet::Single(point)
					}
					IntermediateVertexPosition::Free => {
						let intersection = self.compute_intersection(&cycles);
						match intersection {
							IntersectionPointSet::Unconstrained => todo!(), // Bad
							IntersectionPointSet::Cycle(cycle) => {
								IntersectionPointSet::Cycle(cycle)
							}
							IntersectionPointSet::Pair(point_a, point_b) => {
								// TODO: Use hints (if available) to decide here
								let point_a = insert_point_if_needed(
									&mut points,
									&mut vertices_interested_in_point,
									&points_on_cycle,
									point_a,
									vertex,
									parent_cycle,
								);
								let point_b = insert_point_if_needed(
									&mut points,
									&mut vertices_interested_in_point,
									&points_on_cycle,
									point_b,
									vertex,
									parent_cycle,
								);
								if point_a != point_b {
									for &cycle in cycles.iter() {
										problematic_cycles.push(cycle);
										points_on_cycle[cycle].push(point_a);
										points_on_cycle[cycle].push(point_b);
									}
									IntersectionPointSet::Pair(point_a, point_b)
								} else {
									// This is a stupid branch, but it's possible if you got a really degenerate thing going on.
									for &cycle in cycles.iter() {
										problematic_cycles.push(cycle);
										points_on_cycle[cycle].push(point_a);
									}
									IntersectionPointSet::Single(point_a)
								}
							}
							IntersectionPointSet::Single(point) => {
								let point = insert_point_if_needed(
									&mut points,
									&mut vertices_interested_in_point,
									&points_on_cycle,
									point,
									vertex,
									parent_cycle,
								);
								for &cycle in cycles.iter() {
									points_on_cycle[cycle].push(point);
								}
								IntersectionPointSet::Single(point)
							}
							IntersectionPointSet::Empty => todo!(), // Bad
						}
					}
				}
			}
			problematic_cycles.sort();
			problematic_cycles.dedup();
			for cycle_points in points_on_cycle.iter_mut() {
				cycle_points.sort();
				cycle_points.dedup();
			}
			for point_vertices in vertices_interested_in_point.iter_mut() {
				point_vertices.sort();
				point_vertices.dedup();
			}
			(
				PointData {
					points,
					vertices_interested_in_point,
					vertex_constraints,
				},
				AdditionalCycleData {
					problematic_cycles,
					points_on_cycle,
				},
			)
		};

		// println!("{:?}", point_data.points);
		// println!("{:?}", point_data.vertex_constraints);

		// Attempt to solve `Pair` vertices
		// TODO: Optimise
		{
			// Propagate constraints caused by `Single` points already placed
			for &cycle_id in cycle_data.problematic_cycles.iter() {
				for &point in cycle_data.points_on_cycle[cycle_id].iter() {
					point_data.propagate_constraint(point);
				}
			}
			// Propagate constraints caused by two twin vertices occupying two points for themselves (even if it's unknown which is which)
			for &cycle_id in cycle_data.problematic_cycles.iter() {
				for &vertex in self.cycles[cycle_id].vertex_indices.iter() {
					if let Some(twin_vertex) = point_data.find_twin(vertex) {
						point_data.propagate_twin_constraint(vertex, twin_vertex);
					}
				}
			}
			// Two iterations to let constraints propagate along a cycle, but complete propagation along chains of cycles is not guaranteed
			for _ in 0..2 {
				// Use simple deductions to place remaining
				for &cycle_id in cycle_data.problematic_cycles.iter() {
					let cycle = &self.cycles[cycle_id];
					match cycle.placement {
						Some(CyclePlacement {
							position: cycle_position,
							shape: CycleShape::Circle(_),
						}) => {
							// The closest "previous" point that is known to be fixed and its order in cycle
							let mut prev_point_index: (usize, usize) = {
								let mut prev_point_index: Option<(usize, usize)> = None;
								for (position_in_cycle, &vertex) in
									cycle.vertex_indices.iter().enumerate().rev()
								{
									match point_data.vertex_constraints[vertex] {
										IntersectionPointSet::Single(point) => {
											prev_point_index = Some((point, position_in_cycle));
											break;
										}
										_ => {}
									}
								}
								if let Some(prev_point_index) = prev_point_index {
									prev_point_index
								} else {
									// There are no `Single`s available, there's no reason to bother
									continue;
								}
							};
							// The closest "next" point that is known to be fixed and its order in cycle
							let mut next_point_index: Option<(usize, usize)> = None;
							// The first fixed point found
							let mut first_point: Option<(usize, usize)> = None;
							let mut end_reached_while_looking_for_next_point = false;
							for (position_in_cycle, &vertex) in
								cycle.vertex_indices.iter().enumerate()
							{
								match point_data.vertex_constraints[vertex] {
									IntersectionPointSet::Pair(point_a, point_b) => {
										if !end_reached_while_looking_for_next_point
											&& !next_point_index.is_some_and(
												|(_point, position)| position > position_in_cycle,
											) {
											next_point_index = None;
											for (next_position_in_cycle, &next_vertex) in cycle
												.vertex_indices
												.iter()
												.enumerate()
												.skip(position_in_cycle)
											{
												match point_data.vertex_constraints[next_vertex] {
													IntersectionPointSet::Single(point) => {
														next_point_index =
															Some((point, next_position_in_cycle));
														break;
													}
													_ => {}
												}
											}

											// If nothing is found, loop around and use the first point
											if next_point_index.is_none() {
												next_point_index = first_point;
												// No sense in looking next time if the whole range was traversed already
												end_reached_while_looking_for_next_point = true;
											}
										}
										let (prev_point, prev_position_in_cycle) = prev_point_index;
										// Two constraints are available
										if !if let Some((next_point, next_position_in_cycle)) =
											next_point_index.filter(
												|&(point, next_position_in_cycle)| {
													point != prev_point
														&& next_position_in_cycle
															!= prev_position_in_cycle
												},
											) {
											let clockwiseness = test_index_clockwiseness(
												prev_position_in_cycle,
												position_in_cycle,
												next_position_in_cycle,
											);
											let sign = if clockwiseness { 1.0 } else { -1.0 };

											let score_a = test_point_clockwiseness(
												point_data.points[prev_point],
												point_data.points[point_a],
												point_data.points[next_point],
												Some(cycle_position),
											) * sign;
											let score_b = test_point_clockwiseness(
												point_data.points[prev_point],
												point_data.points[point_b],
												point_data.points[next_point],
												Some(cycle_position),
											) * sign;
											let mut point_a_valid = score_a > 0.0;
											let mut point_b_valid = score_b > 0.0;
											// if one of the results is weirdly small, but not the other one,
											// use the non-tiny one
											if point_a_valid && point_b_valid {
												const THRESHOLD: f32 = 0.001;
												if score_a < score_b * THRESHOLD {
													point_a_valid = false;
												} else if score_b < score_a * THRESHOLD {
													point_b_valid = false;
												}
											}
											match (point_a_valid, point_b_valid) {
												(true, true) => {
													/* Tough luck, nothing got done here */
													false
												}
												(true, false) => {
													point_data.place_vertex(vertex, point_a);
													true
												}
												(false, true) => {
													point_data.place_vertex(vertex, point_b);
													true
												}
												(false, false) => {
													todo!(
														"Raise an error, vertex has nowhere to go"
													);
												}
											}
										} else {
											false
										} {
											if let Some((
												twin_vertex_position_in_cycle,
												twin_vertex,
											)) = point_data.find_twin(vertex).and_then(
												|twin_vertex| {
													cycle
														.vertex_indices
														.iter()
														.copied()
														.enumerate()
														.skip(position_in_cycle)
														.find(|&(_order_in_cycle, vertex)| {
															vertex == twin_vertex
														})
												},
											) {
												let clockwiseness = test_index_clockwiseness(
													prev_position_in_cycle,
													position_in_cycle,
													twin_vertex_position_in_cycle,
												);
												let point_clockwiseness = test_point_clockwiseness(
													point_data.points[prev_point],
													point_data.points[point_a],
													point_data.points[point_b],
													Some(cycle_position),
												);
												if clockwiseness == (point_clockwiseness > 0.0) {
													point_data.place_vertex(vertex, point_a);
													debug_assert!(matches!(point_data.vertex_constraints[twin_vertex], IntersectionPointSet::Single(_)), "Vertex propagation should've filled this in.");
												} else {
													point_data.place_vertex(vertex, point_b);
													debug_assert!(matches!(point_data.vertex_constraints[twin_vertex], IntersectionPointSet::Single(_)), "Vertex propagation should've filled this in.");
												}
											}
										}
									}
									_ => { /* dont care */ }
								}

								match point_data.vertex_constraints[vertex] {
									IntersectionPointSet::Single(point) => {
										prev_point_index = (point, position_in_cycle);
										if first_point.is_none() {
											first_point = Some((point, position_in_cycle));
										}
									}
									_ => { /* dont care */ }
								}
							}
						}
						None => continue, // This shouldn't really happen
					}
				}
			}
		}

		// println!("{:?}", point_data.vertex_constraints);

		// Convert `Single` placements into `Fixed` values
		for (vertex_id, vertex_placement) in self.vertices.iter_mut().enumerate() {
			match point_data.vertex_constraints[vertex_id] {
				IntersectionPointSet::Single(point) => {
					vertex_placement.position =
						IntermediateVertexPosition::Fixed(point_data.points[point])
				}
				IntersectionPointSet::Unconstrained => todo!(),
				IntersectionPointSet::Cycle(_) => {}
				IntersectionPointSet::Pair(_, _) => todo!(),
				IntersectionPointSet::Empty => todo!(),
			}
		}

		// Place all vertices that have a `Cycle` constraint
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

		for (_cycle_index, cycle_placement, cycle_data) in cycles {
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
								self.vertices[target_vertex].position =
									IntermediateVertexPosition::Fixed(new_pos);
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
							self.vertices[target_vertex].position =
								IntermediateVertexPosition::Fixed(new_pos);
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
							self.vertices[j].position = IntermediateVertexPosition::Fixed(new_pos);
						}
					}
				}
			}
		}

		// Verify solution
		for vertex in self.vertices.iter() {
			debug_assert!(
				matches!(vertex.position, IntermediateVertexPosition::Fixed(_)),
				"Unplaced vertex!"
			);
		}

		for cycle in self.cycles.iter() {
			for [(pos_1, vert_1), (pos_2, vert_2), (pos_3, vert_3)] in cycle
				.vertex_indices
				.iter()
				.copied()
				.map(|vertex| self.vertices[vertex].position.get_fixed().unwrap())
				.enumerate()
				.array_combinations::<3>()
			{
				debug_assert!(
					test_index_clockwiseness(pos_1, pos_2, pos_3)
						== (test_point_clockwiseness(vert_1, vert_2, vert_3, None) > 0.0),
					"Vertices out of order"
				);
			}
		}

		for (cycle_id, cycle) in self.cycles.iter().enumerate() {
			let position = cycle.placement.unwrap().position;
			let CycleShape::Circle(radius) = cycle.placement.unwrap().shape;
			for [(pos_1, vert_1), (pos_2, vert_2), (pos_3, vert_3)] in cycle
				.vertex_indices
				.iter()
				.copied()
				.map(|vertex| self.vertices[vertex].position.get_fixed().unwrap())
				.enumerate()
				.array_combinations::<3>()
			{
				debug_assert!(
					test_index_clockwiseness(pos_1, pos_2, pos_3)
						== (test_point_clockwiseness(vert_1, vert_2, vert_3, Some(position)) > 0.0),
					"Vertices out of order"
				);
			}

			for (vertex, point) in cycle
				.vertex_indices
				.iter()
				.map(|&vertex| (vertex, self.vertices[vertex].position.get_fixed().unwrap()))
			{
				debug_assert!(
					point_lies_on_circle(position, radius, point),
					"Vertex {vertex} with position {point} does not lie on cycle {cycle_id} with position {position} and radius {radius}"
				);
			}
		}
	}

	/// Computes the intersection of a set of cycles, taking into account only cycles which are
	/// 1) Circles
	/// 2) Lines
	/// skipping cycles that are other geometries, those should not be used in intersection tests.
	///
	/// # Panics
	/// May panic or give wrong result if
	/// 1) any of the cycle indices are not valid indices into [`LevelBuilder::cycles`]
	/// 2) if any of the provided cycles are unplaced.
	fn compute_intersection(&self, cycles: &[usize]) -> IntersectionPointSet<Vec2> {
		let mut intersection = IntersectionPointSet::Unconstrained;
		for &cycle in cycles.iter() {
			intersection = match (self.cycles[cycle].placement, intersection) {
				(_, IntersectionPointSet::Empty) => {
					return IntersectionPointSet::Empty;
				}
				(_, IntersectionPointSet::Unconstrained) => IntersectionPointSet::Cycle(cycle),
				(None, _) => {
					debug_assert!(false, "Unplaced cycle in `compute_intersection`");
					return IntersectionPointSet::Empty;
				}
				(
					Some(CyclePlacement {
						position,
						shape: CycleShape::Circle(radius),
					}),
					IntersectionPointSet::Single(point),
				) => {
					if point_lies_on_circle(position, radius, point) {
						IntersectionPointSet::Single(point)
					} else {
						return IntersectionPointSet::Empty;
					}
				}
				(
					Some(CyclePlacement {
						position,
						shape: CycleShape::Circle(radius),
					}),
					IntersectionPointSet::Pair(point_a, point_b),
				) => {
					match (
						point_lies_on_circle(position, radius, point_a),
						point_lies_on_circle(position, radius, point_b),
					) {
						(true, true) => IntersectionPointSet::Pair(point_a, point_b),
						(true, false) => IntersectionPointSet::Single(point_a),
						(false, true) => IntersectionPointSet::Single(point_b),
						(false, false) => {
							return IntersectionPointSet::Empty;
						}
					}
				}
				(Some(cycle_a_placement), IntersectionPointSet::Cycle(cycle_b)) => {
					match self.cycles[cycle_b].placement {
						None => {
							debug_assert!(false, "Unplaced cycle in `compute_intersection`");
							return IntersectionPointSet::Empty;
						}
						Some(cycle_b_placement) => {
							let position_a = cycle_a_placement.position;
							let position_b = cycle_b_placement.position;
							match (cycle_a_placement.shape, cycle_b_placement.shape) {
								(CycleShape::Circle(radius_a), CycleShape::Circle(radius_b)) => {
									let tolerance = radius_a.max(radius_b).max(0.01)
										* Self::PLACEMENT_VALIDATION_TOLERANCE;
									let offset = position_a - position_b;
									let offset_length = offset.length();
									if offset_length <= tolerance {
										if (radius_a - radius_b).abs() < tolerance {
											IntersectionPointSet::Cycle(cycle)
										} else {
											return IntersectionPointSet::Empty;
										}
									} else {
										match intersect_circles(
											position_a, position_b, radius_a, radius_b,
										) {
											Some(OneTwo::One(point)) => {
												IntersectionPointSet::Single(point)
											}
											Some(OneTwo::Two(point_a, point_b)) => {
												IntersectionPointSet::Pair(point_a, point_b)
											}
											None => {
												return IntersectionPointSet::Empty;
											}
										}
									}
								}
							}
						}
					}
				}
			}
		}
		intersection
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
			log::warn!("Partial materialization checks can only be run on placed cycles");
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
	fn get_content_bounding_box(&self) -> Aabb2d {
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
		Aabb2d { min, max }
	}

	/// Gets the true bounding box of the level, either computed or set explicitly by the caller
	fn get_bounding_box(&self) -> Aabb2d {
		let content = self.get_content_bounding_box();
		// Replace bounds with explicit ones if appropriate
		let min = Vec2::new(
			self.explicit_bounding_box.left.unwrap_or(content.min.x),
			self.explicit_bounding_box.top.unwrap_or(content.min.y),
		);
		let max = Vec2::new(
			self.explicit_bounding_box.right.unwrap_or(content.max.x),
			self.explicit_bounding_box.bottom.unwrap_or(content.max.y),
		);
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
		let scale = match self.scale_override {
			Some(scale) => scale,
			None => {
				let scale = viewport.half_size() / bounds.half_size();
				// Scaling must be equal in both directions
				scale.x.min(scale.y)
			}
		};
		let scale = if scale.is_finite() {
			scale.abs()
		} else {
			log::warn!(
				"Non-finite scale value in level bound computation, level likely has 0 size."
			);
			1.0
		};

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

		// Transform explicitly set initial camera position to world coordinates
		if let Some(x) = &mut self.initial_camera_pos.x {
			*x = (*x - bounds_center.x) * scale + viewport_center.x;
		}
		if let Some(y) = &mut self.initial_camera_pos.y {
			*y = (*y - bounds_center.y) * scale + viewport_center.y;
		}

		self.bounding_box = Some(Aabb2d::new(
			Vec2::ZERO,
			(bounds.half_size() * scale).max(Vec2::ONE),
		));
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

fn point_lies_on_circle(center: Vec2, radius: f32, point: Vec2) -> bool {
	approx_eq_rel(
		center.distance(point),
		radius,
		LevelBuilder::PLACEMENT_VALIDATION_TOLERANCE,
	)
}

/// Tests if a, b, c are an "increasing triplet" in circular ordering
fn test_index_clockwiseness(a: usize, b: usize, c: usize) -> bool {
	if a == b || b == c || c == a {
		return false;
	}
	match (a < b, b < c) {
		(true, true) => true,
		(true, false) => c < a,
		(false, true) => c < a,
		(false, false) => false,
	}
}

#[test]
fn test_test_index_clockwiseness() {
	assert!(test_index_clockwiseness(1, 2, 3));
	assert!(test_index_clockwiseness(3, 1, 2));
	assert!(test_index_clockwiseness(2, 3, 1));

	assert!(!test_index_clockwiseness(2, 1, 3));
	assert!(!test_index_clockwiseness(1, 3, 2));
	assert!(!test_index_clockwiseness(3, 2, 1));

	assert!(!test_index_clockwiseness(0, 0, 1));
	assert!(!test_index_clockwiseness(0, 3, 0));
	assert!(!test_index_clockwiseness(1, 0, 0));
}

/// Tests if a,b,c are oriented clockwise
/// Returns a float describing the oriented area given by twice the triangle abc
/// if it is positive, the points are oriented clockwise
/// if it is negative, the points are oriented counterclockwise
/// if it is close to zero... well good luck
///
/// Optionally a center point can be provided, the other points are projected onto a unit circle
/// before computing their orientation, potentially improving numerics or fixing certain errors in the input
///
/// # Panics
/// May panic or give incorrect results if `center` is Some and equals any of the other points
fn test_point_clockwiseness(mut a: Vec2, mut b: Vec2, mut c: Vec2, center: Option<Vec2>) -> f32 {
	if let Some(center) = center {
		a = (a - center).normalize_or_zero();
		b = (b - center).normalize_or_zero();
		c = (c - center).normalize_or_zero();
	}
	-(a - b).perp_dot(a - c)
}

#[test]
fn test_test_clockwiseness() {
	assert!(
		test_point_clockwiseness(
			Vec2::new(0.0, 1.0),
			Vec2::new(1.0, 0.0),
			Vec2::new(0.0, -1.0),
			None
		) > 0.0
	);
	assert!(
		test_point_clockwiseness(
			Vec2::new(1.0, 0.0),
			Vec2::new(0.0, 1.0),
			Vec2::new(0.0, -1.0),
			None
		) < 0.0
	);

	// This should not affect the ordering
	assert!(
		test_point_clockwiseness(
			Vec2::new(0.0, 1.0),
			Vec2::new(1.0, 0.0),
			Vec2::new(0.0, -1.0),
			Some(Vec2::new(-2.0, 0.0))
		) > 0.0
	);
	assert!(
		test_point_clockwiseness(
			Vec2::new(1.0, 0.0),
			Vec2::new(0.0, 1.0),
			Vec2::new(0.0, -1.0),
			Some(Vec2::new(-2.0, 0.0))
		) < 0.0
	);

	// This should though, the center is completely outside the intended circle
	assert!(
		test_point_clockwiseness(
			Vec2::new(0.0, 1.0),
			Vec2::new(1.0, 0.0),
			Vec2::new(0.0, -1.0),
			Some(Vec2::new(2.0, 0.0))
		) < 0.0
	);
	assert!(
		test_point_clockwiseness(
			Vec2::new(1.0, 0.0),
			Vec2::new(0.0, 1.0),
			Vec2::new(0.0, -1.0),
			Some(Vec2::new(2.0, 0.0))
		) > 0.0
	);
}

fn approx_eq_rel(a: f32, b: f32, threshold: f32) -> bool {
	(a - b).abs() < threshold * a.abs().max(b.abs())
}

fn approx_eq_points(a: Vec2, b: Vec2, epsilon: f32) -> bool {
	(a - b).abs().max_element() < epsilon
}

#[derive(Clone, Copy, Debug)]
enum IntersectionPointSet<Point>
where
	Point: Clone + Copy,
{
	/// Any point lies in this intersection,
	/// this probably means that the point should be placed differently.
	Unconstrained,
	/// There is an entire cycle of points that can be used,
	/// the value is an index into [`LevelBuilder::cycles`].
	Cycle(usize),
	/// There are two points where the shapes intersect.
	Pair(Point, Point),
	/// There is a single point where the shapes intersect.
	Single(Point),
	/// The shapes do not intersect at all.
	Empty,
}
