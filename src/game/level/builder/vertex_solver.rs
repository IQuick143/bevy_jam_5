//! Determines the positions of vertices in a level
//!
//! This module exports the [`LevelBuilder::solve_vertex_placements`].
//! It is a stand-alone module because it uses a large amount of ad-hoc
//! data structures that are best left encapsulated

use super::{
	error::{CycleDoesNotContainVertexError, VertexSolverError},
	*,
};
use itertools::Itertools as _;
use smallvec::SmallVec;
use std::{cmp::Ordering, f32::consts::PI};

struct AdditionalCycleData {
	problematic_cycles: Vec<usize>,
	points_on_cycle: Vec<Vec<usize>>,
}

#[derive(Clone, Debug)]
struct CyclePoints {
	/// Index of the earliest entry in [`CyclePoints::vertices`] that is a `Pair`
	first_undecided_vertex: Option<usize>,
	/// Index of the earliest entry in [`CyclePoints::vertices`] that is a `Single`
	first_decided_vertex: Option<usize>,
	/// A datastructure containing links to the `Pair` and `Single` vertices on this cycle.
	/// `Single` vertices are linked together into a linked cycle
	/// `Pair` vertices are linked into a doubly-linked cycle and to single vertices
	vertices: Vec<LogicalVertex>,
}

impl CyclePoints {
	/// Calculates the inner links and invariants from scratch
	fn calculate_links(&mut self) {
		let mut running_single_vertex = None;
		let mut running_pair_vertex = None;

		// Go backwards to create links to next_single
		for (index, vert) in self.vertices.iter_mut().enumerate().rev() {
			match &mut vert.inner {
				LogicalVertexVariant::Single { next_single } => {
					if let Some(single) = running_single_vertex {
						*next_single = single;
					}
					running_single_vertex = Some(index);
				}
				LogicalVertexVariant::Pair {
					next_pair,
					prev_pair: _,
					prev_single: _,
				} => {
					if let Some(pair) = running_pair_vertex {
						*next_pair = pair;
					}
					running_pair_vertex = Some(index);
				}
			}
		}
		self.first_decided_vertex = running_single_vertex;
		self.first_undecided_vertex = running_pair_vertex;
		// TODO: This could be optimised to look only until it finds the first entry of either kind
		for (index, vert) in self.vertices.iter_mut().enumerate().rev() {
			match &mut vert.inner {
				LogicalVertexVariant::Single { next_single } => {
					if let Some(single) = running_single_vertex {
						*next_single = single;
					}
					running_single_vertex = Some(index);
				}
				LogicalVertexVariant::Pair {
					next_pair,
					prev_pair: _,
					prev_single: _,
				} => {
					if let Some(pair) = running_pair_vertex {
						*next_pair = pair;
					}
					running_pair_vertex = Some(index);
				}
			}
		}

		// Go forward to create links to `prev_single`
		let mut running_single_vertex = None;
		let mut running_pair_vertex = None;
		for _ in 0..2 {
			for (index, vert) in self.vertices.iter_mut().enumerate() {
				match &mut vert.inner {
					LogicalVertexVariant::Single { next_single: _ } => {
						running_single_vertex = Some(index);
					}
					LogicalVertexVariant::Pair {
						next_pair: _,
						prev_pair,
						prev_single,
					} => {
						*prev_single = running_single_vertex;
						if let Some(running_pair_vertex) = running_pair_vertex {
							*prev_pair = running_pair_vertex;
						}
						running_pair_vertex = Some(index);
					}
				}
			}
		}
		#[cfg(any(debug_assertions, test))]
		self.assert_validity();
	}

	#[cfg(any(debug_assertions, test))]
	fn assert_validity(&self) {
		let assert_single = |index: usize| {
			assert!(matches!(
				self.vertices[index].inner,
				LogicalVertexVariant::Single { .. }
			));
			assert!(self
				.first_decided_vertex
				.is_some_and(|first_single| { first_single <= index }));
		};
		let assert_pair = |index: usize| {
			assert!(matches!(
				self.vertices[index].inner,
				LogicalVertexVariant::Pair { .. }
			));
			assert!(self
				.first_undecided_vertex
				.is_some_and(|first_single| { first_single <= index }));
		};
		for value in self.vertices.iter() {
			match value.inner {
				LogicalVertexVariant::Single { next_single } => assert_single(next_single),
				LogicalVertexVariant::Pair {
					next_pair,
					prev_pair,
					prev_single,
				} => {
					assert_pair(next_pair);
					assert_pair(prev_pair);
					if let Some(prev_single) = prev_single {
						assert_single(prev_single);
					}
				}
			}
		}
	}

	/// Takes an index of a problematic vertex on a given cycle (index into [`Self::vertices`])
	/// and converts the vertex to a `Single` vertex
	///
	/// Updates internal links and their invariants accordingly
	fn convert_pair_to_single(&mut self, index: usize) {
		let (next_pair, prev_pair, prev_single) = match self.vertices[index].inner {
			LogicalVertexVariant::Single { .. } => {
				return;
			}
			LogicalVertexVariant::Pair {
				next_pair,
				prev_pair,
				prev_single,
			} => (next_pair, prev_pair, prev_single),
		};
		// Perform the update
		if let Some(prev_single) = prev_single {
			self.vertices[index].inner = LogicalVertexVariant::Single {
				next_single: match &mut self.vertices[prev_single].inner {
					LogicalVertexVariant::Single {
						next_single: next_after_prev_single,
					} => {
						let previous_successor = *next_after_prev_single;
						*next_after_prev_single = index;
						previous_successor
					}
					LogicalVertexVariant::Pair { .. } => {
						debug_assert!(false, "`prev_single` pointed to a Pair vertex");
						index
					}
				},
			}
		} else {
			// This ought to be the first Single vertex around
			self.vertices[index].inner = LogicalVertexVariant::Single { next_single: index };
		}
		// Fix pair references
		{
			if next_pair != index {
				match &mut self.vertices[next_pair].inner {
					LogicalVertexVariant::Single { .. } => {
						debug_assert!(false, "`next_pair` pointed to a Single vertex");
					}
					LogicalVertexVariant::Pair {
						prev_pair: next_prev_pair,
						..
					} => {
						*next_prev_pair = prev_pair;
					}
				}
			}
			if prev_pair != index {
				match &mut self.vertices[prev_pair].inner {
					LogicalVertexVariant::Single { .. } => {
						debug_assert!(false, "`prev_pair` pointed to a Single vertex");
					}
					LogicalVertexVariant::Pair {
						next_pair: prev_next_pair,
						..
					} => {
						*prev_next_pair = next_pair;
					}
				}
			}
		}

		// Fix prev_single references
		for offset in 1..self.vertices.len() {
			let target = (index + offset) % self.vertices.len();
			match &mut self.vertices[target].inner {
				LogicalVertexVariant::Single { .. } => {
					break;
				}
				LogicalVertexVariant::Pair { prev_single, .. } => *prev_single = Some(index),
			}
		}

		// Update [`Self::first_decided_vertex`]
		if let Some(first_single) = self.first_decided_vertex {
			if index < first_single {
				self.first_decided_vertex = Some(index);
			}
		} else {
			self.first_decided_vertex = Some(index);
		}
		// Update [`Self::first_undecided_vertex`]
		if let Some(first_pair) = self.first_undecided_vertex {
			if first_pair >= index {
				if next_pair > index {
					self.first_undecided_vertex = Some(next_pair);
				} else {
					self.first_undecided_vertex = None;
				}
			}
		}
	}
}

/// A datastructure for a vertex on a cycle that requires logic to decide whether it's `Single` or `Pair`
#[derive(Clone, Debug)]
struct LogicalVertex {
	/// The backing vertex
	/// [`PointData::vertex_constraints`] of this vertex should agree with the data in [`LogicalVertex::inner`]
	vertex: usize,
	/// Associated type data
	inner: LogicalVertexVariant,
}

#[derive(Clone, Debug)]
enum LogicalVertexVariant {
	/// A `Single` vertex
	Single {
		/// Next `Single` logical vertex in cyclic order,
		/// might loop back
		/// might be itself (iff there is exactly one `Single`)
		///
		/// Indexes into [`CyclePoints::vertices`]
		next_single: usize,
	},
	/// A `Pair` vertex
	Pair {
		/// Next `Pair` logical vertex in cyclic order,
		/// might loop back
		/// might be itself (iff there is exactly one `Pair`)
		///
		/// Indexes into [`CyclePoints::vertices`]
		next_pair: usize,
		/// Next `Pair` logical vertex in cyclic order,
		/// might loop back
		/// might be itself (iff there is exactly one `Pair`)
		///
		/// Indexes into [`CyclePoints::vertices`]
		prev_pair: usize,
		/// Nearest previous `Single` logical vertex in cyclic order,
		/// might loop around
		/// might be None if there is no such vertex
		///
		/// Indexes into [`CyclePoints::vertices`]
		prev_single: Option<usize>,
	},
}

#[derive(Debug)]
struct PointData {
	/// Whether there has been progress in the solution
	/// Set to true every time a `Pair` is converted into a `Single`
	progress: bool,
	/// List of registered positions
	points: Vec<Vec2>,
	/// Mapping from point indices to a list of indices of vertices which could be placed at the point
	vertices_interested_in_point: Vec<SmallVec<[usize; 1]>>,
	/// A constraint detailing where a given vertex can be placed, one per vertex
	/// Due to relevant invariants in [`PointData::cycle_data`] and [`PointData::vertices_interested_in_point`]
	/// this array should not be, after creation, changed directly.
	/// Changing a `Pair` vertex into a `Single` vertex is supported via [`PointData::place_vertex`]
	vertex_constraints: Vec<IntersectionPointSet<usize>>,
	/// Additional datastructures, contains invariants related to [`PointData::vertex_constraints`]
	cycle_data: Vec<CyclePoints>,
	/// Mapping from vertex indices to a list of tuples `(cycle_id, logical_order_in_cycle)`,
	/// Used to index into [`PointData::cycle_data`] and the resulting [`CyclePoints::vertices`] arrays respectively.
	/// Guaranteed to be accurate only for vertices that are `Single` or `Pair`
	///
	/// ## Example:
	/// ```ignore
	/// let (cycle_id, position) = self.problematic_vertex_to_cycle[VERTEX][i];
	/// assert_eq!(VERTEX, self.cycle_data[cycle_id].vertices[position].vertex);
	/// ```
	problematic_vertex_to_cycle: Vec<SmallVec<[(usize, usize); 2]>>,
}

impl PointData {
	/// Enforces the constraint that if there is a vertex is placed on a point,
	/// then no other vertex can occupy said point.
	/// Vertices may be placed (converted into `Single`) as a result of this call
	///
	/// May return an error if multiple vertices are placed on a single point or
	/// there are no options remaining for a vertex.
	fn propagate_constraint(&mut self, point: usize, errors: &mut Vec<VertexSolverError>) {
		let mut stack = vec![point];
		while let Some(point) = stack.pop() {
			let mut owner_vertex = None;
			for &vertex in self.vertices_interested_in_point[point].iter() {
				match self.vertex_constraints[vertex] {
					IntersectionPointSet::Single(_) => match owner_vertex {
						Some(vertex_b) => {
							errors.push(VertexSolverError::TwoVerticesCollide {
								vertex_a: vertex,
								vertex_b,
							});
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
							if a == b {
								debug_assert!(false, "Vertex has a Pair placement, but both options in the pair are the same point");
								continue;
							}
							let other_point;
							if a == point {
								other_point = b;
							} else if b == point {
								other_point = a;
							} else {
								debug_assert!(false, "vertices_interested_in_point contains a vertex not interested in that point");
								continue;
							}
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
	}

	/// Enforces the constraint that if two twin vertices occupy two points,
	/// then no other vertex can occupy said points (even if it is unknown which twin is which point)
	/// Vertices may be placed (converted into `Single`) as a result of this call
	///
	/// May return an error if multiple vertices are placed on a single point or
	/// there are no options remaining for a vertex.
	///
	/// # Panics
	/// May panic if `vertex_1` and `vertex_2` aren't valid vertex indices
	/// which are `Pair` vertices and are twins
	fn propagate_twin_constraint(
		&mut self,
		vertex_1: usize,
		vertex_2: usize,
		errors: &mut Vec<VertexSolverError>,
	) {
		match (
			self.vertex_constraints[vertex_1],
			self.vertex_constraints[vertex_2],
		) {
			(IntersectionPointSet::Pair(a1, b1), IntersectionPointSet::Pair(a2, b2)) => {
				if (a1 == b1) || !((a1 == a2 && b1 == b2) || (a1 == b2 && b1 == a2)) {
					debug_assert!(false, "`propagate_twin_constraint` called on a twin pair that is not actually a twin pair v1:({vertex_1} -> {a1},{b1}) v2:({vertex_2} -> {a2},{b2})")
				}
				// All vertices that are no longer eligible for poinst `a1` and `b2`
				// This is all vertices that target those points except the twins
				let mut evicted_vertices: Vec<usize> = self.vertices_interested_in_point[a1]
					.iter()
					.chain(&self.vertices_interested_in_point[b1])
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
									errors.push(VertexSolverError::VertexHasNoPointsAvailable {
										vertex,
									});
								}
								(true, false) => {
									self.place_vertex(vertex, option_b, errors);
								}
								(false, true) => {
									self.place_vertex(vertex, option_a, errors);
								}
								(false, false) => {
									debug_assert!(false, "`propagate_twin_constraint` encountered vertex {vertex} interested in a point ({a1} or {b1}) which it actually wasn't. Desired points: ({option_a}, {option_b}).");
								}
							}
						}
						IntersectionPointSet::Single(option) => {
							if option == a1 || option == b1 {
								errors
									.push(VertexSolverError::VertexHasNoPointsAvailable { vertex });
							}
						}
						_ => {
							// Unreachable, points with placements other than Single and Pair
							// cannot be targeting points by definition
							debug_assert!(false, "`propagate_twin_constraint` encountered vertex {vertex} interested in points which weren't actually constrained to points");
						}
					}
				}
			}
			_ => {
				// this shouldn't really happen
				debug_assert!(false, "`propagate_twin_constraint` called on a twin pair that is not actually a twin pair v1:({vertex_1}) v2:({vertex_2})");
			}
		}
	}

	/// Turns a vertex into a `Single` vertex placed at the given point
	///
	/// `vertex` should be a valid vertex index
	///
	/// `point` should be a valid point index (into [`PointData::points`])
	///
	/// This placement also propagates constraints caused by the `point` becoming occupied,
	/// displacing other vertices from it.
	///
	/// May return an error if a contradiction was reached during propagation of the constraints.
	fn place_vertex(&mut self, vertex: usize, point: usize, errors: &mut Vec<VertexSolverError>) {
		self.place_vertex_internal(vertex, point);
		self.propagate_constraint(point, errors);
	}

	/// Turns a vertex into a `Single` vertex placed at the given point
	///
	/// `vertex` should be a valid vertex index
	///
	/// `point` should be a valid point index (into [`PointData::points`])
	///
	/// This placement ignores geometrical constraints caused by the `point` becoming occupied,
	/// but still maintains internal datastructure invariants.
	fn place_vertex_internal(&mut self, vertex: usize, point: usize) {
		match self.vertex_constraints[vertex] {
			IntersectionPointSet::Pair(a, b) => {
				self.remove_interest(vertex, a);
				self.remove_interest(vertex, b);
				self.progress = true;
			}
			IntersectionPointSet::Single(a) => self.remove_interest(vertex, a),
			_ => {}
		}
		self.vertex_constraints[vertex] = IntersectionPointSet::Single(point);
		self.vertices_interested_in_point[point].push(vertex);
		self.make_vertex_single_in_cycles_internal(vertex);
	}

	fn make_vertex_single_in_cycles_internal(&mut self, vertex: usize) {
		for &(cycle, index) in self.problematic_vertex_to_cycle[vertex].iter() {
			self.cycle_data[cycle].convert_pair_to_single(index);
		}
	}

	fn remove_interest(&mut self, vertex: usize, point: usize) {
		if let Some(index) = self.vertices_interested_in_point[point]
			.iter()
			.position(|&x| x == vertex)
		{
			self.vertices_interested_in_point[point].remove(index);
		}
	}

	/// Finds a "twin" vertex for a given [`IntersectionPointSet::Pair`] vertex
	///
	/// A twin vertex is a second [`IntersectionPointSet::Pair`] vertex
	/// which has the same desired points
	fn find_twin(&self, vertex: usize) -> Option<usize> {
		match self.vertex_constraints[vertex] {
			IntersectionPointSet::Pair(point_a, point_b) => {
				for &potential_vertex in
					self.vertices_interested_in_point[point_a]
						.iter()
						.filter(|&&potential_vertex| {
							potential_vertex != vertex
								&& self.vertices_interested_in_point[point_b]
									.contains(&potential_vertex)
						}) {
					if let IntersectionPointSet::Pair(point_a2, point_b2) =
						self.vertex_constraints[potential_vertex]
					{
						if (point_a == point_a2 && point_b == point_b2)
							|| (point_a == point_b2 && point_b == point_a2)
						{
							return Some(potential_vertex);
						}
					}
				}
				None
			}
			_ => None,
		}
	}
}

impl LevelBuilder {
	/// Tolerance in validation of manual placements
	const PLACEMENT_VALIDATION_TOLERANCE: f32 = 0.001;

	/// Computes the placements of unplaced vertices based on the geometric constraints or gives up.
	#[must_use]
	pub(super) fn solve_vertex_placements(&mut self) -> Vec<VertexSolverError> {
		let mut error_log: Vec<VertexSolverError> = Vec::new();
		let (mut point_data, mut cycle_data) = self.compute_initial_geometry(&mut error_log);
		self.first_pass_pair_placements(&mut point_data, &mut cycle_data, &mut error_log);
		self.pin_single_placements(&mut point_data, &cycle_data, &mut error_log);
		self.check_oversaturated_points(&point_data, &mut error_log);
		self.check_vertex_placements_are_clockwise(&mut error_log);
		self.pin_cycle_placements();
		#[cfg(debug_assertions)]
		if error_log.is_empty() {
			self.debug_assert_valid_solution();
		}
		error_log
	}

	/// Computes initial parameters that can be directly extracted from the level builder
	///
	/// ## Return Value
	/// - For each vertex, list of all cycles it lies on
	/// - Numeric tolerance based on the scale of the level
	fn initial_characterize_layout(&self) -> (Vec<SmallVec<[usize; 2]>>, f32) {
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

			#[expect(clippy::single_match, reason = "There will be more")]
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
		(
			vertex_to_cycle,
			absolute_precision_limit * Self::PLACEMENT_VALIDATION_TOLERANCE,
		)
	}

	/// Expresses the geometrical constraints inherent to the level
	///
	/// Determines geometric constraints and places uniquely determined points.
	/// Finds cycles that have ambiguous points.
	/// Points are deduplicated and indexed, facilitating equality comparisons.
	fn compute_initial_geometry(
		&self,
		errors: &mut Vec<VertexSolverError>,
	) -> (PointData, AdditionalCycleData) {
		let (vertex_to_cycle, absolute_precision_limit) = self.initial_characterize_layout();
		// TODO: End early if all vertices are fixed

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

		for (vertex, cycles) in vertex_to_cycle.iter().enumerate() {
			let Some(&parent_cycle) = cycles.first() else {
				errors.push(VertexSolverError::VertexHasNoCycle(vertex));
				continue;
			};
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
						if let Some(placement) = self.cycles[cycle].placement {
							if !point_lies_on_cycle(placement, position) {
								errors.push(VertexSolverError::CycleDoesNotContainVertex(
									CycleDoesNotContainVertexError {
										vertex,
										cycle,
										position,
										placement,
									},
								));
							}
						}
					}
					IntersectionPointSet::Single(point)
				}
				IntermediateVertexPosition::Free => {
					let intersection = self.compute_intersection(cycles);
					match intersection {
						IntersectionPointSet::Unconstrained => {
							errors.push(VertexSolverError::VertexIsUnconstrained { vertex });
							IntersectionPointSet::Unconstrained
						}
						IntersectionPointSet::Cycle(cycle) => {
							// Use hints if possible
							match self.vertices[vertex].hint_position {
								Some(hint_position) => {
									if let Some(CyclePlacement {
										position: cycle_position,
										shape,
									}) = self.cycles[cycle].placement
									{
										match shape {
											CycleShape::Circle(radius) => {
												let distance_from_center =
													cycle_position.distance(hint_position);
												if distance_from_center <= absolute_precision_limit
												{
													errors.push(
														VertexSolverError::UnnecessaryHint(vertex),
													);
													IntersectionPointSet::Cycle(cycle)
												} else {
													let offset = hint_position - cycle_position;
													let point = cycle_position
														+ (radius / distance_from_center) * offset;
													IntersectionPointSet::Single(
														insert_point_if_needed(
															&mut points,
															&mut vertices_interested_in_point,
															&points_on_cycle,
															point,
															vertex,
															parent_cycle,
														),
													)
												}
											}
										}
									} else {
										// Should not be happening
										debug_assert!(false, "Unplaced cycle");
										IntersectionPointSet::Cycle(cycle)
									}
								}
								None => IntersectionPointSet::Cycle(cycle),
							}
						}
						IntersectionPointSet::Pair(point_a, point_b) => {
							// Drop one of the positions if the user is hinting towards the other
							let reduced_placement = match self.vertices[vertex].hint_position {
								Some(position) => {
									match Vec2::distance_squared(point_a, position)
										.partial_cmp(&Vec2::distance_squared(point_b, position))
									{
										Some(Ordering::Greater) => One(point_b),
										Some(Ordering::Less) => One(point_a),
										Some(Ordering::Equal) => {
											errors.push(VertexSolverError::UnnecessaryHint(vertex));
											eprintln!("eq {point_a} {point_b} {position}");
											Two(point_a, point_b)
										}
										None => {
											errors.push(VertexSolverError::UnnecessaryHint(vertex));
											eprintln!("nc {point_a} {point_b} {position}");
											Two(point_a, point_b)
										}
									}
								}
								None => Two(point_a, point_b),
							};
							// Translate the positions into point indices
							// by adding points into the container
							let points_placement = match reduced_placement {
								Two(point_a, point_b) => {
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
									if point_a == point_b {
										// This is a stupid branch, but it's possible if you got a really degenerate thing going on.
										One(point_a)
									} else {
										Two(point_a, point_b)
									}
								}
								One(point) => One(insert_point_if_needed(
									&mut points,
									&mut vertices_interested_in_point,
									&points_on_cycle,
									point,
									vertex,
									parent_cycle,
								)),
							};
							match points_placement {
								Two(point_a, point_b) => {
									for &cycle in cycles.iter() {
										problematic_cycles.push(cycle);
										points_on_cycle[cycle].push(point_a);
										points_on_cycle[cycle].push(point_b);
									}
									IntersectionPointSet::Pair(point_a, point_b)
								}
								One(point) => {
									for &cycle in cycles.iter() {
										problematic_cycles.push(cycle);
										points_on_cycle[cycle].push(point);
									}
									IntersectionPointSet::Single(point)
								}
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
						IntersectionPointSet::Empty => {
							errors.push(VertexSolverError::VertexHasNoPointsAvailable { vertex });
							IntersectionPointSet::Empty
						}
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
		let mut cycle_data = vec![
			CyclePoints {
				first_undecided_vertex: None,
				first_decided_vertex: None,
				vertices: Vec::new()
			};
			self.cycles.len()
		];
		let mut problematic_vertex_to_cycle = vec![SmallVec::new(); self.vertices.len()];
		for &cycle_index in problematic_cycles.iter() {
			for &vertex in self.cycles[cycle_index].vertex_indices.iter() {
				match vertex_constraints[vertex] {
					IntersectionPointSet::Pair(_, _) => {
						cycle_data[cycle_index].vertices.push(LogicalVertex {
							vertex,
							inner: LogicalVertexVariant::Pair {
								next_pair: 0,
								prev_pair: 0,
								prev_single: None,
							},
						});
						problematic_vertex_to_cycle[vertex]
							.push((cycle_index, cycle_data[cycle_index].vertices.len() - 1));
					}
					IntersectionPointSet::Single(_) => {
						cycle_data[cycle_index].vertices.push(LogicalVertex {
							vertex,
							inner: LogicalVertexVariant::Single { next_single: 0 },
						});
						problematic_vertex_to_cycle[vertex]
							.push((cycle_index, cycle_data[cycle_index].vertices.len() - 1));
					}
					_ => {}
				}
			}
			cycle_data[cycle_index].calculate_links();
		}
		(
			PointData {
				progress: true,
				points,
				vertices_interested_in_point,
				vertex_constraints,
				cycle_data,
				problematic_vertex_to_cycle,
			},
			AdditionalCycleData {
				problematic_cycles,
				points_on_cycle,
			},
		)
	}

	/// First attempt to solve [`IntersectionPointSet::Pair`] placements
	///
	/// TODO: Optimise
	fn first_pass_pair_placements(
		&self,
		point_data: &mut PointData,
		cycle_data: &mut AdditionalCycleData,
		error_log: &mut Vec<VertexSolverError>,
	) {
		// Propagate constraints caused by `Single` points already placed
		for &cycle_id in cycle_data.problematic_cycles.iter() {
			for &point in cycle_data.points_on_cycle[cycle_id].iter() {
				point_data.propagate_constraint(point, error_log);
			}
		}
		// Propagate constraints caused by two twin vertices occupying two points for themselves (even if it's unknown which is which)
		for &cycle_id in cycle_data.problematic_cycles.iter() {
			for &vertex in self.cycles[cycle_id].vertex_indices.iter() {
				if let Some(twin_vertex) = point_data.find_twin(vertex) {
					point_data.propagate_twin_constraint(vertex, twin_vertex, error_log);
				}
			}
		}

		point_data.progress = true;
		while point_data.progress && error_log.is_empty() {
			point_data.progress = false;
			// Use simple deductions to place remaining undecided vertices
			for &cycle_id in cycle_data.problematic_cycles.iter() {
				self.try_reduce_pairs_on_cycle(cycle_id, point_data, error_log);
			}
			cycle_data.problematic_cycles.retain(|&cycle| {
				point_data.cycle_data[cycle]
					.first_undecided_vertex
					.is_some()
			});
		}
	}

	/// Single step of [`Self::first_pass_pair_placements`]
	///
	/// Attempts to reduce the number of [`IntersectionPointSet::Pair`] placements
	/// on a single cycle
	fn try_reduce_pairs_on_cycle(
		&self,
		cycle_id: usize,
		point_data: &mut PointData,
		error_log: &mut Vec<VertexSolverError>,
	) {
		let cycle = &self.cycles[cycle_id];
		let Some(placement) = cycle.placement else {
			// This should not really happen
			return;
		};
		let cycle_position = placement.position;
		match placement.shape {
			CycleShape::Circle(_) => {
				// Only go through the logic if there is an undecided vertex available.
				if let Some(first_pair_index) =
					point_data.cycle_data[cycle_id].first_undecided_vertex
				{
					if point_data.cycle_data[cycle_id]
						.first_decided_vertex
						.is_none()
					{
						// There are no `Single`s available,
						// so we go after `Pair` therapy
						self.try_reduce_pairs_on_cycle_without_single(
							cycle_id,
							cycle_position,
							point_data,
							error_log,
						);
					}
					// If there is a `Single` vertex available (as an anchor)
					// The check is done again, because pair_therapy might have created a new one
					if point_data.cycle_data[cycle_id]
						.first_decided_vertex
						.is_some()
					{
						self.try_reduce_pairs_on_cycle_using_single(
							cycle_id,
							cycle_position,
							first_pair_index,
							point_data,
							error_log,
						);
					}
				}
			}
		}
	}

	/// Single step of [`Self::first_pass_pair_placements`]
	///
	/// Attempts to resolve a [`IntersectionPointSet::Pair`] placement
	/// on a cycle where there are no [`IntersectionPointSet::Single`] vertices
	fn try_reduce_pairs_on_cycle_without_single(
		&self,
		cycle_id: usize,
		cycle_position: Vec2,
		point_data: &mut PointData,
		error_log: &mut Vec<VertexSolverError>,
	) {
		'pair_therapy: for pair_vertex_index in 0..point_data.cycle_data[cycle_id].vertices.len() {
			// Each vertex should be a `Pair` given that there was no `Single` when we reached this code
			// If a vertex was placed, `'pair_therapy` should be stopped, as faster and more powerful algorithms apply.
			let LogicalVertex {
				vertex: pair_vertex,
				inner:
					LogicalVertexVariant::Pair {
						next_pair: _,
						prev_pair: _,
						prev_single: None,
					},
			} = point_data.cycle_data[cycle_id].vertices[pair_vertex_index]
			else {
				debug_assert!(false, "Invariants broken, the truth goes unspoken.");
				break;
			};
			// To make a deduction, we need a twin vertex
			let Some((twin_vertex, twin_vertex_index)) =
				point_data.find_twin(pair_vertex).and_then(|twin_vertex| {
					point_data.problematic_vertex_to_cycle[twin_vertex]
						.iter()
						.find(|&&(twin_cycle_id, _twin_position_in_cycle)| {
							twin_cycle_id == cycle_id
						})
						.map(|&(_, twin_position_in_cycle)| (twin_vertex, twin_position_in_cycle))
				})
			else {
				continue;
			};
			// Avoid doing the work twice
			if twin_vertex < pair_vertex {
				continue;
			}
			// Go through every other pair, if both points of that pair lie on a given side
			// of the twin pair, then we can deduce the twin pair orientation and place them
			for constraint_vertex_index in 0..point_data.cycle_data[cycle_id].vertices.len() {
				// We need a separate vertex, not one already present
				if constraint_vertex_index == pair_vertex_index
					|| constraint_vertex_index == twin_vertex_index
				{
					continue;
				}
				let constraint_vertex =
					point_data.cycle_data[cycle_id].vertices[constraint_vertex_index].vertex;
				if let (
					IntersectionPointSet::Pair(point_a, point_b),
					IntersectionPointSet::Pair(option_1, option_2),
				) = (
					point_data.vertex_constraints[pair_vertex],
					point_data.vertex_constraints[constraint_vertex],
				) {
					let clockwiseness = test_index_clockwiseness(
						pair_vertex_index,
						twin_vertex_index,
						constraint_vertex_index,
					);
					let sign = if clockwiseness { 1.0 } else { -1.0 };

					let score_1 = test_point_clockwiseness(
						point_data.points[point_a],
						point_data.points[point_b],
						point_data.points[option_1],
						Some(cycle_position),
					) * sign;
					let score_2 = test_point_clockwiseness(
						point_data.points[point_a],
						point_data.points[point_b],
						point_data.points[option_2],
						Some(cycle_position),
					) * sign;

					// If both situations agree
					if score_1.signum() == score_2.signum() {
						// Whether point_a, point_b is correct order or they should be flipped
						if score_1.is_sign_positive() {
							point_data.place_vertex(pair_vertex, point_a, error_log);
							point_data.place_vertex(twin_vertex, point_b, error_log);
						} else {
							point_data.place_vertex(pair_vertex, point_b, error_log);
							point_data.place_vertex(twin_vertex, point_a, error_log);
						}
						// Now that a pair of vertices has been placed, there should be singles available for the second algorithm to pick up.
						break 'pair_therapy;
					}
				} else {
					debug_assert!(false, "THERE SHOULD BE ONLY PAIR VERTICES");
				}
			}
		}
	}

	/// Single step of [`Self::first_pass_pair_placements`]
	///
	/// Attempts to resolve a [`IntersectionPointSet::Pair`] placement
	/// on a cycle using a [`IntersectionPointSet::Single`] vertex as reference point
	fn try_reduce_pairs_on_cycle_using_single(
		&self,
		cycle_id: usize,
		cycle_position: Vec2,
		first_pair_index: usize,
		point_data: &mut PointData,
		error_log: &mut Vec<VertexSolverError>,
	) {
		for pair_vertex_index in first_pair_index..point_data.cycle_data[cycle_id].vertices.len() {
			let LogicalVertex {
				vertex: pair_vertex,
				inner:
					LogicalVertexVariant::Pair {
						next_pair: _,
						prev_pair: _,
						prev_single: Some(prev_single_index),
					},
			} = point_data.cycle_data[cycle_id].vertices[pair_vertex_index]
			else {
				continue;
			};
			// There ought to exist a `Single` vertex, `first_decided_vertex` points to one dammit! (or does it...)
			let LogicalVertex {
				vertex: prev_single_vertex,
				inner: LogicalVertexVariant::Single {
					next_single: next_single_index,
				},
			} = point_data.cycle_data[cycle_id].vertices[prev_single_index]
			else {
				debug_assert!(false, "Invariants broken, the truth goes unspoken.");
				break;
			};
			let next_single_vertex =
				point_data.cycle_data[cycle_id].vertices[next_single_index].vertex;

			match (
				point_data.vertex_constraints[pair_vertex],
				point_data.vertex_constraints[prev_single_vertex],
				point_data.vertex_constraints[next_single_vertex],
			) {
				(
					IntersectionPointSet::Pair(point_a, point_b),
					IntersectionPointSet::Single(prev_point),
					IntersectionPointSet::Single(next_point),
				) => {
					// If there is only one `Single` vertex, then it will point to itself and
					// `prev_single_vertex` might equal `next_single_vertex`

					// Inner if checks whether we have two different vertices, and if so,
					// Executes code that might resolve the `Pair`
					// Outer if checks if the previous code failed, and if so,
					// Tries running a special branch that applies to twin vertices.
					if !if prev_single_vertex != next_single_vertex {
						// The two `Single` make a "sandwich" constraining the range of
						// Allowed positions for the `Pair` vertex.
						// This might be enough to eliminate one of the options

						// Two constraints are available
						let clockwiseness = test_index_clockwiseness(
							prev_single_index,
							pair_vertex_index,
							next_single_index,
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
								point_data.place_vertex(pair_vertex, point_a, error_log);
								true
							}
							(false, true) => {
								point_data.place_vertex(pair_vertex, point_b, error_log);
								true
							}
							(false, false) => {
								error_log.push(VertexSolverError::VertexHasNoPointsAvailable {
									vertex: pair_vertex,
								});
								// Pretend the code succeeded in placing the vertex, since the situation is unsolvable
								// so there's no need for more attempts at solving
								true
							}
						}
					} else {
						false
					} {
						// Only one constraint is guaranteed, but twin vertices can still be decided this way
						if let Some((twin_vertex, twin_vertex_index)) =
							// Similarly as in `'pair_therapy`, the twin vertices have an order fixed by an external point
							// But in the case of a `Single` it can always be decided which order is correct.
							point_data.find_twin(pair_vertex).and_then(|twin_vertex| {
									point_data.problematic_vertex_to_cycle[twin_vertex]
										.iter()
										.find(|&&(twin_cycle_id, _twin_position_in_cycle)| {
											twin_cycle_id == cycle_id
										})
										.map(|&(_, twin_position_in_cycle)| {
											(twin_vertex, twin_position_in_cycle)
										})
								}) {
							let clockwiseness = test_index_clockwiseness(
								prev_single_index,
								pair_vertex_index,
								twin_vertex_index,
							);
							let point_clockwiseness = test_point_clockwiseness(
								point_data.points[prev_point],
								point_data.points[point_a],
								point_data.points[point_b],
								Some(cycle_position),
							);
							if clockwiseness == (point_clockwiseness > 0.0) {
								point_data.place_vertex(pair_vertex, point_a, error_log);
							} else {
								point_data.place_vertex(pair_vertex, point_b, error_log);
							}
							debug_assert!(
								!error_log.is_empty()
									|| matches!(
										point_data.vertex_constraints[twin_vertex],
										IntersectionPointSet::Single(_)
									),
								"Vertex propagation should've filled this in."
							);
						}
					}
				}
				_ => { /* dont care */ }
			}

			#[cfg(any(debug_assertions, test))]
			point_data.cycle_data[cycle_id].assert_validity();
		}
	}

	/// Converts [`IntersectionPointSet::Single`] placements into
	/// [`IntermediateVertexPosition::Fixed`] values
	///
	/// Decides if remaining [`IntersectionPointSet::Pair`] placements
	/// can be guessed without breaking anything
	fn pin_single_placements(
		&mut self,
		point_data: &mut PointData,
		cycle_data: &AdditionalCycleData,
		error_log: &mut Vec<VertexSolverError>,
	) {
		for (vertex_id, vertex_placement) in self.vertices.iter_mut().enumerate() {
			match point_data.vertex_constraints[vertex_id] {
				IntersectionPointSet::Single(point) => {
					vertex_placement.position =
						IntermediateVertexPosition::Fixed(point_data.points[point]);
				}
				IntersectionPointSet::Cycle(_) => {}
				IntersectionPointSet::Pair(point_a, point_b) => {
					error_log.push(VertexSolverError::VertexRemainsUndecided { vertex: vertex_id });
					let selected_point = Self::collapse_and_pin_pair_placement(
						vertex_id,
						point_a,
						point_b,
						&self.cycles,
						point_data,
						cycle_data,
					)
					.unwrap_or_else(|err| {
						error_log.push(err);
						// Return whichever point as a placeholder
						// so we can grnerate a partial build
						point_a
					});
					point_data.place_vertex(vertex_id, selected_point, error_log);
					vertex_placement.position =
						IntermediateVertexPosition::Fixed(point_data.points[selected_point]);
				}
				IntersectionPointSet::Empty | IntersectionPointSet::Unconstrained => {
					// Do nothing, errors have already been emited
				}
			}
		}
	}

	/// Called on a vertex that has [`IntersectionPointSet::Pair`] placement
	/// to choose its final position
	///
	/// The position is chosen heuristically to create a best fit for the new vertex
	///
	/// ## Return Value
	/// One of `point_a` or `point_b`; the position chosen for the vertex
	fn collapse_and_pin_pair_placement(
		pair_vertex: usize,
		point_a: usize,
		point_b: usize,
		cycles: &[IntermediateCycleData],
		point_data: &PointData,
		cycle_data: &AdditionalCycleData,
	) -> Result<usize, VertexSolverError> {
		if let Some(twin_vertex) = point_data.find_twin(pair_vertex) {
			Self::collapse_and_pin_twin_pair_placement(
				pair_vertex,
				twin_vertex,
				point_a,
				point_b,
				cycles,
				point_data,
			)
		} else {
			Self::collapse_and_pin_unsaturated_pair_placement(
				pair_vertex,
				point_a,
				point_b,
				cycles,
				point_data,
				cycle_data,
			)
		}
	}

	/// Called on a vertex that has [`IntersectionPointSet::Pair`] placement
	/// to choose its final position. The pair placement is shared with another vertex
	///
	/// The position is chosen heuristically to create a best fit for the new vertex
	///
	/// ## Return Value
	/// One of `point_a` or `point_b`; the position chosen for the vertex
	fn collapse_and_pin_twin_pair_placement(
		pair_vertex: usize,
		twin_vertex: usize,
		point_a: usize,
		point_b: usize,
		cycles: &[IntermediateCycleData],
		point_data: &PointData,
	) -> Result<usize, VertexSolverError> {
		let mut cycles_to_check: Vec<usize> = point_data.problematic_vertex_to_cycle[pair_vertex]
			.iter()
			.chain(point_data.problematic_vertex_to_cycle[twin_vertex].iter())
			.map(|(cycle, _)| *cycle)
			.collect();
		cycles_to_check.sort();
		cycles_to_check.dedup();
		for &cycle in cycles_to_check.iter() {
			for logical_vertex in point_data.cycle_data[cycle].vertices.iter() {
				if logical_vertex.vertex != pair_vertex && logical_vertex.vertex != twin_vertex {
					return Err(VertexSolverError::CannotPinTwinPair([
						pair_vertex,
						twin_vertex,
						logical_vertex.vertex,
					]));
				}
			}
		}
		// A measure of how squished a given choice of point placements is.
		// High positive pressure means it's a bad idea to place pair -> point_b and twin -> point_a
		let mut pressure = 0.0;
		/// A positive value added to the denominator to avoid divisions by 0,
		/// represents how tolerable it is to shove a vertex into a 0-length cycle segment
		/// At 0 it would be unthinkable to allow that
		/// At +inf it would not matter at all to place vertices very tightly together
		const REGULARISATION: f32 = 0.2_f32;
		for &cycle in cycles_to_check.iter() {
			match cycles[cycle].placement {
				Some(CyclePlacement {
					position,
					shape: CycleShape::Circle(_),
				}) => {
					// Calculate disparity between inner and outer areas
					let dir_a = Dir2::new(point_data.points[point_a] - position).unwrap_or(Dir2::X);
					let dir_b = Dir2::new(point_data.points[point_b] - position).unwrap_or(Dir2::Y);
					let inner_fraction = {
						let mut fraction = dir_a.rotation_to(dir_b).as_turn_fraction();
						if fraction < 0.0 {
							fraction += 1.0;
						}
						#[expect(clippy::manual_clamp, reason = "clamp handles NaN differently")]
						fraction.min(1.0).max(0.0)
					};
					let outer_fraction = 1.0 - inner_fraction;

					// Calculate disparity between inner and outer free vertex counts
					let mut inverted = false;
					let mut inner_vertices: i32 = 0;
					let mut outer_vertices: i32 = 0;
					let mut seen_first = false;
					let mut seen_second = false;

					for &vertex in cycles[cycle].vertex_indices.iter() {
						if vertex == pair_vertex {
							if seen_second && !seen_first {
								inverted = true;
							}
							seen_first = true;
						}
						if vertex == twin_vertex {
							seen_second = true;
						}
						if vertex != pair_vertex && vertex != twin_vertex {
							if seen_first != seen_second {
								inner_vertices += 1;
							} else {
								outer_vertices += 1;
							}
						}
					}

					if inverted {
						(inner_vertices, outer_vertices) = (outer_vertices, inner_vertices);
					}

					pressure += (inner_vertices - outer_vertices) as f32
						* (1.0 / (inner_fraction + REGULARISATION)
							- 1.0 / (outer_fraction + REGULARISATION));
				}
				None => {
					// uhhh
				}
			}
		}

		// The twin vertex should now get itself set to `Single` and should be caught by a later iteration of the main loop
		if pressure < 0.0 {
			Ok(point_b)
		} else {
			Ok(point_a)
		}
	}

	/// Called on a vertex that has [`IntersectionPointSet::Pair`] placement
	/// to choose its final position. The pair placement is unsaturated
	/// (one of the points will end up empty)
	///
	/// The position is chosen heuristically to create a best fit for the new vertex
	///
	/// ## Return Value
	/// One of `point_a` or `point_b`; the position chosen for the vertex
	fn collapse_and_pin_unsaturated_pair_placement(
		pair_vertex: usize,
		point_a: usize,
		point_b: usize,
		cycles: &[IntermediateCycleData],
		point_data: &PointData,
		cycle_data: &AdditionalCycleData,
	) -> Result<usize, VertexSolverError> {
		let mut cycles_to_check: Vec<usize> = point_data.problematic_vertex_to_cycle[pair_vertex]
			.iter()
			.map(|(cycle, _)| *cycle)
			.collect();
		cycles_to_check.sort();
		cycles_to_check.dedup();
		for &cycle in cycles_to_check.iter() {
			match cycles[cycle].placement {
				Some(CyclePlacement {
					position,
					shape: CycleShape::Circle(_),
				}) => {
					let mut constrained_inside = false;
					let mut constrained_outside = false;

					for &point in cycle_data.points_on_cycle[cycle].iter() {
						let score = test_point_clockwiseness(
							point_data.points[point_a],
							point_data.points[point],
							point_data.points[point_b],
							Some(position),
						);
						if score.abs() > 0.001 {
							if score > 0.0 {
								constrained_inside = true;
							} else {
								constrained_outside = true;
							}
							if constrained_inside && constrained_outside {
								return Err(VertexSolverError::CannotPinUnsaturatedPair(
									pair_vertex,
								));
							}
						}
					}
				}
				None => {
					// Technically unreachable, unplaced cycles are not involved in this
					debug_assert!(
						false,
						"Unplaced cycle encountered when pinning unsaturated vertex"
					);
				}
			}
		}
		// Place it in the first spot idk who cares lmao.
		Ok(point_a)
	}

	/// Place all vertices that have a [`IntersectionPointSet::Cycle`] constraint
	fn pin_cycle_placements(&mut self) {
		for cycle_data in &self.cycles {
			let Some(cycle_placement) = cycle_data.placement else {
				continue;
			};
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
	}

	fn check_oversaturated_points(
		&self,
		point_data: &PointData,
		errors: &mut Vec<VertexSolverError>,
	) {
		let mut vertex_by_point = vec![None; point_data.points.len()];
		for (i, &constraint) in point_data.vertex_constraints.iter().enumerate() {
			match constraint {
				IntersectionPointSet::Pair(_, _) => {
					// All pairs should be singled out by now
					debug_assert!(
						false,
						"Found pair placement after pair placements have been resolved"
					);
				}
				IntersectionPointSet::Single(point) => {
					if let Some(j) = vertex_by_point[point].replace(i) {
						errors.push(VertexSolverError::TwoVerticesCollide {
							vertex_a: i,
							vertex_b: j,
						});
					}
				}
				// No other placements can oversaturate our points
				_ => {}
			}
		}
	}

	fn check_vertex_placements_are_clockwise(&self, errors: &mut Vec<VertexSolverError>) {
		for (cycle_id, cycle) in self.cycles.iter().enumerate() {
			let Some(placement) = &cycle.placement else {
				continue;
			};
			let positions = cycle
				.vertex_indices
				.iter()
				.filter_map(|&i| self.vertices[i].position.get_fixed().map(|p| (i, p)))
				.collect::<Vec<_>>();
			if positions.len() < 3 {
				continue;
			}
			for ((ia, a), (ib, b), (ic, c)) in positions.into_iter().circular_tuple_windows() {
				let score = test_point_clockwiseness(a, b, c, Some(placement.position));
				if score < 0.0 {
					errors.push(VertexSolverError::VerticesNotClockwise {
						cycle: cycle_id,
						vertices: [ia, ib, ic],
					});
					// Break the inner loop, we do not need additional diagnostics
					// for the same cycle
					break;
				}
			}
		}
	}

	#[cfg(debug_assertions)]
	fn debug_assert_valid_solution(&self) {
		for (cycle_id, cycle) in self.cycles.iter().enumerate() {
			// TODO: This should be an internal (debug) error someday
			// Right now it can happen if the user does not place a cycle
			let Some(placement) = cycle.placement else {
				continue;
			};

			let position = placement.position;
			let CycleShape::Circle(radius) = placement.shape;

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
					"Vertices out of order: [{pos_1} -> {vert_1}, {pos_2} -> {vert_2}, {pos_3} -> {vert_3}]"
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
	///
	/// skipping unplaced cycles and cycles that are other geometries,
	/// those should not be used in intersection tests.
	///
	/// # Panics
	/// May panic if any of the cycle indices are not valid indices into [`LevelBuilder::cycles`]
	fn compute_intersection(&self, cycles: &[usize]) -> IntersectionPointSet<Vec2> {
		let mut intersection = IntersectionPointSet::Unconstrained;
		for &cycle in cycles.iter() {
			intersection = match (self.cycles[cycle].placement, intersection) {
				(_, IntersectionPointSet::Empty) => {
					// Technically unreachable because we return early
					// whenever the intersection would become empty
					return IntersectionPointSet::Empty;
				}
				(None, current_intersection) => {
					// Unplaced cycles and cycles with loose geometries
					// do not affect the intersection
					// TODO: when new loose cycle shapes are added, they go to this branch
					current_intersection
				}
				(_, IntersectionPointSet::Unconstrained) => IntersectionPointSet::Cycle(cycle),
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
							// Technically unreachable as the `None` branch does not assign
							// the intersection points
							IntersectionPointSet::Cycle(cycle)
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

fn point_lies_on_cycle(cycle: CyclePlacement, point: Vec2) -> bool {
	match cycle.shape {
		CycleShape::Circle(radius) => point_lies_on_circle(cycle.position, radius, point),
	}
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
/// or if any of the inputs are infinity or NaN
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

/// Container that holds one or two of something
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum OneTwo<T> {
	One(T),
	Two(T, T),
}
use OneTwo::*;

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
