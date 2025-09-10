//! ECS-independent gameplay logic

use super::level::{CycleTurnability, DetectorOrGroup, GlyphData, LevelData, ObjectData};
use bevy::prelude::*;

/// Contains an overview of conditions that are needed to complete the level
#[derive(Resource, Debug, Clone, Copy, Reflect, Default)]
pub struct LevelCompletionConditions {
	pub buttons_present: u32,
	pub buttons_triggered: u32,
	pub flags_present: u32,
	pub flags_occupied: u32,
}

impl LevelCompletionConditions {
	/// Whether the level has been completed
	pub fn is_level_completed(&self) -> bool {
		self.is_goal_unlocked() && self.flags_occupied == self.flags_present
	}

	/// Whether all secondary completion criteria have been met,
	/// and the level will be completed as soon as all players travel to a goal
	pub fn is_goal_unlocked(&self) -> bool {
		self.buttons_present == self.buttons_triggered
	}
}

/// Contains all game state that is not fixed in the level description
///
/// The state object does not hold references to the structural level
/// data, so most operations on it require borrowing the level data
#[derive(Resource, Clone, PartialEq, Eq, Hash, Debug, Default)]
pub struct GameState {
	/// Objects that lie on vertices of the level
	///
	/// Zippable with [`LevelData::vertices`]
	pub objects: Vec<Option<ObjectData>>,
}

impl GameState {
	/// Constructs a new game state object from a level description,
	/// initialized to the position defined by the level
	pub fn new(level_data: &LevelData) -> Self {
		let objects = level_data
			.vertices
			.iter()
			.map(|vertex| vertex.object)
			.collect();
		Self { objects }
	}

	/// Turns a cycle and all cycles linked to it by any number of ticks
	pub fn turn_cycle_with_links(
		&mut self,
		level: &LevelData,
		cycle_index: usize,
		rotate_by: i64,
	) -> Result<TurnCycleResult, GameStateActionError> {
		let result = self.simulate_turn_cycle_with_links(level, cycle_index, rotate_by)?;
		if !result.blocked() {
			result.reorder_sequence_by_all_cycle_turns(level, &mut self.objects)?;
		}
		Ok(result)
	}

	/// Calculates the outcome of trying to turn a cycle and all cycles linked to it
	pub fn simulate_turn_cycle_with_links(
		&self,
		level: &LevelData,
		cycle_index: usize,
		rotate_by: i64,
	) -> Result<TurnCycleResult, GameStateActionError> {
		let groups_turned_by =
			self.calculate_rotation_tables_for_cycle_turn(level, cycle_index, rotate_by)?;
		let clashes = self.find_cycle_clashes(level, &groups_turned_by);
		Ok(TurnCycleResult {
			groups_turned_by,
			clashes,
		})
	}

	/// Checks whether a given vertex contains both a glyph and a matching object
	pub fn is_vertex_triggered(
		&self,
		level: &LevelData,
		vertex_index: usize,
	) -> Result<bool, GameStateActionError> {
		let object = *self
			.objects
			.get(vertex_index)
			.ok_or(GameStateActionError::CycleIndexOutOfRange(vertex_index))?;
		let glyph = level
			.vertices
			.get(vertex_index)
			.ok_or(GameStateActionError::MismatchedLevelData)?
			.glyph;
		Ok(match (glyph, object) {
			(Some(GlyphData::Flag), Some(ObjectData::Player)) => true,
			(Some(GlyphData::Button(button_color)), Some(ObjectData::Box(box_color))) => {
				let button_color = button_color.map(|(c, _)| c);
				button_color.is_none() || box_color.is_none() || button_color == box_color
			}
			_ => false,
		})
	}

	/// Evaluate the level's completion conditions
	pub fn get_completion(&self, level_data: &LevelData) -> LevelCompletionConditions {
		let mut completion = LevelCompletionConditions::default();
		for (i, vertex) in level_data.vertices.iter().enumerate() {
			let is_triggered = self.is_vertex_triggered(level_data, i).unwrap();
			match &vertex.glyph {
				Some(GlyphData::Flag) => {
					completion.flags_present += 1;
					if is_triggered {
						completion.flags_occupied += 1;
					}
				}
				Some(GlyphData::Button(_)) => {
					completion.buttons_present += 1;
					if is_triggered {
						completion.buttons_triggered += 1;
					}
				}
				_ => {}
			}
		}
		completion
	}

	/// Checks whether a cycle can be turned with regard to current game state
	pub fn is_cycle_turnable(
		&self,
		level: &LevelData,
		cycle_index: usize,
	) -> Result<bool, GameStateActionError> {
		let Some(cycle) = level.cycles.get(cycle_index) else {
			return Err(GameStateActionError::CycleIndexOutOfRange(cycle_index));
		};
		Ok(match cycle.turnability {
			CycleTurnability::Always => true,
			CycleTurnability::Never => false,
			CycleTurnability::WithPlayer => cycle.vertex_indices.iter().copied().any(|vertex_id| {
				self.objects.get(vertex_id).copied().flatten() == Some(ObjectData::Player)
			}),
		})
	}

	/// Calculates how many turns of individual cycles are caused by turning a given cycle
	/// a specified number of times
	///
	/// ## Return Value
	/// Number of turns (positive is clockwise) taken by each cycle group
	fn calculate_rotation_tables_for_cycle_turn(
		&self,
		level: &LevelData,
		cycle_index: usize,
		rotate_by: i64,
	) -> Result<Vec<i64>, GameStateActionError> {
		let mut detector_rotations = vec![0i64; level.detectors.len()];
		let mut group_rotations = vec![0i64; level.groups.len()];

		let Some(source_cycle) = level.cycles.get(cycle_index) else {
			return Err(GameStateActionError::CycleIndexOutOfRange(cycle_index));
		};

		// We assume that the calls to this function always target a valid target and a rotation happens.
		// Queue the rotation
		group_rotations[source_cycle.group] += source_cycle.orientation_within_group * rotate_by;

		// Propagate one-way links
		for step in &level.execution_order {
			match *step {
				DetectorOrGroup::Group(group_id) => {
					if group_rotations[group_id] == 0 {
						continue;
					}
					for link in &level.groups[group_id].linked_groups {
						group_rotations[link.target_group] +=
							link.direction * group_rotations[group_id] * link.multiplicity as i64
					}
					for &detector_cycle_id in &level.groups[group_id].outgoing_detector_cycles {
						// Gather data
						let Some(detector_cycle) = level.cycles.get(detector_cycle_id) else {
							return Err(GameStateActionError::BrokenLevelData(
								"outgoing_detector_cycles references cycle out of range",
							));
						};
						let n_vertices = detector_cycle.vertex_indices.len();
						let n_detectors = detector_cycle.detector_indices.len();
						if n_vertices == 0 || n_detectors == 0 {
							return Err(GameStateActionError::BrokenLevelData("cycle with no vertices or no detectors is somehow in the outgoing_detector_cycles list."));
						}
						// Grab vertex occupancy data from state
						let vertex_occupation = detector_cycle
							.vertex_indices
							.iter()
							.copied()
							.map(|i| {
								self.objects
									.get(i)
									.map(Option::is_some)
									.ok_or(GameStateActionError::MismatchedLevelData)
							})
							.collect::<Result<Vec<_>, _>>()?;
						let n_objects = vertex_occupation.iter().filter(|x| **x).count();
						// Nothing to detect
						if n_objects == 0 {
							continue;
						}
						// Compute rotation characteristics
						let n_rotations =
							detector_cycle.orientation_within_group * group_rotations[group_id];
						let full_turns = n_rotations.signum()
							* i64::div_euclid(n_rotations.abs(), n_vertices as i64);
						let partial_turns = (n_rotations.signum()
							* i64::rem_euclid(n_rotations.abs(), n_vertices as i64))
							as isize;
						for (detector_id, _) in &detector_cycle.detector_indices {
							detector_rotations[*detector_id] += (n_objects as i64) * full_turns;
						}
						if partial_turns != 0 {
							// How long of a strip of vertices needs to be scanned for objects
							let interval_length = partial_turns.unsigned_abs();
							// Counts how many objects are in a interval <i - interval_length, i) (accounting for looping)
							let mut objects_in_interval = vec![0; n_vertices];
							let mut running_total: i32 = 0;
							#[allow(clippy::needless_range_loop)]
							for i in (n_vertices - interval_length)..n_vertices {
								if vertex_occupation[i] {
									running_total += 1;
								}
							}
							for i in 0..n_vertices {
								// Write the value
								objects_in_interval[i] = running_total;
								// Move the interval
								let back_index = (n_vertices - interval_length + i) % n_vertices;
								if vertex_occupation[back_index] {
									running_total -= 1;
								}
								if vertex_occupation[i] {
									running_total += 1;
								}
							}
							for &(detector_id, offset) in &detector_cycle.detector_indices {
								let detections = if partial_turns > 0 {
									objects_in_interval[(offset + 1) % n_vertices]
								} else {
									-objects_in_interval
										[(offset + 1 + interval_length) % n_vertices]
								} as i64;
								detector_rotations[detector_id] += detections;
							}
						}
					}
				}
				DetectorOrGroup::Detector(detector_id) => {
					if detector_rotations[detector_id] == 0 {
						continue;
					}
					for link in &level.detectors[detector_id].linked_groups {
						group_rotations[link.target_group] += link.direction
							* detector_rotations[detector_id]
							* link.multiplicity as i64;
					}
				}
			}
		}

		Ok(group_rotations)
	}

	/// Finds all pairs of cycles that cannot turn at the same time,
	/// but are both being rotated anyway
	///
	/// ## Return Value
	/// Indices into [`LevelData::forbidden_group_pairs`] where both groups turned
	fn find_cycle_clashes(&self, level: &LevelData, group_rotations: &[i64]) -> Vec<usize> {
		// TODO: Change this code, so that it takes into account the modulo rotations
		// to enable certain mechanisms, Unless this is decided against.
		let mut clashes = Vec::new();
		let mut pair_index = 0;
		for group_a_id in 0..level.groups.len() {
			if group_rotations[group_a_id] != 0 {
				while pair_index < level.forbidden_group_pairs.len() {
					let (a, b, _) = level.forbidden_group_pairs[pair_index];
					if a > group_a_id {
						break;
					}
					if a == group_a_id && group_rotations[b] != 0 {
						clashes.push(pair_index);
					}
					pair_index += 1;
				}
			}
		}
		clashes
	}
}

/// Information about a cycle turning action that took place
#[derive(Event, Clone, Debug, Default)]
pub struct TurnCycleResult {
	/// How many ticks (clockwise) each cycle group turned,
	/// or would have if the rotation was blocked
	///
	/// Can be zipped with [`LevelData::groups`]
	pub groups_turned_by: Vec<i64>,
	/// All pairs of mutually conflicting groups
	/// that would have had to turn simultaneously
	///
	/// Unless empty, the rotation was blocked
	///
	/// Elements can be used to index [`LevelData::forbidden_group_pairs`]
	pub clashes: Vec<usize>,
}

impl TurnCycleResult {
	/// Whether the rotation was blocked by a clash
	pub fn blocked(&self) -> bool {
		!self.clashes.is_empty()
	}

	/// True if the rotation caused a shift in layout,
	/// or would have if the rotation was blocked
	pub fn layout_changed(&self) -> bool {
		self.groups_turned_by
			.iter()
			.any(|rotate_by| *rotate_by != 0)
	}

	/// Iterate the indices to cycles that turned with the amount they turned
	///
	/// Cycles that did not turn are not included
	pub fn cycles_turned_by<'a>(
		&'a self,
		level: &'a LevelData,
	) -> impl Iterator<Item = (usize, i64)> + 'a {
		self.groups_turned_by
			.iter()
			.zip(&level.groups)
			.filter(|&(amount, _)| *amount != 0)
			.flat_map(|(amount, group)| {
				group
					.cycles
					.iter()
					.map(|&(cycle, direction)| (cycle, direction * *amount))
			})
	}

	/// Reorders in-place a sequence of values that correspond
	/// to individual vertices to simulate the turn represented
	/// by this object
	///
	/// ## Parameters
	/// `target_sequence` - the sequence to reorder. Must be zippable
	/// with [`LevelData::groups`]
	pub fn reorder_sequence_by_all_cycle_turns<T: Default>(
		&self,
		level: &LevelData,
		target_sequence: &mut [T],
	) -> Result<(), GameStateActionError> {
		for (group_data, &rotation) in level.groups.iter().zip(&self.groups_turned_by) {
			if rotation == 0 {
				continue;
			}
			for &(target_cycle, relative_direction) in &group_data.cycles {
				let rotate_by = relative_direction * rotation;
				Self::reorder_sequence_by_single_cycle_turn(
					level,
					target_cycle,
					rotate_by,
					target_sequence,
				)?;
			}
		}
		Ok(())
	}

	/// Reorders in-place a sequence of values that correspond
	/// to individual vertices to simulate the turn of a single cycle
	pub fn reorder_sequence_by_single_cycle_turn<T: Default>(
		level_data: &LevelData,
		cycle_index: usize,
		rotate_by: i64,
		target_sequence: &mut [T],
	) -> Result<(), GameStateActionError> {
		if rotate_by == 0 {
			return Ok(());
		}
		let Some(cycle_data) = level_data.cycles.get(cycle_index) else {
			return Err(GameStateActionError::CycleIndexOutOfRange(cycle_index));
		};
		let n_vertices = cycle_data.vertex_indices.len();
		if n_vertices == 0 {
			// No vertices, nothing to turn
			return Ok(());
		}
		let amount = rotate_by.rem_euclid(n_vertices as i64) as usize;
		if amount == 0 {
			// NOOP, we are done
			return Ok(());
		}
		let n_loops = gcd(amount, n_vertices);
		let loop_len = n_vertices / n_loops;
		for loop_id in 0..n_loops {
			let mut cached_object = T::default();
			for i in 0..loop_len + 1 {
				let index = (loop_id + amount * i) % n_vertices;
				let vertex_id = cycle_data.vertex_indices[index];
				let Some(object_on_target_vertex) = target_sequence.get_mut(vertex_id) else {
					return Err(GameStateActionError::MismatchedLevelData);
				};
				std::mem::swap(object_on_target_vertex, &mut cached_object);
			}
		}
		Ok(())
	}
}

/// Error codes returned by operations on level data
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum GameStateActionError {
	CycleIndexOutOfRange(usize),
	MismatchedLevelData,
	BrokenLevelData(&'static str),
}

impl std::error::Error for GameStateActionError {}

impl std::fmt::Display for GameStateActionError {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::CycleIndexOutOfRange(i) => write!(
				f,
				"attempted to access cycle {i}, but there are not that many cycles"
			),
			Self::MismatchedLevelData => write!(
				f,
				"game state method got different LevelData than it was created with"
			),
			Self::BrokenLevelData(reason) => {
				write!(f, "level data invariants have been violated: {reason}")
			}
		}
	}
}

fn gcd(mut a: usize, mut b: usize) -> usize {
	while a > 0 {
		(a, b) = (b % a, a);
	}
	b
}
