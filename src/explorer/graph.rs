//! Explores the state graph by simulating moves in a level

use crate::game::{
	level::{CycleTurnability, LevelData, ObjectData},
	logic::GameState,
};
use bevy::platform::collections::{hash_map::Entry, HashMap, HashSet};
use std::collections::VecDeque;

#[derive(Clone, Copy, Debug)]
pub struct StateExplorerOptions {
	pub max_node_count: Option<usize>,
	pub max_depth: Option<usize>,
	pub reduce: bool,
}

impl Default for StateExplorerOptions {
	fn default() -> Self {
		Self {
			max_depth: None,
			max_node_count: Some(10000),
			reduce: false,
		}
	}
}

/// Describes predefined equivalences that can be used
/// to reduce a state graph
#[derive(Clone, Copy, PartialEq, Eq, Debug, Default)]
enum StateExplorerReduction {
	/// No reduction is applied
	#[default]
	None,
	/// Disregard colors of boxes
	BoxColors,
	/// Disregard the existence of boxes entirely
	Boxes,
	/// All is one
	Singularity,
}

/// Special attributes of game states
#[derive(Clone, Copy, PartialEq, Eq, Debug, Default)]
pub struct GameStateAttributes {
	/// True if the state is the initial state
	pub is_initial: bool,
	/// True if the state is a solution to the level
	pub is_solution: bool,
	/// True if the state has been resolved
	/// and all its neighbors have been discovered
	pub is_closed: bool,
}

#[derive(Clone, Copy, Debug, Default)]
pub enum ExplorerStopReason {
	#[default]
	Exhaustive,
	DepthLimit,
	StateLimit,
}

/// Graph of the state space of a level
#[derive(Clone, Debug, Default)]
pub struct StateGraph {
	/// All states that can be created with valid moves
	/// and special properties of the states
	pub reachable_states: HashMap<GameState, GameStateAttributes>,
	/// Pairs of states that are connected by a valid move
	/// and index of the group that turns to make that move
	pub moves: Vec<([GameState; 2], usize)>,
	/// Number of moves to reach the nearest solution
	pub first_solution: Option<usize>,
	/// What caused the explorer to stop
	pub stop_reason: ExplorerStopReason,
}

impl StateGraph {
	pub fn traverse_state_graph(level: &LevelData, options: StateExplorerOptions) -> Self {
		let reduction = if options.reduce {
			level.get_max_lossless_reduction()
		} else {
			StateExplorerReduction::None
		};

		let mut graph = Self::default();
		let mut queue = VecDeque::new();

		let initial_state = GameState::new(level).canonical_under_reduction(reduction);
		let is_solution = initial_state.get_completion(level).is_level_completed();
		graph.reachable_states.insert(
			initial_state.clone(),
			GameStateAttributes {
				is_initial: true,
				is_solution,
				is_closed: false,
			},
		);
		if is_solution {
			graph.first_solution = Some(0);
		}
		queue.push_back((initial_state, 0));

		let mut iterations = 0;
		while let Some((state, depth)) = queue.pop_front() {
			let mut resolved_this_turn = HashSet::<GameState>::new();
			for (cycle_index, cycle) in level.cycles.iter().enumerate() {
				// Only legal moves are interesting
				if !state.is_cycle_turnable(level, cycle_index).unwrap() {
					continue;
				}
				for rotate_by in [-1, 1] {
					let mut next_state = state.clone();
					next_state
						.turn_cycle_with_links(level, cycle_index, rotate_by)
						.unwrap();
					// Moves that are no-op are not interesting
					if state == next_state {
						continue;
					}
					match graph.reachable_states.entry(next_state.clone()) {
						Entry::Occupied(entry) => {
							if !entry.get().is_closed && !resolved_this_turn.contains(&next_state) {
								graph
									.moves
									.push(([state.clone(), next_state.clone()], cycle.group));
							}
							continue;
						}
						Entry::Vacant(entry) => {
							let is_solution = next_state.get_completion(level).is_level_completed();
							entry.insert(GameStateAttributes {
								is_initial: false,
								is_solution,
								is_closed: false,
							});
							graph
								.moves
								.push(([state.clone(), next_state.clone()], cycle.group));
							if is_solution && graph.first_solution.is_none() {
								graph.first_solution = Some(depth + 1);
							}
						}
					}
					// Do not do anything with the state until next turn
					// to prevent duplicate links
					resolved_this_turn.insert(next_state.clone());
					// Resolve the state later
					if options.max_depth.is_none_or(|max_depth| depth < max_depth) {
						queue.push_back((next_state, depth + 1));
					} else {
						graph.stop_reason = ExplorerStopReason::DepthLimit;
					}
				}
			}
			// Close the state, all of its neighbors have been pushed now
			graph.reachable_states.get_mut(&state).unwrap().is_closed = true;

			// Break early if the iteration limit has been reached
			iterations += 1;
			if options.max_node_count.is_some_and(|i| iterations > i) {
				graph.stop_reason = ExplorerStopReason::StateLimit;
				break;
			}
		}

		graph
	}
}

impl LevelData {
	fn get_max_lossless_reduction(&self) -> StateExplorerReduction {
		let has_detectors = self
			.cycles
			.iter()
			.any(|c| !c.detector_indices.is_empty() || !c.wall_indices.is_empty());
		if has_detectors {
			return StateExplorerReduction::BoxColors;
		}

		let has_player_cycles = self
			.cycles
			.iter()
			.any(|c| c.turnability == CycleTurnability::WithPlayer);
		if has_player_cycles {
			return StateExplorerReduction::Boxes;
		}

		StateExplorerReduction::Singularity
	}
}

impl GameState {
	fn canonical_under_reduction(mut self, reduction: StateExplorerReduction) -> Self {
		for object in &mut self.objects {
			*object = match reduction {
				StateExplorerReduction::None => *object,
				StateExplorerReduction::BoxColors => {
					// Turn all boxes into wildcard boxes
					if matches!(object, Some(ObjectData::Box(_))) {
						Some(ObjectData::Box(None))
					} else {
						*object
					}
				}
				StateExplorerReduction::Boxes => {
					// Turn everything that is not player into boxes
					if *object == Some(ObjectData::Player) {
						*object
					} else {
						Some(ObjectData::Box(None))
					}
				}
				StateExplorerReduction::Singularity => None,
			}
		}
		self
	}
}
