//! Explores the state graph by simulating moves in a level

use crate::game::{level::LevelData, logic::GameState};
use bevy::platform::collections::{hash_map::Entry, HashMap, HashSet};
use std::collections::VecDeque;

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

/// Graph of the state space of a level
#[derive(Clone, Debug, Default)]
pub struct StateGraph {
	/// All states that can be created with valid moves
	/// and special properties of the states
	pub reachable_states: HashMap<GameState, GameStateAttributes>,
	/// Pairs of states that are connected by a valid move
	/// and index of the group that turns to make that move
	pub moves: Vec<([GameState; 2], usize)>,
}

impl StateGraph {
	pub fn traverse_state_graph(level: &LevelData, max_iterations: Option<usize>) -> Self {
		let mut graph = Self::default();
		let mut queue = VecDeque::new();

		let initial_state = GameState::new(level);
		let is_solution = initial_state.get_completion(level).is_level_completed();
		graph.reachable_states.insert(
			initial_state.clone(),
			GameStateAttributes {
				is_initial: true,
				is_solution,
				is_closed: false,
			},
		);
		queue.push_back(initial_state);

		let mut iterations = 0;
		while let Some(state) = queue.pop_front() {
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
						}
					}
					// Do not do anything with the state until next turn
					// to prevent duplicate links
					resolved_this_turn.insert(next_state.clone());
					// Resolve the state later
					queue.push_back(next_state);
				}
			}
			// Close the state, all of its neighbors have been pushed now
			graph.reachable_states.get_mut(&state).unwrap().is_closed = true;

			// Break early if the iteration limit has been reached
			iterations += 1;
			if max_iterations.is_some_and(|i| iterations > i) {
				break;
			}
		}

		graph
	}
}
