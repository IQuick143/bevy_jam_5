//! Tools for exploring the state space

use crate::game::{
	level::{backend::builder::parse_and_run, LevelData, ObjectData},
	logic::GameState,
};
use bevy::platform::collections::{hash_map::Entry, HashMap, HashSet};
use std::{collections::VecDeque, io::Write};

pub fn run_state_explorer(level_source: &str, output: &mut impl Write) -> Result<(), String> {
	let level = parse_and_run(level_source, |_| {})
		.relaxed()
		.map_err(|e| format!("Could not build level: {e}"))?;
	let graph = StateGraph::traverse_state_graph(&level, None);
	graph
		.wrap_in_html_page(output)
		.map_err(|e| format!("Could not save explorer result: {e}"))
}

impl GameState {
	fn state_id(&self) -> String {
		let mut buf = std::io::BufWriter::new(Vec::new());
		for object in &self.objects {
			match object {
				None => write!(buf, "n").unwrap(),
				Some(ObjectData::Player) => write!(buf, "p").unwrap(),
				Some(ObjectData::Box(None)) => write!(buf, "b").unwrap(),
				Some(ObjectData::Box(Some(color))) => {
					if color.is_pictogram {
						write!(buf, "c{}", color.color_index).unwrap();
					} else {
						write!(buf, "b{}", color.color_index).unwrap();
					}
				}
			}
		}
		let bytes = buf.into_inner().unwrap();
		String::from_utf8(bytes).unwrap()
	}
}

/// Special attributes of game states
#[derive(Clone, Copy, PartialEq, Eq, Debug, Default)]
struct GameStateAttributes {
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
struct StateGraph {
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

	pub fn wrap_in_html_page(&self, writer: &mut impl std::io::Write) -> std::io::Result<()> {
		write!(
			writer,
			r#"<!doctype html>
<html>
    <head>
        <!-- https://github.com/vasturiano/3d-force-graph -->
        <script src="https://cdn.jsdelivr.net/npm/3d-force-graph"></script>
        <script>
            addEventListener('load', () => {{
                const main = document.getElementById('main')
                const graph = new ForceGraph3D(main);
                graph.linkOpacity(0.5);
                graph.graphData({self});
            }});
        </script>
        <style>
            body, html {{
                margin: 0;
            }}
            #main {{
                height: 100%;
            }}
        </style>
    </head>
    <body>
        <div id="main"></div>
    </body>
</html>
"#
		)
	}
}

impl std::fmt::Display for StateGraph {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		f.write_str("{\n\t\"nodes\": [\n")?;
		for (node, attrs) in &self.reachable_states {
			let id = node.state_id();
			writeln!(
				f,
				"\t\t{{\"id\": \"{id}\", \"name\": \"{id}\", \"color\": \"{}\"}},",
				state_attributes_to_color(attrs)
			)?;
		}
		f.write_str("\t],\n\t\"links\": [\n")?;
		for ([from, to], group) in &self.moves {
			writeln!(
				f,
				"\t\t{{\"source\": \"{}\", \"target\": \"{}\", \"color\": \"{}\"}},",
				from.state_id(),
				to.state_id(),
				group_index_to_color(*group)
			)?;
		}
		f.write_str("\t]\n}\n")?;
		Ok(())
	}
}

fn group_index_to_color(group: usize) -> &'static str {
	const GROUP_TO_COLOR: &[&str] = &[
		"white",
		"cyan",
		"yellow",
		"violet",
		"lime",
		"orange",
		"red",
		"goldenrod",
		"seagreen",
		"aquamarine",
	];
	GROUP_TO_COLOR[group % GROUP_TO_COLOR.len()]
}

fn state_attributes_to_color(attrs: &GameStateAttributes) -> &'static str {
	if attrs.is_initial {
		"red"
	} else if attrs.is_solution {
		"lime"
	} else if attrs.is_closed {
		""
	} else {
		"darkslateblue"
	}
}
