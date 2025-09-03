// Disable console on Windows for non-dev builds.
#![cfg_attr(not(feature = "dev"), windows_subsystem = "windows")]

use bevy::ecs::error::{warn, GLOBAL_ERROR_HANDLER};
use bevy::prelude::*;
use bevy_cycle_permutation_puzzle::{explorer, AppPlugin};

fn start_game() -> AppExit {
	// Configure the ECS error handler to not explode, hopefully.
	GLOBAL_ERROR_HANDLER
		.set(warn)
		.expect("The error handler can only be set once, globally.");

	App::new().add_plugins(AppPlugin).run()
}

fn explore_state_space(level_file: &str) -> AppExit {
	let level_source = match std::fs::read_to_string(level_file) {
		Ok(source) => source,
		Err(err) => {
			eprintln!("Could not open {level_file}: {err}");
			return AppExit::error();
		}
	};
	let level = match explorer::compile_level(&level_source) {
		Ok(level) => level,
		Err(err) => {
			eprintln!("Could not parse level: {err}");
			return AppExit::error();
		}
	};
	let graph = explorer::StateGraph::traverse_state_graph(&level, None);
	graph
		.wrap_in_html_page(&mut std::io::stdout().lock())
		.unwrap();
	AppExit::Success
}

fn main() -> AppExit {
	let mut argv = std::env::args();
	match argv.len() {
		0..=1 => start_game(),
		2 => explore_state_space(&argv.nth(1).unwrap()),
		3.. => {
			eprintln!("Too many arguments. Run without arguments to launch the game, or supply a path to a level file to explore its state space.");
			AppExit::error()
		}
	}
}
