// Disable console on Windows for non-dev builds.
#![cfg_attr(not(feature = "dev"), windows_subsystem = "windows")]

use bevy::ecs::error::{warn, GLOBAL_ERROR_HANDLER};
use bevy::prelude::*;
use bevy_cycle_permutation_puzzle::{
	explorer::{run_state_explorer, StateExplorerOptions},
	AppPlugin,
};

fn start_game() -> AppExit {
	// Configure the ECS error handler to not explode, hopefully.
	GLOBAL_ERROR_HANDLER
		.set(warn)
		.expect("The error handler can only be set once, globally.");

	App::new().add_plugins(AppPlugin).run()
}

fn explore_state_space(level_file: &str, options: StateExplorerOptions) -> AppExit {
	let result = std::fs::read_to_string(level_file)
		.map_err(|e| format!("Could not open {level_file}: {e}"))
		.and_then(|source| run_state_explorer(&source, &mut std::io::stdout().lock(), options));
	match result {
		Ok(()) => AppExit::Success,
		Err(err) => {
			eprintln!("{err}");
			AppExit::error()
		}
	}
}

fn parse_command_line(
	argv: impl IntoIterator<Item = String>,
) -> Result<(String, StateExplorerOptions), &'static str> {
	let mut argv = argv.into_iter();
	let mut level_file = None;
	let mut options = StateExplorerOptions::default();

	while let Some(arg) = argv.next() {
		match arg.as_str() {
			"--max-nodes" => {
				let Some(next_arg) = argv.next() else {
					return Err("expected argument after --max-nodes");
				};
				let Ok(max_nodes) = next_arg.parse() else {
					return Err("expected number after --max-nodes");
				};
				options.max_node_count = Some(max_nodes);
			}
			"--max-depth" => {
				let Some(next_arg) = argv.next() else {
					return Err("expected argument after --max-depth");
				};
				let Ok(max_depth) = next_arg.parse() else {
					return Err("expected number after --max-depth");
				};
				options.max_depth = Some(max_depth);
			}
			_ => {
				if level_file.is_some() {
					return Err("duplicate argument for input level file");
				}
				level_file = Some(arg);
			}
		}
	}

	let Some(level_file) = level_file else {
		return Err("missing input level file");
	};

	Ok((level_file, options))
}

fn main() -> AppExit {
	let argv = std::env::args();
	match argv.len() {
		0..=1 => start_game(),
		2.. => match parse_command_line(argv.skip(1)) {
			Ok((level_file, options)) => explore_state_space(&level_file, options),
			Err(err) => {
				eprintln!("Error: {err}");
				AppExit::error()
			}
		},
	}
}
