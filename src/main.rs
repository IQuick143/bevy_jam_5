// Disable console on Windows for non-dev builds.
#![cfg_attr(not(feature = "dev"), windows_subsystem = "windows")]

mod assets;
mod audio;
mod camera;
#[cfg(feature = "dev")]
mod dev_tools;
mod drawing;
mod epilang;
pub mod explorer;
mod game;
mod graphics;
mod persistent;
mod save;
mod screen;
mod settings;
mod ui;

use bevy::{
	asset::AssetMetaCheck,
	audio::{AudioPlugin, Volume},
	ecs::error::warn,
	prelude::*,
};

use crate::explorer::{run_state_explorer, StateExplorerOptions};

pub struct AppPlugin;

impl Plugin for AppPlugin {
	fn build(&self, app: &mut App) {
		// Configure the ECS error handler to not explode, hopefully.
		app.set_error_handler(warn);

		// Order new `AppStep` variants by adding them here:
		app.configure_sets(
			Update,
			(
				AppSet::TickTimers,
				AppSet::RecordInput,
				AppSet::ExecuteInput,
				AppSet::GameLogic,
				AppSet::UpdateVisuals,
			)
				.chain(),
		);

		// Add Bevy plugins.
		app.add_plugins(
			DefaultPlugins
				.set(AssetPlugin {
					// Wasm builds will check for meta files (that don't exist) if this isn't set.
					// This causes errors and even panics on web build on itch.
					// See https://github.com/bevyengine/bevy_github_ci_template/issues/48.
					meta_check: AssetMetaCheck::Never,
					#[cfg(feature = "dev")]
					watch_for_changes_override: Some(true),
					..default()
				})
				.set(WindowPlugin {
					primary_window: Window {
						title: "Epicycles".to_string(),
						canvas: Some("#bevy".to_string()),
						fit_canvas_to_parent: true,
						prevent_default_event_handling: true,
						..default()
					}
					.into(),
					..default()
				})
				.set(AudioPlugin {
					global_volume: GlobalVolume {
						volume: Volume::Linear(0.3),
					},
					..default()
				}),
		);

		// Add other plugins.
		app.add_plugins((
			persistent::plugin,
			save::plugin,
			drawing::plugin,
			game::plugin,
			screen::plugin,
			audio::plugin,
			graphics::plugin,
			assets::plugin,
			ui::plugin,
			settings::plugin,
			camera::plugin,
		));

		// Enable dev tools for dev builds.
		#[cfg(feature = "dev")]
		app.add_plugins(dev_tools::plugin);
	}
}

/// High-level groupings of systems for the app in the `Update` schedule.
/// When adding a new variant, make sure to order it in the `configure_sets`
/// call above.
#[derive(SystemSet, Debug, Clone, Copy, Eq, PartialEq, Hash)]
enum AppSet {
	/// Tick timers.
	TickTimers,
	/// Record player input.
	RecordInput,
	/// Process inputs that correspond to one-shot actions rather than lasting state
	/// (that should be pretty much all inputs in this particular game)
	ExecuteInput,
	/// Evaluate in-game logic
	GameLogic,
	/// Update visual representation of internal state
	UpdateVisuals,
}

/// System that sends a message every time it runs.
/// Use together with input-based run conditions to send input messages
fn send_message<E: Message + Clone>(message: E) -> impl Fn(MessageWriter<E>) {
	move |mut messages| {
		messages.write(message.clone());
	}
}

fn start_game() -> AppExit {
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
	// This is to work around a bug with hot reloading
	let argv: Vec<String> = std::env::args().filter(|arg| !arg.is_empty()).collect();
	match argv.len() {
		0..=1 => start_game(),
		2.. => match parse_command_line(argv.into_iter().skip(1)) {
			Ok((level_file, options)) => explore_state_space(&level_file, options),
			Err(err) => {
				eprintln!("Error: {err}");
				AppExit::error()
			}
		},
	}
}
