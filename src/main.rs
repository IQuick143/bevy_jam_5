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
mod input;
mod mechanics_overview;
mod persistent;
#[cfg(feature = "playtest")]
mod playtest;
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
use explorer::{StateExplorerOptions, run_state_explorer};
use mechanics_overview::MechanicsOverview;

pub struct AppPlugin;

impl Plugin for AppPlugin {
	fn build(&self, app: &mut App) {
		// Configure the ECS error handler to not explode, hopefully.
		app.set_error_handler(warn);

		// Order new `AppSet` variants by adding them here:
		app.configure_sets(
			Update,
			(
				AppSet::TickTimers,
				AppSet::SwitchState,
				AppSet::RecordInput,
				AppSet::ExecuteInput,
				AppSet::PreGameLogic,
				AppSet::GameLogic,
				AppSet::PostGameLogic,
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
			input::plugin,
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

		// Enable playtest support for test builds
		#[cfg(feature = "playtest")]
		app.add_plugins(playtest::plugin);
	}
}

/// High-level groupings of systems for the app in the `Update` schedule.
/// When adding a new variant, make sure to order it in the `configure_sets`
/// call above.
#[derive(SystemSet, Debug, Clone, Copy, Eq, PartialEq, Hash)]
enum AppSet {
	/// Tick timers.
	TickTimers,
	/// Update global states like screen
	SwitchState,
	/// Record player input.
	RecordInput,
	/// Process inputs that correspond to one-shot actions rather than lasting state
	/// (that should be pretty much all inputs in this particular game)
	ExecuteInput,
	/// Evaluate setup before the main in-game logic
	PreGameLogic,
	/// Evaluate in-game logic
	GameLogic,
	/// Evaluate fallback after the main in-game logic
	PostGameLogic,
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

/// System that triggers an event every time it runs.
/// Use together with input-based run conditions to send input events
fn send_event<E: Event + Clone>(event: E) -> impl Fn(Commands)
where
	for<'a> E::Trigger<'a>: Default,
{
	move |mut commands| {
		commands.trigger(event.clone());
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

fn print_mechanics_overview() -> AppExit {
	match MechanicsOverview::load() {
		Ok(overview) => {
			println!("{overview}");
			AppExit::Success
		}
		Err(err) => {
			eprintln!("{err}");
			AppExit::error()
		}
	}
}

/// Enumerates possible subcommands of the app
enum CommandLine {
	/// Launches a normal game session
	StartGame,
	/// Explores the state space of a level
	/// and prints the results to stdout
	ExploreStates {
		level_file: String,
		options: StateExplorerOptions,
	},
	/// Compiles an overview of what mechanics are present
	/// in each level in the current bundle
	MechanicsOverview,
}

fn parse_command_line(argv: impl IntoIterator<Item = String>) -> Result<CommandLine, &'static str> {
	let mut argv = argv.into_iter();
	let mut level_file = None;
	let mut options = StateExplorerOptions::default();
	let mut has_mechanics_flag = false;

	while let Some(arg) = argv.next() {
		match arg.as_str() {
			"" => continue,
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
			"--reduce" => {
				options.reduce = true;
			}
			"--mechanics" => {
				has_mechanics_flag = true;
			}
			_ => {
				if level_file.is_some() {
					return Err("duplicate argument for input level file");
				}
				level_file = Some(arg);
			}
		}
	}

	if let Some(level_file) = level_file {
		Ok(CommandLine::ExploreStates {
			level_file,
			options,
		})
	} else if has_mechanics_flag {
		Ok(CommandLine::MechanicsOverview)
	} else {
		Ok(CommandLine::StartGame)
	}
}

fn main() -> AppExit {
	match parse_command_line(std::env::args().skip(1)) {
		Ok(CommandLine::StartGame) => start_game(),
		Ok(CommandLine::ExploreStates {
			level_file,
			options,
		}) => explore_state_space(&level_file, options),
		Ok(CommandLine::MechanicsOverview) => print_mechanics_overview(),
		Err(err) => {
			eprintln!("Error: {err}");
			AppExit::error()
		}
	}
}
