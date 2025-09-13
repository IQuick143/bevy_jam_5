mod assets;
mod audio;
mod camera;
#[cfg(feature = "dev")]
mod dev_tools;
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
						title: "Ptolemy's Epicycles".to_string(),
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

/// System that sends an message every time it runs.
/// Use together with input-based run conditions to send input messages
fn send_message<E: Message + Clone>(message: E) -> impl Fn(MessageWriter<E>) {
	move |mut messages| {
		messages.write(message.clone());
	}
}
