mod assets;
mod audio;
#[cfg(feature = "dev")]
mod dev_tools;
mod epilang;
mod game;
mod graphics;
mod screen;
mod ui;

use bevy::{
	asset::AssetMetaCheck,
	audio::{AudioPlugin, Volume},
	prelude::*,
};

pub struct AppPlugin;

impl Plugin for AppPlugin {
	fn build(&self, app: &mut App) {
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
			game::plugin,
			screen::plugin,
			ui::plugin,
			audio::plugin,
			assets::plugin,
			graphics::plugin,
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

/// System that sends an event every time it runs.
/// Use together with input-based run conditions to send input events
fn send_event<E: Event + Clone>(event: E) -> impl Fn(EventWriter<E>) {
	move |mut events| {
		events.write(event.clone());
	}
}
