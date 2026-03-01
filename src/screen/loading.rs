//! A loading screen during which game assets are loaded.
//! This reduces stuttering, especially for audio on WASM.

use super::Screen;
use crate::{assets::*, audio::soundtrack::PlaySoundtrack, ui::prelude::*};
use bevy::prelude::*;

pub(super) fn plugin(app: &mut App) {
	app.add_systems(OnEnter(Screen::Loading), enter_loading)
		.add_systems(OnExit(Screen::Loading), start_gameplay_soundtrack);

	// Playtest build will plug in its own proceed system
	#[cfg(not(feature = "playtest"))]
	app.add_systems(
		Update,
		(|mut s: ResMut<NextState<_>>| s.set(Screen::Title))
			.run_if(in_state(Screen::Loading).and(all_assets_loaded)),
	);
}

fn enter_loading(mut commands: Commands, font: Res<GlobalFont>) {
	commands.spawn((
		widgets::ui_root(),
		DespawnOnExit(Screen::Loading),
		children![widgets::label("Loading...", font.0.clone())],
	));
}

fn start_gameplay_soundtrack(mut commands: Commands) {
	commands.trigger(PlaySoundtrack::Key(SoundtrackKey::Gameplay));
}
