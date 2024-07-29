//! This module deals with playing sound effects for in-game events

use super::{assets, audio, prelude::*};

pub(super) fn plugin(app: &mut App) {
	app.add_systems(
		Update,
		play_level_complete_audio.run_if(resource_changed::<IsLevelCompleted>),
	);
	app.observe(play_enter_level_audio);
}

fn play_level_complete_audio(mut commands: Commands, is_completed: Res<IsLevelCompleted>) {
	if is_completed.0 {
		commands.trigger(audio::sfx::PlaySfx::Effect(assets::SfxKey::Victory));
	}
}

fn play_enter_level_audio(_: Trigger<SpawnLevel>, mut commands: Commands) {
	commands.trigger(audio::sfx::PlaySfx::Effect(assets::SfxKey::EnterLevel));
}
