//! This module deals with playing sound effects for in-game events

use super::prelude::*;
use crate::{assets, audio};

pub(super) fn plugin(app: &mut App) {
	app.add_systems(
		Update,
		(
			play_level_complete_audio.run_if(resource_changed::<IsLevelCompleted>),
			play_enter_level_audio,
		),
	);
}

fn play_level_complete_audio(mut commands: Commands, is_completed: Res<IsLevelCompleted>) {
	if is_completed.0 {
		commands.trigger(audio::sfx::PlaySfx::Effect(assets::SfxKey::Victory));
	}
}

fn play_enter_level_audio(mut events: MessageReader<EnterLevel>, mut commands: Commands) {
	if events.read().any(|event| event.0.is_some()) {
		commands.trigger(audio::sfx::PlaySfx::Effect(assets::SfxKey::EnterLevel));
	}
}
