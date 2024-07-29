//! This module deals with playing sound effects for in-game events

use super::{assets, audio, prelude::*};

pub(super) fn plugin(app: &mut App) {
	app.add_systems(
		Update,
		(
			play_level_complete_audio.run_if(resource_changed::<IsLevelCompleted>),
			play_goal_unlocked_audio.run_if(resource_changed::<IsGoalUnlocked>),
		),
	);
}

fn play_level_complete_audio(mut commands: Commands, is_completed: Res<IsLevelCompleted>) {
	if is_completed.0 {
		commands.trigger(audio::sfx::PlaySfx::Effect(assets::SfxKey::Victory));
	}
}

fn play_goal_unlocked_audio(mut commands: Commands, is_open: Res<IsGoalUnlocked>) {
	if is_open.0 {
		commands.trigger(audio::sfx::PlaySfx::Effect(assets::SfxKey::GoalComplete));
	}
}
