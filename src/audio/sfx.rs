use crate::assets::{HandleMap, SfxKey};
use bevy::prelude::*;
use bevy_seedling::prelude::*;

pub(super) fn plugin(app: &mut App) {
	app.add_observer(play_sfx);
}

fn play_sfx(trigger: On<PlaySfx>, mut commands: Commands, sfx_handles: Res<HandleMap<SfxKey>>) {
	let sfx_key = match trigger.event() {
		PlaySfx::Effect(key) => *key,
	};
	commands.spawn((
		DefaultPool,
		SamplePlayer {
			sample: sfx_handles[&sfx_key].clone(),
			repeat_mode: RepeatMode::PlayOnce,
			volume: Volume::Linear(sfx_key.volume_multiplier()),
		},
	));
}

/// Trigger this event to play a single sound effect.
#[derive(Event)]
pub enum PlaySfx {
	Effect(SfxKey),
}
