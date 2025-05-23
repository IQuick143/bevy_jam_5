use bevy::{
	audio::{PlaybackMode, Volume},
	prelude::*,
};

use crate::assets::{HandleMap, SfxKey};

pub(super) fn plugin(app: &mut App) {
	app.add_observer(play_sfx);
}

fn play_sfx(
	trigger: Trigger<PlaySfx>,
	mut commands: Commands,
	sfx_handles: Res<HandleMap<SfxKey>>,
) {
	let sfx_key = match trigger.event() {
		PlaySfx::Effect(key) => *key,
	};
	commands.spawn((
		AudioPlayer(sfx_handles[&sfx_key].clone_weak()),
		PlaybackSettings {
			mode: PlaybackMode::Despawn,
			volume: Volume::Linear(sfx_key.volume_multiplier()),
			..default()
		},
	));
}

/// Trigger this event to play a single sound effect.
#[derive(Event)]
pub enum PlaySfx {
	Effect(SfxKey),
}
