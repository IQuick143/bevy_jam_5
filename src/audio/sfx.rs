use crate::{
	assets::{HandleMap, SfxKey},
	settings::Settings,
};
use bevy::{
	audio::{PlaybackMode, Volume},
	prelude::*,
};

pub(super) fn plugin(app: &mut App) {
	app.add_observer(play_sfx);
}

const MAX_VOLUME_MULTIPLIER: f32 = 4.0;

fn play_sfx(
	trigger: On<PlaySfx>,
	mut commands: Commands,
	sfx_handles: Res<HandleMap<SfxKey>>,
	settings: Res<Settings>,
) {
	let sfx_key = match trigger.event() {
		PlaySfx::Effect(key) => *key,
	};
	commands.spawn((
		AudioPlayer(sfx_handles[&sfx_key].clone()),
		PlaybackSettings {
			mode: PlaybackMode::Despawn,
			volume: Volume::Linear(
				sfx_key.volume_multiplier() * settings.sfx_volume * MAX_VOLUME_MULTIPLIER,
			),
			..default()
		},
	));
}

/// Trigger this event to play a single sound effect.
#[derive(Event)]
pub enum PlaySfx {
	Effect(SfxKey),
}
