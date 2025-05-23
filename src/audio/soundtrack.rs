use bevy::{
	audio::{PlaybackMode, Volume},
	prelude::*,
};

use crate::assets::{HandleMap, SoundtrackKey};

pub(super) fn plugin(app: &mut App) {
	app.register_type::<IsSoundtrack>();
	app.add_observer(play_soundtrack);
}

fn play_soundtrack(
	trigger: Trigger<PlaySoundtrack>,
	mut commands: Commands,
	soundtrack_handles: Res<HandleMap<SoundtrackKey>>,
	soundtrack_query: Query<Entity, With<IsSoundtrack>>,
) {
	for entity in &soundtrack_query {
		commands.entity(entity).despawn();
	}

	let soundtrack_key = match trigger.event() {
		PlaySoundtrack::Key(key) => *key,
		PlaySoundtrack::Disable => return,
	};
	commands.spawn((
		AudioPlayer(soundtrack_handles[&soundtrack_key].clone_weak()),
		PlaybackSettings {
			mode: PlaybackMode::Loop,
			volume: Volume::Linear(3.5),
			..default()
		},
		IsSoundtrack,
	));
}

/// Trigger this event to play or disable the soundtrack.
/// Playing a new soundtrack will overwrite the previous one.
/// Soundtracks will loop.
#[allow(dead_code)]
#[derive(Event)]
pub enum PlaySoundtrack {
	Key(SoundtrackKey),
	Disable,
}

/// Marker component for the soundtrack entity so we can find it later.
#[derive(Component, Reflect)]
#[reflect(Component)]
struct IsSoundtrack;
