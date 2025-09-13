use crate::{
	assets::{HandleMap, SoundtrackKey},
	settings::Settings,
};
use bevy::{
	audio::{PlaybackMode, Volume},
	prelude::*,
};

pub(super) fn plugin(app: &mut App) {
	app.add_observer(play_soundtrack).add_systems(
		Update,
		update_soundtrack_volume.run_if(resource_changed::<Settings>),
	);
}

const MAX_VOLUME: f32 = 4.0;

fn play_soundtrack(
	trigger: On<PlaySoundtrack>,
	mut commands: Commands,
	soundtrack_handles: Res<HandleMap<SoundtrackKey>>,
	soundtrack_query: Query<Entity, With<IsSoundtrack>>,
	settings: Res<Settings>,
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
			volume: Volume::Linear(settings.soundtrack_volume * MAX_VOLUME),
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

fn update_soundtrack_volume(
	mut query: Query<&mut AudioSink, With<IsSoundtrack>>,
	settings: Res<Settings>,
) {
	for mut soundtrack in &mut query {
		soundtrack.set_volume(Volume::Linear(settings.soundtrack_volume * MAX_VOLUME));
	}
}
