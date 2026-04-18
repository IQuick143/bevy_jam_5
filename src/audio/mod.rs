pub mod sfx;
pub mod soundtrack;

use crate::settings::Settings;
use bevy::prelude::*;

pub fn plugin(app: &mut App) {
	app.add_plugins((sfx::plugin, soundtrack::plugin))
		.add_systems(Update, update_loudness);
}

use bevy_seedling::prelude::*;
const MAX_VOLUME: f32 = 4.0;

fn update_loudness(
	settings: Res<Settings>,
	mut music: Single<&mut VolumeNode, (With<SamplerPool<MusicPool>>, Without<SoundEffectsBus>)>,
	mut sfx: Single<&mut VolumeNode, With<SoundEffectsBus>>,
) {
	fn set_volume(node: &mut Mut<VolumeNode>, amount: Volume) {
		if node.volume != amount {
			node.volume = amount;
		}
	}
	set_volume(
		&mut music,
		Volume::Linear(settings.soundtrack_volume * MAX_VOLUME),
	);
	set_volume(&mut sfx, Volume::Linear(settings.sfx_volume * MAX_VOLUME));
}
