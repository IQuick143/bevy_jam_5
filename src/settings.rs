//! Settings that can be adjusted by the player

use bevy::prelude::*;

pub(super) fn plugin(app: &mut App) {
	app.init_resource::<Settings>();
}

/// Settings that can be adjusted by the player
#[derive(Resource, Debug, Default, Reflect)]
#[reflect(Resource)]
pub struct Settings {
	/// Volume of sound effects, as a fraction of maximum volume
	pub sfx_volume: f32,
	/// Volume of music, as a fraction of maximum volume
	pub soundtrack_volume: f32,
}
