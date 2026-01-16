//! Utilities to support playtesting

mod log;
mod playing;
mod stars;
mod title;

use bevy::prelude::*;

pub(super) fn plugin(app: &mut App) {
	app.add_plugins((log::plugin, playing::plugin, stars::plugin, title::plugin));
}
