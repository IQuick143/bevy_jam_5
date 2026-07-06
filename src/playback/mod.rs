//! Facilities for creating gameplay animations directly inside the game

mod animate;
mod asset;
mod backend;
mod script;
mod ui;

use bevy::prelude::*;

pub(super) fn plugin(app: &mut App) {
	app.add_plugins((animate::plugin, asset::plugin, script::plugin, ui::plugin));
}
