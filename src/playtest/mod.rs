//! Utilities to support playtesting

mod log;
mod main_form;
mod playing;
mod stars;
mod title;

use bevy::prelude::*;

pub(super) fn plugin(app: &mut App) {
	app.add_plugins((
		log::plugin,
		main_form::plugin,
		playing::plugin,
		stars::plugin,
		title::plugin,
	));
}
