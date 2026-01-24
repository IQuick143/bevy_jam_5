//! Utilities to support playtesting

mod consts;
mod log;
mod main_form;
mod playing;
mod stars;
mod title;
mod widgets;

use bevy::prelude::*;

pub(super) fn plugin(app: &mut App) {
	app.add_plugins((
		bevy_ui_text_input::TextInputPlugin,
		log::plugin,
		main_form::plugin,
		playing::plugin,
		stars::plugin,
		title::plugin,
	));
}
