//! Utilities to support playtesting

mod consts;
mod log;
mod main_form;
mod playing;
mod privacy;
mod recording;
mod stars;
mod submit;
mod title;
mod widgets;

use bevy::prelude::*;

pub(super) fn plugin(app: &mut App) {
	app.add_plugins((
		bevy_ui_text_input::TextInputPlugin,
		log::plugin,
		main_form::plugin,
		playing::plugin,
		privacy::plugin,
		recording::plugin,
		stars::plugin,
		submit::plugin,
		title::plugin,
	));
}
