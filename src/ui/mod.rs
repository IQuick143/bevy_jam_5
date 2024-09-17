//! Reusable UI widgets & theming.

// Unused utilities and re-exports may trigger these lints undesirably.
#![allow(dead_code, unused_imports)]

pub mod background;
pub mod freeze;
pub mod hover;
pub mod interaction;
pub mod palette;
pub mod screen_fade;
mod widgets;

pub mod prelude {
	pub use super::{
		char_input_pressed,
		freeze::ui_not_frozen,
		interaction::{InteractionPalette, InteractionQuery},
		palette as ui_palette,
		widgets::{Containers as _, Widgets as _},
	};
}

use bevy::{
	input::{
		keyboard::{Key, KeyboardInput},
		ButtonState,
	},
	prelude::*,
};

pub fn char_input_pressed(c: char) -> impl Fn(EventReader<KeyboardInput>) -> bool {
	let key = Key::Character(String::from(c).into());
	move |mut events| {
		events
			.read()
			.last()
			.is_some_and(|input| input.state.is_pressed() && input.logical_key == key)
	}
}

pub(super) fn plugin(app: &mut App) {
	app.add_plugins((
		interaction::plugin,
		screen_fade::plugin,
		hover::plugin,
		background::plugin,
		freeze::plugin,
	));
}
