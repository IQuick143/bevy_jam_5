//! Reusable UI widgets & theming.

pub mod background;
pub mod freeze;
pub mod hover;
pub mod interaction;
pub mod multistate;
pub mod palette;
pub mod screen_fade;
pub mod slider;
mod widgets;

#[allow(unused_imports)]
pub mod prelude {
	pub use super::{
		char_input_pressed,
		freeze::{ui_not_frozen, FreezeUi},
		interaction::{InteractionPalette, InteractionQuery},
		palette as ui_palette,
		screen_fade::{AddFadeMessage as _, FadeAnimation, FadeAnimationBundle},
		widgets::{Containers as _, Widgets as _},
	};
}

use bevy::{
	input::keyboard::{Key, KeyboardInput},
	prelude::*,
};

pub fn char_input_pressed(c: char) -> impl Fn(MessageReader<KeyboardInput>) -> bool {
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
		slider::plugin,
		multistate::plugin,
	));
}
