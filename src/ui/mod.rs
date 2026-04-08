//! Reusable UI widgets & theming.

pub mod background;
pub mod consts;
pub mod freeze;
pub mod fullscreen;
pub mod hover;
pub mod interaction;
pub mod multistate;
pub mod screen_fade;
pub mod scrollbox;
pub mod slider;
pub mod widgets;

#[allow(unused_imports)]
pub mod prelude {
	pub use super::{
		freeze::{FreezeUi, IsUiFrozen, ui_not_frozen},
		interaction::{InteractionPalette, InteractionQuery},
		screen_fade::{AddFadeMessage as _, FadeAnimation, FadeAnimationBundle},
		widgets,
	};
}

use bevy::prelude::*;

pub(super) fn plugin(app: &mut App) {
	app.add_plugins((
		interaction::plugin,
		screen_fade::plugin,
		hover::plugin,
		background::plugin,
		slider::plugin,
		multistate::plugin,
		scrollbox::plugin,
		fullscreen::plugin,
	));
}
