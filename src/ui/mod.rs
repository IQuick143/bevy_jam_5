//! Reusable UI widgets & theming.

// Unused utilities and re-exports may trigger these lints undesirably.
#![allow(dead_code, unused_imports)]

pub mod hud;
pub mod interaction;
pub mod palette;
mod widgets;
pub mod screen_fade;

pub mod prelude {
	pub use super::{
		hud,
		interaction::{InteractionPalette, InteractionQuery},
		palette as ui_palette,
		widgets::{Containers as _, Widgets as _},
	};
}

use bevy::prelude::*;

pub(super) fn plugin(app: &mut App) {
	app.add_plugins((interaction::plugin, hud::plugin, screen_fade::plugin));
}
