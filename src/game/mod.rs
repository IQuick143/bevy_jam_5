//! Game mechanics and content.

pub mod assets;
pub mod audio;
pub mod components;
pub mod events;
pub mod spawn;
pub mod level;

pub mod prelude {
	pub use super::components::*;
	pub use super::events::*;
	pub use bevy::prelude::*;
}

use bevy::app::App;

pub(super) fn plugin(app: &mut App) {
	app.add_plugins((audio::plugin, assets::plugin, spawn::plugin));
}
