//! Game mechanics and content.

pub mod animation;
pub mod assets;
pub mod audio;
pub mod components;
pub mod events;
mod game_sfx;
pub mod graphics;
pub mod inputs;
pub mod level;
pub mod logic;
pub mod resources;
pub mod spawn;

pub mod prelude {
	pub use super::components::*;
	pub use super::events::*;
	pub use super::resources::*;
	pub use bevy::prelude::*;
}

use bevy::prelude::*;

pub(super) fn plugin(app: &mut App) {
	app.add_plugins((
		audio::plugin,
		assets::plugin,
		spawn::plugin,
		logic::plugin,
		animation::plugin,
		resources::plugin,
		inputs::plugin,
		game_sfx::plugin,
	));
	app.add_event::<events::GameLayoutChanged>();
	app.add_event::<events::RotateCycleGroup>();
	app.add_event::<events::RotateSingleCycle>();
}
