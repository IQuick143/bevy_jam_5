//! Game mechanics and content.

pub mod animation;
pub mod assets;
pub mod audio;
pub mod components;
pub mod events;
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
	pub use super::LevelID;
	pub use bevy::prelude::*;
}

use bevy::prelude::*;

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub enum LevelID {
	Intro,
	Transfer,
	Boxes,
	Manual,
	Sync,
	Bicycle,
	Swap,
	Sort,
	Tricycle,
	CargoTricycle,
	CargoSinglePlayer,
	Lotus,
	ThreeInARow,
	Car,
	Olympic,
	Disrupt,
	Send,
	Teamwork,
	Sort2,
	//	#[cfg(not(target = "wasm"))]
	//	Custom, TODO
}

impl LevelID {
	/// Get the level that comes after a specific one, if any
	pub fn next_level(self) -> Option<Self> {
		match self {
			Self::Intro => Some(Self::Transfer),
			Self::Transfer => Some(Self::Boxes),
			Self::Boxes => Some(Self::Manual),
			Self::Manual => Some(Self::Sync),
			Self::Sync => Some(Self::Bicycle),
			Self::Bicycle => Some(Self::Swap),
			Self::Swap => Some(Self::Sort),
			Self::Sort => Some(Self::Tricycle),
			Self::Tricycle => Some(Self::CargoTricycle),
			Self::CargoTricycle => Some(Self::CargoSinglePlayer),
			Self::CargoSinglePlayer => Some(Self::Lotus),
			Self::Lotus => Some(Self::ThreeInARow),
			Self::ThreeInARow => Some(Self::Car),
			Self::Car => Some(Self::Olympic),
			Self::Olympic => Some(Self::Disrupt),
			Self::Disrupt => Some(Self::Send),
			Self::Send => Some(Self::Teamwork),
			Self::Teamwork => Some(Self::Sort2),
			Self::Sort2 => None,
		}
	}
}

pub(super) fn plugin(app: &mut App) {
	app.add_plugins((
		audio::plugin,
		assets::plugin,
		spawn::plugin,
		logic::plugin,
		animation::plugin,
		resources::plugin,
		inputs::plugin,
	));
	app.add_event::<events::GameLayoutChanged>();
	app.add_event::<events::RotateCycleGroup>();
	app.add_event::<events::RotateSingleCycle>();
}
