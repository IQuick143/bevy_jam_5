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
	pub use bevy::prelude::*;
}

use bevy::prelude::*;

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub enum LevelID {
	Intro,
	Boxes,
	Manual,
	Cycle,
	Bicycle,
	Swap,
	Tricycle,
	CargoTricycle,
	CargoSinglePlayer,
	SquareCycle,
	DiamondCycle,
	Lotus,
	ThreeInARow,
	TripleRing,
	Car,
	Olympic,
	Pedalo,
	Disrupt,
	Pyramid,
	Teamwork,
}

impl LevelID {
	/// Get the level that comes after a specific one, if any
	pub fn next_level(self) -> Option<Self> {
		match self {
			Self::Intro => Some(Self::Boxes),
			Self::Boxes => Some(Self::Manual),
			Self::Manual => Some(Self::Cycle),
			Self::Cycle => Some(Self::Bicycle),
			Self::Bicycle => Some(Self::Swap),
			Self::Swap => Some(Self::Tricycle),
			Self::Tricycle => Some(Self::CargoTricycle),
			Self::CargoTricycle => Some(Self::CargoSinglePlayer),
			Self::CargoSinglePlayer => Some(Self::SquareCycle),
			Self::SquareCycle => Some(Self::DiamondCycle),
			Self::DiamondCycle => Some(Self::Lotus),
			Self::Lotus => Some(Self::ThreeInARow),
			Self::ThreeInARow => Some(Self::TripleRing),
			Self::TripleRing => Some(Self::Car),
			Self::Car => Some(Self::Olympic),
			Self::Olympic => Some(Self::Pedalo),
			Self::Pedalo => Some(Self::Disrupt),
			Self::Disrupt => Some(Self::Pyramid),
			Self::Pyramid => Some(Self::Teamwork),
			Self::Teamwork => None,
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
		graphics::plugin,
		inputs::plugin,
	));
	app.add_event::<events::GameLayoutChanged>();
	app.add_event::<events::RotateCycleGroup>();
	app.add_event::<events::RotateSingleCycle>();
}
