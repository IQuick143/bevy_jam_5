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
	Sync2,
	Colors,
	Bicycle,
	Swap,
	Sort,
	Tricycle,
	CargoTricycle,
	CargoSinglePlayer,
	Lotus,
	ThreeInARowSimple,
	ThreeInARow,
	Car,
	Olympic,
	Disrupt,
	Send,
	Teamwork,
	Sort2,
	Rubik,
	//	#[cfg(not(target = "wasm"))]
	//	Custom, TODO
}

impl LevelID {
	/// Get the level that comes after a specific one, if any
	pub fn next_level(self) -> Option<Self> {
		let mut level_order = Self::LEVEL_ORDER.iter();
		level_order
			.find(|x| **x == self)
			.expect("All levels should be included in canonical order");
		level_order.next().copied()
	}

	/// Get the display name of a level
	pub fn level_name(self) -> &'static str {
		match self {
			Self::Intro => "Intro",
			Self::Transfer => "Transfer",
			Self::Boxes => "Boxes",
			Self::Manual => "Manual",
			Self::Sync => "Sync",
			Self::Sync2 => "Sync 2",
			Self::Colors => "Colors",
			Self::Bicycle => "Bicycle",
			Self::Swap => "Swap",
			Self::Sort => "Sort",
			Self::Tricycle => "Tricycle",
			Self::CargoTricycle => "Tricycle 2",
			Self::CargoSinglePlayer => "Tricycle 3",
			Self::Lotus => "Lotus",
			Self::ThreeInARowSimple => "Grid",
			Self::ThreeInARow => "Grid EX",
			Self::Car => "Car",
			Self::Olympic => "Olympic",
			Self::Disrupt => "Disrupt",
			Self::Send => "Send",
			Self::Teamwork => "Teamwork",
			Self::Sort2 => "Sort 2",
			Self::Rubik => "Rubik",
		}
	}

	/// Get the hint for a level
	pub fn level_hint(self) -> Option<&'static str> {
		match self {
			Self::Intro => None,
			Self::Transfer => None,
			Self::Boxes => None,
			Self::Manual => None,
			Self::Sync => Some("Linkages make cycles move in Sync!"),
			Self::Sync2 => None,
			Self::Colors => None,
			Self::Bicycle => None,
			Self::Swap => None,
			Self::Sort => None,
			Self::Tricycle => None,
			Self::CargoTricycle => None,
			Self::CargoSinglePlayer => None,
			Self::Lotus => Some("In compliance with the Aperture Science regulatory body, I am legally forced to inform you, this level is looking kinda good."),
			Self::ThreeInARowSimple => Some("Tip: Think about the paths the player can take and plan them ahead."),
			Self::ThreeInARow => Some("I hope you enjoyed Grid."),
			Self::Car => Some("Fun fact: Originally this level was thought to be impossible!"),
			Self::Olympic => Some("Tip: In levels with a single player and manual cycles, it's helpful to think about the player's routes through the crossings."),
			Self::Disrupt => Some("So close, yet so far... Tip: Modular arithmetic."),
			Self::Send => None,
			Self::Teamwork => Some("Fact: Teamwork makes the dream work, sometimes."),
			Self::Sort2 => None,
			Self::Rubik => None,
		}
	}

	/// The canonical level order
	pub const LEVEL_ORDER: [LevelID; 23] = [
		Self::Intro,
		Self::Transfer,
		Self::Boxes,
		Self::Manual,
		Self::Sync,
		Self::Sync2,
		Self::Colors,
		Self::Bicycle,
		Self::Swap,
		Self::Sort,
		Self::Tricycle,
		Self::CargoTricycle,
		Self::CargoSinglePlayer,
		Self::Lotus,
		Self::ThreeInARowSimple,
		Self::Olympic,
		Self::Disrupt,
		Self::Send,
		Self::Teamwork,
		Self::Sort2,
		Self::ThreeInARow,
		Self::Car,
		Self::Rubik,
	];
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
