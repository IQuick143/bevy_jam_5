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
		}
	}

	/// The canonical level order
	pub const LEVEL_ORDER: [LevelID; 22] = [
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
