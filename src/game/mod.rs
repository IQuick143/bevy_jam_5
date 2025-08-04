//! Game mechanics and content.

mod animation;
pub mod components;
pub mod drawing;
mod game_sfx;
mod hints;
pub mod history;
mod inputs;
pub mod level;
pub mod logic;
pub mod spawn;
mod synchronization;
#[cfg(test)]
pub mod test;

pub mod prelude {
	#[allow(unused_imports)]
	pub use super::{
		components::LevelScoped,
		history::{MoveHistory, UndoMove},
		level::LevelData,
		logic::{
			CycleTurningDirection, GameLayoutChanged, IsLevelCompleted, LevelCompletionConditions,
			RotateCycle,
		},
		spawn::{EnterLevel, LevelInitialization, LevelInitializationSet},
		synchronization::IsLevelPersistentlyCompleted,
	};
	pub use bevy::prelude::*;
}

use bevy::prelude::*;

pub(super) fn plugin(app: &mut App) {
	app.add_plugins((
		spawn::plugin,
		logic::plugin,
		animation::plugin,
		drawing::plugin,
		inputs::plugin,
		game_sfx::plugin,
		history::plugin,
		hints::plugin,
		synchronization::plugin,
	));
}
