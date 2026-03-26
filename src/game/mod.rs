//! Game mechanics and content.

pub mod animation;
pub mod components;
pub mod drawing;
mod game_sfx;
mod hints;
pub mod history;
mod inputs;
pub mod level;
pub mod logic;
pub mod logic_relay;
pub mod spawn;
mod synchronization;
#[cfg(test)]
pub mod test;

pub mod prelude {
	#[allow(unused_imports)]
	pub use super::{
		components::{GameStateEcsIndex, LevelHandle, PlayingLevelData},
		history::{AlterHistory, MoveHistory},
		level::LevelData,
		logic::{GameState, LevelCompletionConditions, TurnCycleResult},
		logic_relay::{GameLayoutChanged, IsLevelCompleted, RotateCycle},
		spawn::{EnterLevel, LevelInitialization, LevelInitializationSet},
		synchronization::IsLevelPersistentlyCompleted,
	};
	pub use bevy::prelude::*;
}

use bevy::prelude::*;

pub(super) fn plugin(app: &mut App) {
	app.add_plugins((
		spawn::plugin,
		logic_relay::plugin,
		animation::plugin,
		drawing::plugin,
		inputs::plugin,
		game_sfx::plugin,
		history::plugin,
		hints::plugin,
		synchronization::plugin,
	));
}
