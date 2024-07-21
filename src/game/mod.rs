//! Game mechanics and content.

pub mod assets;
pub mod audio;
pub mod components;
pub mod events;
pub mod level;
pub mod spawn;
pub mod logic;

pub mod prelude {
	pub use super::components::*;
	pub use super::events::*;
	pub use bevy::prelude::*;
}

use bevy::prelude::*;
use level::ValidLevelData;
use spawn::level::SpawnLevel;

use crate::screen::Screen;

pub(super) fn plugin(app: &mut App) {
	app.add_plugins((audio::plugin, assets::plugin, spawn::plugin, logic::plugin));
	app.add_systems(OnEnter(Screen::Playing), load_level);
}

fn load_level(mut commands: Commands) {
	let data = r"
VERTEX a b c x 1 2 3 

CYCLE[MANUAL] cycle_a a b c x
CYCLE[ENGINE] cycle_b x 1 2 3

OBJECT[BOX] x a
OBJECT[FLAG] 2
OBJECT[PLAYER] b
OBJECT[BUTTON] b
";
	let level: ValidLevelData = level::parser::parse(data).unwrap().try_into().unwrap();
	commands.trigger(SpawnLevel(level));
}
