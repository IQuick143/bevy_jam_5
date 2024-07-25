//! Game mechanics and content.

pub mod animation;
pub mod assets;
pub mod audio;
pub mod components;
pub mod events;
pub mod graphics;
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
use level::ValidLevelData;
use spawn::level::SpawnLevel;

use crate::screen::Screen;

pub(super) fn plugin(app: &mut App) {
	app.add_plugins((
		audio::plugin,
		assets::plugin,
		spawn::plugin,
		logic::plugin,
		animation::plugin,
		graphics::plugin,
	));
	app.add_systems(OnEnter(Screen::Playing), load_level);
	app.add_event::<events::GameLayoutChanged>();
	app.add_event::<events::RotateCycleGroup>();
	app.add_event::<events::RotateSingleCycle>();
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

PLACE cycle_a -100 0 100
PLACE cycle_b +100 0 100
";
	let level_file = level::parser::parse(data).unwrap();
	let level: ValidLevelData = level_file.data.try_into().unwrap();
	let mut layout_builder = level::layout::LevelLayoutBuilder::new(&level);
	for placement in level_file.layout {
		layout_builder.add_placement(placement).unwrap();
	}
	let level_layout = layout_builder.build().unwrap();
	eprintln!("{level_layout:?}");
	commands.trigger(SpawnLevel(level, level_layout));
}
