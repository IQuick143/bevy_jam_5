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
VERTEX b1 b2 b3 bgi bgo bri bro r1 r2 r3 rgi rgo g1 g2 g3

CYCLE[MANUAL] blue b1 b2 b3 bgo bri bgi bro
CYCLE[MANUAL] red r1 r2 r3 bro rgi bri rgo
CYCLE[MANUAL] green g1 g2 g3 rgo bgi rgi bgo

OBJECT[BOX] b2 r2 g2
OBJECT[BUTTON] rgi bri bgi
OBJECT[PLAYER] rgi bri bgi
OBJECT[FLAG] b2 r2 g2

PLACE blue -200 280 300
PLACE red 0 0 300
PLACE green 200 280 300
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
