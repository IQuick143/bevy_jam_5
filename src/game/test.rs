#[allow(unused_imports)]
use bevy::{ecs::system::RunSystemOnce, prelude::*};

mod utils {
	use crate::game::level::{GlyphType, ObjectType, ThingData};

	use super::super::{
		components::Vertex,
		level::{parser, LevelData},
		spawn::{EnterLevel, LevelInitialization, LevelInitializationSet},
	};
	#[allow(unused_imports)]
	use bevy::{ecs::system::RunSystemOnce, prelude::*};

	pub fn setup_app() -> App {
		let mut app = App::new();
		app.add_plugins((
			AssetPlugin::default(),
			super::super::level::asset::plugin,
			super::super::logic::plugin,
			super::super::history::plugin,
			super::super::spawn::plugin,
		))
		// Disable spawning of visual entities
		.configure_sets(
			LevelInitialization,
			LevelInitializationSet::SpawnVisuals.run_if(|| false),
		);
		app
	}

	/// Resource for extracting vertex information from the game
	#[derive(PartialEq)]
	pub struct VertexDebugData {
		/// How many standard (non-horocycle) vertices there are.
		pub n_vertices: usize, // TODO: Add fields for Horocycle data.
		                       //		pub objects: Vec<Option<ObjectType>>,
		                       //		pub glyphs: Vec<Option<GlyphType>>,
	}

	fn load_level(
		In(level_data): In<&str>,
		mut asset_server: ResMut<Assets<LevelData>>,
		mut spawn_trigger: EventWriter<EnterLevel>,
	) -> Result<(), parser::LevelParsingError> {
		let level = parser::parse(level_data)?;
		let handle = asset_server.add(level);
		spawn_trigger.send(EnterLevel(Some(handle)));
		Ok(())
	}

	pub fn app_with_level(level: &'static str) -> App {
		let mut app = setup_app();
		app.world_mut()
			.run_system_once_with(level, load_level)
			.expect("Level should've parsed!");
		// An update is needed to trigger the spawning logic
		app.update();
		app
	}

	fn read_system(vertices: Query<&Vertex>) -> VertexDebugData {
		VertexDebugData {
			n_vertices: vertices.iter().count(),
		}
	}

	//	fn turn_system(In((id, amount)): In<(usize, i32)>, vertices: Query<&Vertex>) -> () {}

	pub trait GameLogicAppExt {
		fn read_vertices(&mut self) -> VertexDebugData;
		//		fn turn_cycle(&mut self, cycle_id: usize, amount: i32) -> ();
	}

	impl GameLogicAppExt for App {
		fn read_vertices(&mut self) -> VertexDebugData {
			self.world_mut().run_system_once(read_system)
		}

		//		fn turn_cycle(&mut self, cycle_id: usize, amount: i32) -> () {
		//			self.world_mut()
		//				.run_system_once_with((cycle_id, amount), turn_system)
		//		}
	}
}

use utils::*;

/// Metatest for asserting that running the headless game works.
#[test]
fn test_app() {
	let mut app = setup_app();
	for _ in 0..5 {
		app.update();
		let vertex_data = app.read_vertices();
		assert_eq!(vertex_data.n_vertices, 0, "Vertices appeared from nowhere!");
	}
}

/// Metatest for asserting that loading a level works.
#[test]
fn test_debug_level_load() {
	let mut app = app_with_level(
		r"
NAME=Debug1
HINT=A player should not be reading this message!

VERTEX a b c d e f g
CYCLE cycle a b c d e f g

OBJECT[PLAYER] f
OBJECT[FLAG] b

PLACE cycle 0 0 100
",
	);
	let vertex_data = app.read_vertices();
	assert_eq!(vertex_data.n_vertices, 7, "Level should have 7 vertices!");
	app.update();
}

/*
/// Test for basic cycle rotation.
#[test]
fn test_basic_inputs() {
	let mut app = app_with_level(
		r"
NAME=Debug2
HINT=A player should not be reading this message!

VERTEX a b c d e f g
CYCLE cycle a b c d e f g

OBJECT[PLAYER] f
OBJECT[FLAG] b

PLACE cycle 0 0 100
",
	);

	let initial_vertex_data = app.read_vertices();

	for i in 0..6 {
		app.update();
	}
}
*/
