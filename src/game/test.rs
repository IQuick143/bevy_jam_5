mod utils {
	use crate::game::{
		components::{CycleEntities, PlacedGlyph, PlacedObject, VertexDebugID},
		level::{GlyphData, ObjectData, ThingData},
		logic::{CycleTurningDirection, RotateCycle, RotateCycleGroup},
	};

	use super::super::{
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
	#[derive(Debug, Clone)]
	pub struct VertexDebugData {
		/// How many standard (non-horocycle) vertices there are.
		pub n_vertices: usize, // TODO: Add fields for Horocycle data.
		pub objects: Vec<Option<ObjectData>>,
		pub glyphs: Vec<Option<GlyphData>>,
	}

	impl PartialEq for VertexDebugData {
		fn eq(&self, other: &Self) -> bool {
			if self.n_vertices == other.n_vertices {
				for i in 0..self.n_vertices {
					// TODO: Rethink checking data which might be irrelevant (like grpahical orientation stored in glyph_data)
					if !(self.objects[i] == other.objects[i] && self.glyphs[i] == other.glyphs[i]) {
						return false;
					}
				}
				return true;
			}
			false
		}
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

	fn read_system(
		vertices: Query<(&VertexDebugID, &PlacedObject, &PlacedGlyph)>,
		things: Query<&ThingData>,
	) -> VertexDebugData {
		let n_vertices = vertices.iter().count();
		let mut glyph_data = vec![None; n_vertices];
		let mut object_data = vec![None; n_vertices];
		for (vertex, object, glyph) in vertices.iter() {
			let i = vertex.0;

			// Extract glyph information
			glyph_data[i] = glyph
				.0
				.map(|e| {
					things
						.get(e)
						.expect("Vertex points to a non-existent entity")
				})
				.map(|thing| match thing {
					ThingData::Object(_object_data) => {
						panic!("Glyph data points to an object, not a glyph")
					}
					ThingData::Glyph(glyph_data) => glyph_data.clone(),
				});
			// Extract glyph information
			object_data[i] = object
				.0
				.map(|e| {
					things
						.get(e)
						.expect("Vertex points to a non-existent entity")
				})
				.map(|thing| match thing {
					ThingData::Object(object_data) => object_data.clone(),
					ThingData::Glyph(_glyph_data) => {
						panic!("Glyph data points to an object, not a glyph")
					}
				});
		}

		VertexDebugData {
			n_vertices,
			objects: object_data,
			glyphs: glyph_data,
		}
	}

	fn turn_system(
		In((id, amount)): In<(usize, i32)>,
		mut events: EventWriter<RotateCycleGroup>,
		cycle_list: Query<&CycleEntities>,
	) -> () {
		events.send(RotateCycleGroup(RotateCycle {
			target_cycle: cycle_list.single().0[id],
			direction: if amount >= 0 {
				CycleTurningDirection::Nominal
			} else {
				CycleTurningDirection::Reverse
			},
			amount: amount.abs() as usize,
		}));
	}

	pub trait GameLogicAppExt {
		fn read_vertices(&mut self) -> VertexDebugData;
		fn turn_cycle(&mut self, cycle_id: usize, amount: i32) -> ();
	}

	impl GameLogicAppExt for App {
		fn read_vertices(&mut self) -> VertexDebugData {
			self.world_mut().run_system_once(read_system)
		}

		fn turn_cycle(&mut self, cycle_id: usize, amount: i32) -> () {
			self.world_mut()
				.run_system_once_with((cycle_id, amount), turn_system)
		}
	}
}

#[allow(unused_imports)]
use bevy::prelude::*;
#[allow(unused_imports)]
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

	// Rotate 6 times
	for _ in 0..6 {
		app.turn_cycle(0, 1);
		app.update();
		let data = app.read_vertices();
		assert_ne!(
			initial_vertex_data, data,
			"Cycle should've turned to a new state!"
		);
	}

	// Data after 6 turns
	let six_steps_data = app.read_vertices();

	// Rotate final time
	app.turn_cycle(0, 1);
	app.update();
	assert_eq!(
		initial_vertex_data,
		app.read_vertices(),
		"Cycle should've turned all the way around!"
	);

	// Rotate back
	app.turn_cycle(0, -1);
	app.update();
	assert_eq!(
		six_steps_data,
		app.read_vertices(),
		"Cycle should've turned back to 6."
	);

	// TODO: test -6 rotation once implemented.
}
