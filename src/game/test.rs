mod utils {
	use crate::game::{
		components::{Cycle, CycleEntities, PlacedGlyph, PlacedObject, VertexDebugID},
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
		)) // Disable spawning of visual entities
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

	impl VertexDebugData {
		pub fn count_objects(&self) -> usize {
			self.objects.iter().filter(|x| x.is_some()).count()
		}
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

	/// System that counts how many cycles there are.
	fn count_cycles_system(cycles: Query<&Cycle>, cycle_master: Query<&CycleEntities>) -> usize {
		let cycle_count = cycles.iter().count();
		let declared_count = cycle_master
			.get_single()
			.expect("There should be exactly one CycleEntities entity")
			.0
			.len();
		assert_eq!(
			cycle_count, declared_count,
			"Number of Cycle entities should match the number of entities in CycleEntities"
		);
		cycle_count
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
		fn conut_cycles(&mut self) -> usize;
		fn turn_cycle(&mut self, cycle_id: usize, amount: i32) -> ();
	}

	impl GameLogicAppExt for App {
		fn read_vertices(&mut self) -> VertexDebugData {
			self.world_mut().run_system_once(read_system)
		}

		fn conut_cycles(&mut self) -> usize {
			self.world_mut().run_system_once(count_cycles_system)
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
use rand::{Rng, SeedableRng};
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

/// Test for normal and crossed links.
#[test]
fn test_basic_links() {
	let mut app = app_with_level(
		r"
NAME=DebugLinkBasic
HINT=A player should not be reading this message!

VERTEX 1 2 3 4 5 6 7 8 9
CYCLE a 1 2 3
CYCLE b 4 5 6
CYCLE c 7 8 9

OBJECT[BOX:1] 1
OBJECT[BOX:2] 2
OBJECT[BOX:3] 3
OBJECT[BOX:4] 4
OBJECT[BOX:5] 5
OBJECT[BOX:6] 6
OBJECT[BOX:7] 7
OBJECT[BOX:8] 8
OBJECT[BOX:9] 9

# A valid triangle of links, should not error
LINK a b
LINK[CROSSED] b c
LINK[CROSSED] a c

# Placement does not really matter
PLACE a 0 0 100
PLACE b 200 0 100
PLACE c 0 200 100
",
	);

	let state_0 = app.read_vertices();
	app.turn_cycle(0, 1);
	app.update();
	let state_1 = app.read_vertices();
	app.turn_cycle(0, 1);
	app.update();
	let state_2 = app.read_vertices();
	app.turn_cycle(0, 1);
	app.update();
	assert_eq!(
		state_0,
		app.read_vertices(),
		"After 3 turns each cycle should've reset to its original state."
	);
	assert_ne!(
		state_0, state_1,
		"Each turn should've produced a new state."
	);
	assert_ne!(
		state_0, state_2,
		"Each turn should've produced a new state."
	);
	assert_ne!(
		state_1, state_2,
		"Each turn should've produced a new state."
	);

	// Level is in state_0, turn into 1 then 2 then 0, but use the second cycle this time.
	app.turn_cycle(1, 1);
	app.update();
	assert_eq!(state_1, app.read_vertices(), "Level is in wrong state.");
	app.turn_cycle(1, 1);
	app.update();
	assert_eq!(state_2, app.read_vertices(), "Level is in wrong state.");
	app.turn_cycle(1, 1);
	app.update();
	assert_eq!(state_0, app.read_vertices(), "Level is in wrong state.");
	// Level is in state_0, turn into 2 then 1 then 0, but use the third cycle this time, which is reversly linked
	app.turn_cycle(2, 1);
	app.update();
	assert_eq!(state_2, app.read_vertices(), "Level is in wrong state.");
	app.turn_cycle(2, 1);
	app.update();
	assert_eq!(state_1, app.read_vertices(), "Level is in wrong state.");
	app.turn_cycle(2, 1);
	app.update();
	assert_eq!(state_0, app.read_vertices(), "Level is in wrong state.");
	// TODO: Test multirotation arithmetic once implemented.
}

/// Test for rotating two cycles in sync, and counting the combinatorics of when they reset.
#[test]
fn test_link_combinatorics() {
	let mut app = app_with_level(
		r"
NAME=DebugLinkCombinatorics
HINT=A player should not be reading this message!

VERTEX 1 2 3 4 5 6 7 8 9 10 11 12 13 14
CYCLE a 1 2 3 4 5
CYCLE b 6 7 8 9 10 11 12 13 14

OBJECT[BOX:1] 1
OBJECT[BOX:2] 2
OBJECT[BOX:3] 3
OBJECT[BOX:4] 4
OBJECT[BOX:5] 5
OBJECT[BOX:6] 6
OBJECT[BOX:7] 7
OBJECT[BOX:8] 8
OBJECT[BOX:9] 9
OBJECT[BOX:10] 10
OBJECT[BOX:11] 11
OBJECT[BOX:12] 12
OBJECT[BOX:13] 13
OBJECT[BOX:14] 14

LINK a b

# Placement does not really matter
PLACE a 0 0 100
PLACE b 200 0 100
",
	);

	let state_0 = app.read_vertices();

	// We have a 5-cycle and a 9-cycle
	let cycle_a = 5;
	let cycle_b = 9;

	for _ in 0..(cycle_a * cycle_b - 1) {
		app.turn_cycle(0, 1);
		app.update();
		assert_ne!(
			state_0,
			app.read_vertices(),
			"Each turn should've produced a new state."
		);
	}
	app.turn_cycle(0, 1);
	app.update();
	assert_eq!(
		state_0,
		app.read_vertices(),
		"After A*B turns each cycle should've reset to its original state."
	);
}

/// Generates a random iterator of `n_steps` moves in the form (cycle, rotation).
fn generate_random_cycle_walk<'a>(
	n_cycles: usize,
	n_steps: usize,
	rng: &'a mut impl Rng,
) -> impl Iterator<Item = (usize, i32)> + 'a {
	(0..n_steps).map(move |_| {
		(
			rng.gen_range(0..n_cycles),
			if rng.gen_bool(0.5) { 1 } else { -1 },
		)
	})
}

/// Generates a random iterator of `n_steps` moves in the form (cycle, rotation)
/// with the additional property that it undoes all its moves.
fn generate_random_returning_cycle_walk(
	n_cycles: usize,
	n_steps: usize,
	rng: &mut impl Rng,
) -> Vec<(usize, i32)> {
	let iter: Vec<(usize, i32)> = generate_random_cycle_walk(n_cycles, n_steps, rng).collect();
	iter.iter()
		.cloned()
		.chain(
			iter.iter()
				.rev()
				.map(|(cycle, direction)| (*cycle, -direction)),
		)
		.collect()
}

/// Perform a large amount of random moves and then undoes them.
/// ## ASSERTS:
/// That the amount of objects is invariant.
///
/// That after doing all moves in reverse the state has returned to the starting position.
fn move_fuzz(app: &mut App, n_steps: usize, seed: u64) {
	let n_cycles = app.conut_cycles();
	let intial_state = app.read_vertices();
	let n_boxes = intial_state.count_objects();
	// Perform many moves
	let mut rng = rand::rngs::SmallRng::seed_from_u64(seed);
	for &(cycle, turn) in generate_random_returning_cycle_walk(n_cycles, n_steps, &mut rng).iter() {
		app.turn_cycle(cycle, turn);
		app.update();
		let state = app.read_vertices();
		// TODO: Redo for Horocycles or other mechanics violating object conservation laws.
		let total_boxes = state.count_objects();
		assert_eq!(
			total_boxes, n_boxes,
			"The number of boxes should not change."
		)
	}
	assert_eq!(
		intial_state,
		app.read_vertices(),
		"Moves should've been undone."
	);
}

/// Tests rotation with intersecting cycles
#[test]
fn stress_test_tricycle() {
	let mut app = app_with_level(
		r"
Name=DebugTricycle

VERTEX b1 b2 b3 bgi bgo bri bro r1 r2 r3 rgi rgo g1 g2 g3

CYCLE blue b1 b2 b3 bgo bri bgi bro
CYCLE red r1 r2 r3 bro rgi bri rgo
CYCLE green g1 g2 g3 rgo bgi rgi bgo

OBJECT[BOX:0] rgi
OBJECT[BOX:1] bri
OBJECT[BOX:2] bgi
OBJECT[BUTTON:0] b2
OBJECT[BUTTON:1] r2
OBJECT[BUTTON:2] g2

PLACE blue -87 50 130
PLACE red 0 -100 130
PLACE green 87 50 130
",
	);
	let intial_state = app.read_vertices();

	let n_cycles = app.conut_cycles();
	for i in 0..n_cycles {
		app.turn_cycle(i, 1);
		app.update();
	}
	assert_ne!(
		intial_state,
		app.read_vertices(),
		"Something should've changed."
	);
	for i in (0..n_cycles).rev() {
		app.turn_cycle(i, -1);
		app.update();
	}
	assert_eq!(
		intial_state,
		app.read_vertices(),
		"Moves should've been undone."
	);

	move_fuzz(&mut app, 1024, 1234123412341234);
}

/// Tests rotation with cycles intersecting at one point.
#[test]
fn stress_test_dicycle() {
	let mut app = app_with_level(
		r"
Name=DebugDicycle

VERTEX X 1 2 3 a b

CYCLE A X 1 2 3
CYCLE B X a b

OBJECT[BOX:0] X
OBJECT[BOX:1] 1
OBJECT[BOX:2] 2
OBJECT[BOX:3] 3
OBJECT[BOX:4] a
OBJECT[BOX:5] b

PLACE A -100 0 100
PLACE B  100 0 100
",
	);
	let intial_state = app.read_vertices();

	app.turn_cycle(0, 1);
	app.update();
	app.turn_cycle(1, 1);
	app.update();
	assert_ne!(
		intial_state,
		app.read_vertices(),
		"Something should've changed."
	);
	app.turn_cycle(1, -1);
	app.update();
	app.turn_cycle(0, -1);
	app.update();
	assert_eq!(
		intial_state,
		app.read_vertices(),
		"Moves should've been undone."
	);

	// Perform many moves
	move_fuzz(&mut app, 1024, 1234123412341234);
}

/// Tries fuzzing a random selection of levels
#[test]
fn stress_test_random_levels() {
	let levels = [
		include_str!("../../assets/levels/1_intro.txt"),
		include_str!("../../assets/levels/2_sort.txt"),
		include_str!("../../assets/levels/rubik.txt"),
		include_str!("../../assets/levels/5_sync.txt"),
		include_str!("../../assets/levels/6_sync2.txt"),
		include_str!("../../assets/levels/send.txt"),
		include_str!("../../assets/levels/linked_sort.txt"),
	];
	for level in levels {
		let mut app = app_with_level(level);
		move_fuzz(&mut app, 4096, 1337133713371337);
	}
}
