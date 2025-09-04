mod utils {
	use crate::game::{
		components::{Cycle, GameStateEcsIndex, Vertex},
		level::{backend::builder as parser, GlyphData, ObjectData},
		logic_relay::RotateCycleGroup,
		prelude::*,
	};
	use bevy::{ecs::system::RunSystemOnce, prelude::*};

	pub fn setup_app() -> App {
		let mut app = App::new();
		app.add_plugins((
			AssetPlugin {
				// Turn this of because it suddenly turned into a performance bottleneck
				watch_for_changes_override: Some(false),
				..default()
			},
			super::super::level::asset::plugin,
			super::super::logic_relay::plugin,
			super::super::history::plugin,
			super::super::spawn::plugin,
		))
		.add_systems(
			LevelInitialization,
			assign_debug_ids.after(LevelInitializationSet::SpawnPrimaryEntities),
		)
		// Disable spawning of visual entities
		.configure_sets(
			LevelInitialization,
			LevelInitializationSet::SpawnVisuals.run_if(|| false),
		);
		app
	}

	/// A vertex (node) on the circle
	#[derive(Component, Debug, Clone, Copy, Default, Reflect)]
	pub struct VertexDebugID(pub usize);

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

	fn assign_debug_ids(query: Query<Entity, Added<Vertex>>, mut commands: Commands) {
		for (i, id) in query.iter().enumerate() {
			commands.entity(id).insert(VertexDebugID(i));
		}
	}

	fn load_level(
		In(level): In<LevelData>,
		mut asset_server: ResMut<Assets<LevelData>>,
		mut spawn_trigger: EventWriter<EnterLevel>,
	) -> Result<(), parser::Error> {
		let handle = asset_server.add(level);
		spawn_trigger.write(EnterLevel(Some(handle)));
		Ok(())
	}

	pub fn app_with_level(level: &str) -> App {
		let mut app = setup_app();
		let level = parser::parse_and_run(level, |_| {})
			.value()
			.expect("Level data should compile correctly!");
		assert!(level.is_valid);

		app.world_mut()
			.run_system_once_with(load_level, level)
			.expect("System should've ran.")
			.expect("Level should've parsed!");
		// An update is needed to trigger the spawning logic
		app.update();
		app
	}

	fn read_system(
		vertices: Query<&VertexDebugID>,
		game_state: Res<GameState>,
		level: PlayingLevelData,
	) -> VertexDebugData {
		let level = level.get().expect("Playing level is not available");
		let n_vertices = vertices.iter().count();
		let mut glyph_data = vec![None; n_vertices];
		for &VertexDebugID(i) in &vertices {
			glyph_data[i] = level.vertices[i].glyph;
		}

		VertexDebugData {
			n_vertices,
			objects: game_state.objects.clone(),
			glyphs: glyph_data,
		}
	}

	/// System that counts how many cycles there are.
	fn count_cycles_system(cycles: Query<&Cycle>, entity_index: Res<GameStateEcsIndex>) -> usize {
		let cycle_count = cycles.iter().count();
		let declared_count = entity_index.cycles.len();
		assert_eq!(
			cycle_count, declared_count,
			"Number of Cycle entities should match the number of entities in CycleEntities"
		);
		cycle_count
	}

	fn turn_system(In((id, amount)): In<(usize, i32)>, mut events: EventWriter<RotateCycleGroup>) {
		events.write(RotateCycleGroup(RotateCycle {
			target_cycle: id,
			amount: amount as i64,
		}));
	}

	pub trait GameLogicAppExt {
		fn read_vertices(&mut self) -> VertexDebugData;
		fn conut_cycles(&mut self) -> usize;
		fn turn_cycle(&mut self, cycle_id: usize, amount: i32);
	}

	impl GameLogicAppExt for App {
		fn read_vertices(&mut self) -> VertexDebugData {
			self.world_mut()
				.run_system_once(read_system)
				.expect("System should have all necessary objects.")
		}

		fn conut_cycles(&mut self) -> usize {
			self.world_mut()
				.run_system_once(count_cycles_system)
				.expect("System should have all necessary objects.")
		}

		fn turn_cycle(&mut self, cycle_id: usize, amount: i32) {
			self.world_mut()
				.run_system_once_with(turn_system, (cycle_id, amount))
				.expect("System should have all necessary objects.")
		}
	}
}

#[allow(unused_imports)]
use bevy::prelude::*;
use itertools::Itertools;
#[allow(unused_imports)]
use rand::{Rng, SeedableRng};
#[allow(unused_imports)]
use utils::*;

use crate::game::level::ObjectData;

/// Metatest for asserting that running the headless game works.
#[test]
fn test_app() {
	let mut app = app_with_level("");
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
name = 'Debug1';
hint = 'A player should not be reading this message!';

circle(cycle(_ flag() _ _ _ player() _); 0 0 100);
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
name = 'Debug2';
hint = 'A player should not be reading this message!';

circle(cycle(_ flag() _ _ _ player() _); 0 0 100);
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

	app.turn_cycle(0, 1);
	app.update();
	assert_eq!(initial_vertex_data, app.read_vertices());

	app.turn_cycle(0, 6);
	app.update();
	assert_eq!(
		six_steps_data,
		app.read_vertices(),
		"Rotating sextuply should have the same effect as rotating once six times."
	);

	app.turn_cycle(0, -3);
	app.update();
	app.turn_cycle(0, -3);
	app.update();
	assert_eq!(initial_vertex_data, app.read_vertices(), "2 * -3 = -6.");
}

/// Test for 0-sized cycles
#[test]
fn test_zero_cycle() {
	let mut app = app_with_level(
		r"
name = 'Zero cycle';
hint = 'A player should not be reading this message!';

circle(cycle(); 0 0 100);
",
	);

	app.turn_cycle(0, 1);
	app.update();

	app.turn_cycle(0, -1);
	app.update();

	app.turn_cycle(0, 1684657993);
	app.update();
}

/// Test that the cycle rotation direction is truly forwards
#[test]
fn test_rotation_direction() {
	fn test_case(level: &'static str) {
		let mut app = app_with_level(level);

		let state_start = app.read_vertices();
		// Box should move to intersect, not dummy2
		app.turn_cycle(0, 1);
		app.update();
		// Box should get carried off to off
		app.turn_cycle(1, 1);
		app.update();
		// Box should not return
		app.turn_cycle(0, -1);
		app.update();
		assert_ne!(
			state_start,
			app.read_vertices(),
			"Box should've moved forwards and off."
		);
	}

	let test_cases = [
		r"
name = 'DebugDirection1';
hint = 'A player should not be reading this message!';

intersect = vertex();
test = cycle(_ box(1) intersect _ _);
switch = cycle(intersect _);

# Placement does not really matter
circle(test; 0 0 100);
circle(switch; 200 0 100);
",
		r"
name = 'DebugDirection2';
hint = 'A player should not be reading this message!';

intersect = vertex();
driver = cycle(_ _);
switch = cycle(intersect _);
test = cycle(_ box(1) intersect _ _);

link(driver test);

# Placement does not really matter
circle(driver; 400 0 100);
circle(test; 0 0 100);
circle(switch; 200 0 100);
",
		r"
name = 'DebugDirection3';
hint = 'A player should not be reading this message!';

intersect = vertex();
driver = cycle(_ _);
switch = cycle(intersect _);
gear = cycle(_ _);
test = cycle(_ box(1) intersect _ _);

link('invert'; driver gear);
link('invert'; gear test);

# Placement does not really matter
circle(driver; 400 0 100);
circle(gear; 400 0 50);
circle(test; 0 0 100);
circle(switch; 200 0 100);
",
		r"
name = 'DebugDirection4';
hint = 'A player should not be reading this message!';

intersect = vertex();
test = cycle(_ box(1) intersect _ _);
switch = cycle(intersect _);
dummy_gear = cycle(_ _);

link('invert'; dummy_gear test);

# Placement does not really matter
circle(dummy_gear; 400 0 50);
circle(test; 0 0 100);
circle(switch; 200 0 100);
",
	];
	for test in test_cases {
		test_case(test);
	}
}

/// Test for normal and crossed links.
#[test]
fn test_basic_links() {
	let mut app = app_with_level(
		r"
name = 'DebugLinkBasic';
hint = 'A player should not be reading this message!';

a = cycle(box(1) box(2) box(3));
b = cycle(box(4) box(5) box(6));
c = cycle(box(7) box(8) box(9));

# A valid triangle of links, should not error
link(a b);
link('invert'; b c);
link('invert'; a c);

# Placement does not really matter
circle(a; 0 0 100);
circle(b; 200 0 100);
circle(c; 0 200 100);
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

	// Testing multiturn arithmetic
	let states = [state_0, state_1, state_2];
	// We start in state 0 according to last assert
	let mut state_index: usize = 0;
	for i in 1..32 {
		app.turn_cycle(1, i);
		app.update();
		state_index += i as usize;
		state_index %= 3;
		assert_eq!(
			states[state_index],
			app.read_vertices(),
			"Level is in wrong state."
		);
	}
	for i in 1..32 {
		app.turn_cycle(2, i);
		app.update();
		// 2 == -1 in mod 3 arithmetic, so we use it as subtraction
		state_index += 2 * i as usize;
		state_index %= 3;
		assert_eq!(
			states[state_index],
			app.read_vertices(),
			"Level is in wrong state."
		);
	}
}

/// Test for rotating two cycles in sync, and counting the combinatorics of when they reset.
#[test]
fn test_link_combinatorics() {
	let mut app = app_with_level(
		r"
name = 'DebugLinkCombinatorics';
hint = 'A player should not be reading this message!';

a = cycle(box(1) box(2) box(3) box(4) box(5));
b = cycle(box(6) box(7) box(8) box(9) box(10) box(11) box(12) box(13) box(14));

link(a b);

# Placement does not really matter
circle(a; 0 0 100);
circle(b; 200 0 100);
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

/// Test one-way links with higher multiplicity
#[test]
fn test_link_multiplicity() {
	use crate::game::level::ObjectData;

	let mut app = app_with_level(
		r"
name = 'DebugLinkMultiplicity';
hint = 'A player should not be reading this message!';

a = circle(cycle(box() _ _ _ _); 0 0 1);
b = circle(cycle(box() _ _ _ _); 0 0 1);
c = circle(cycle(box() _ _ _ _); 0 0 1);
d = circle(cycle(box() _ _ _ _); 0 0 1);
e = circle(cycle(box() _ _ _ _); 0 0 1);
f = circle(cycle(box() _ _ _ _); 0 0 1);
g = circle(cycle(box() _ _ _ _); 0 0 1);
h = circle(cycle(box() _ _ _ _); 0 0 1);

link(   1; a b);
link(  -1; c a); # This link should be symmetrical
oneway( 1; a d);
oneway(-1; a e);
oneway( 2; a f);
oneway(-2; a g);
oneway( 3; a h);
",
	);

	const CYCLE_COUNT: usize = 8;

	// Shorthand for box object for assertions
	const BOX: Option<ObjectData> = Some(ObjectData::Box(None));

	// Sanity check: objects should be where we placed them
	let initial_state = [[BOX, None, None, None, None]; CYCLE_COUNT]
		.into_iter()
		.flatten()
		.collect::<Vec<_>>();
	assert_eq!(app.read_vertices().objects, initial_state);

	// Turn the center cycle once
	app.turn_cycle(0, 1);
	app.update();

	// Other cycles should follow
	let expected_state = [
		None, BOX, None, None, None, // Cycle a was turned manually
		None, BOX, None, None, None, // Cycle b should turn forward once
		None, None, None, None, BOX, // Cycle c should turn backwards once
		None, BOX, None, None, None, // Cycle d should turn forward once
		None, None, None, None, BOX, // Cycle e should turn backwards once
		None, None, BOX, None, None, // Cycle f should turn forward twice
		None, None, None, BOX, None, // Cycle g should turn backwards twice
		None, None, None, BOX, None, // Cycle h should turn forward thrice
	];
	assert_eq!(app.read_vertices().objects, expected_state);
}

/// Test that detectors activate when expected.
#[test]
fn test_detector_activations() {
	let level_header = "
name = 'TestDetectorActivations';
hint = 'A player should not be reading this message!';

test_cycle = cycle(box() _ _ _ _ _ _ _ _);
circle(test_cycle; 0 0 1);
d = detector();
oneway(d test_cycle);
circle(";

	let level_footer = "; 0, 0, 1);";
	let should_activate_instantly_forward = [
		"cycle(d box())",
		"cycle(box() d)",
		"cycle(_ box() d)",
		"cycle(_ box() d _)",
		"cycle(_ _ box() box() d _ _)",
		"cycle(_ box() box() d box() _ _)",
		"cycle(d _ _ box())",
		"cycle(_ _ box() d)",
		"cycle(_ d _ d box() d)",
	];
	let should_activate_instantly_backward = [
		"cycle(d box())",
		"cycle(box() d)",
		"cycle(_ d box())",
		"cycle(_ d box() _)",
		"cycle(_ _ d box() box() _ _)",
		"cycle(_ box() d box() box() _ _)",
		"cycle(d box() _ _ )",
		"cycle(box() _ _ d)",
		"cycle(_ d _ d box() d)",
	];
	const N_VERTICES_ON_TEST_CYCLE: usize = 9;
	fn check_test_cycle_in_position(app: &mut App, position: usize) {
		let vertices = app.read_vertices();
		assert!(
			position < N_VERTICES_ON_TEST_CYCLE,
			"Some doofus done doofed up the testcase"
		);
		assert_eq!(
			vertices.objects[position],
			Some(ObjectData::Box(None)),
			"Test cycle is in a wrong position"
		);
	}
	for circle in should_activate_instantly_forward.iter() {
		let level = format!("{level_header}{circle}{level_footer}");
		let mut app = app_with_level(&level);
		// Test cycle should be unrotated
		check_test_cycle_in_position(&mut app, 0);
		app.turn_cycle(1, 1);
		app.update();
		check_test_cycle_in_position(&mut app, 1);
		app.turn_cycle(1, -1);
		app.update();
		check_test_cycle_in_position(&mut app, 0);
	}
	for circle in should_activate_instantly_backward.iter() {
		let level = format!("{level_header}{circle}{level_footer}");
		let mut app = app_with_level(&level);
		// Test cycle should be unrotated
		check_test_cycle_in_position(&mut app, 0);
		app.turn_cycle(1, -1);
		app.update();
		check_test_cycle_in_position(&mut app, N_VERTICES_ON_TEST_CYCLE - 1);
		app.turn_cycle(1, 1);
		app.update();
		check_test_cycle_in_position(&mut app, 0);
	}
}

/// Test that detectors activate when expected when turned a lot.
#[test]
fn test_detector_multi_activations() {
	let level = "
name = 'TestDetectorActivations';
hint = 'A player should not be reading this message!';

test_cycle = cycle(box() _ _ _ _ _ _ _ _);
circle(test_cycle; 0 0 1);
d = detector();
oneway(d test_cycle);
circle(cycle(d box() _ _ _); 0 0 1);";
	const N_VERTICES_ON_TEST_CYCLE: usize = 9;
	const N_VERTICES_ON_DETECTOR_CYCLE: usize = 4;
	fn check_test_cycle_in_position(app: &mut App, position: usize) {
		let vertices = app.read_vertices();
		assert!(
			position < N_VERTICES_ON_TEST_CYCLE,
			"Some doofus done doofed up the testcase"
		);
		assert_eq!(
			vertices.objects[position],
			Some(ObjectData::Box(None)),
			"Test cycle is in a wrong position"
		);
	}
	for i in 0..200 {
		let mut app = app_with_level(level);
		// Test cycle should be unrotated
		check_test_cycle_in_position(&mut app, 0);
		app.turn_cycle(1, i);
		app.update();
		check_test_cycle_in_position(
			&mut app,
			(i32::div_euclid(i, N_VERTICES_ON_DETECTOR_CYCLE as i32)) as usize
				% N_VERTICES_ON_TEST_CYCLE,
		);
		app.turn_cycle(1, -i);
		app.update();
		check_test_cycle_in_position(&mut app, 0);
	}
}

// Tests that a contraption for divisibility works
#[test]
fn test_divisibilitor() {
	for divisor in 2..10 {
		for divisee in 1..50 {
			let level = format!(
				"name = 'Divisibility Contraption';

			d = detector();
			blocker_vertex = vertex();

			source = cycle(box() _);
			gear = cycle(blocker_vertex);
			blocker = cycle(blocker_vertex);
			divider = cycle(d box() {});
			
			oneway({divisee}; source gear);
			oneway(-1; gear blocker);
			oneway(1; gear divider);
			oneway({divisor}; d blocker);

			circle(source; 0, -10, 1);
			circle(divider; -1, 0, 1);
			circle(gear; -1, 0, 1);
			circle(blocker; +1, 0, 1);",
				(1..divisor).map(|_| { "_" }).join(" ")
			);
			let mut app = app_with_level(&level);
			let initial_vertex_state = app.read_vertices();
			assert_eq!(initial_vertex_state.n_vertices, divisor + 3);
			// println!("Divisor: {divisor}, Divisee: {divisee}");
			app.turn_cycle(0, 1);
			app.update();
			let new_state = app.read_vertices();
			if divisee % divisor == 0 {
				// Turn should've went through and changed stuff
				assert_ne!(new_state, initial_vertex_state);
			} else {
				// Turn shouldn't've went through and nothing should've changed
				assert_eq!(new_state, initial_vertex_state);
			}
			app.turn_cycle(0, -1);
			app.update();
			let final_state = app.read_vertices();
			assert_eq!(final_state, initial_vertex_state);
		}
	}
}

/// Tests the edge case of a cycle without vertices
#[test]
fn turning_empty_cycle() {
	let mut app = app_with_level(
		r"
name = 'EmptyCycleTurning';
hint = 'A player should not be reading this message!';

a = circle(cycle(); 0 0 100);
b = circle(cycle(box() _); 0 200 100);
c = circle(cycle(box() _); 0, -200, 100);
link(b a);
oneway(c a);
	",
	);

	// Indices of the cycles
	let tested_cycle = 0;
	let hard_linked_cycle = 1;
	let oneway_linked_cycle = 2;

	// Turn the cycles one or more times
	// This should not cause a zero division error
	for i in -4..=4 {
		// Turn the cycle directly
		app.turn_cycle(tested_cycle, i);
		app.update();
		// Turn a hard-linked cycle
		app.turn_cycle(hard_linked_cycle, i);
		app.update();
		// Turn a one-way-linked cycle
		app.turn_cycle(oneway_linked_cycle, i);
		app.update();
	}
}

/// Generates a random iterator of `n_steps` moves in the form (cycle, rotation).
fn generate_random_cycle_walk(
	n_cycles: usize,
	n_steps: usize,
	rng: &mut impl Rng,
) -> impl Iterator<Item = (usize, i32)> + '_ {
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

/// Test for 0-sized cycles with links
#[test]
fn test_gears() {
	let mut app = app_with_level(
		r"
name = 'Zero cycle 2';
hint = 'A player should not be reading this message!';

circle(gear1 = cycle(); 0 0 100);
circle(gear2 = cycle(); 0 0 100);
circle(gear3 = cycle(); 0 0 100);

circle(a = cycle(box() _ _); 0 0 100);
circle(b = cycle(box() _ _ _); 0 0 100);
circle(c = cycle(box() _ _ _ _); 0 0 100);

oneway(a gear1 gear2 gear3 c);
link(gear2 b);
",
	);

	let state_1 = app.read_vertices();

	app.turn_cycle(0, 1);
	app.update();

	let state_2 = app.read_vertices();

	app.turn_cycle(1, -1);
	app.update();

	let state_1_2 = app.read_vertices();

	app.turn_cycle(2, 1);
	app.update();

	let state_3 = app.read_vertices();

	assert_ne!(state_1, state_2);
	assert_ne!(state_2, state_3);
	assert_ne!(state_3, state_1);
	assert_eq!(state_1, state_1_2);

	move_fuzz(&mut app, 128, 3);
}

/// Tests rotation with intersecting cycles
#[test]
fn stress_test_tricycle() {
	let mut app = app_with_level(
		r"
name = 'DebugTricycle';

bgi = vertex(box(2));
bgo = vertex();
bri = vertex(box(1));
bro = vertex();
rgi = vertex(box(0));
rgo = vertex();

blue = cycle(_ button(0) _ bgo bri bgi bro);
red = cycle(_ button(1) _ bro rgi bri rgo);
green = cycle(_ button(2) _ rgo bgi rgi bgo);

circle(blue; -87, 50 130);
circle(red; 0, -100, 130);
circle(green; 87 50 130);
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
name = 'DebugDicycle';

x = vertex(box(0));
a = cycle(x box(1) box(2) box(3));
b = cycle(x box(4) box(5));

circle(a; -100, 0 100);
circle(b; 100 0 100);
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

#[test]
fn stress_test_detector_invertibility() {
	let level = "
name = 'DetectorStressTest';
hint = 'A player should not be reading this message!';

d1 = detector();
d2 = detector();
d3 = detector();
d4 = detector();

a1 = cycle(d1 box() player() _ _);
a2 = cycle(d2 player() _ d1 _);

link(-1; a1 a2);

b1 = cycle(box() _ player() _ d4 _);
b2 = cycle(player() _ d3 box() d4);

link(b1 b2);

oneway(2; d1 b1);
oneway(-3; d2 b2);

c1 = cycle(box() _ _ _);
c2 = cycle(box() _ _ _);

link(c1, c2);

oneway(d3, c1);
oneway(d4, c2);

circle(a1; 0 0 1);
circle(a2; 0 0 1);
circle(b1; 0 0 1);
circle(b2; 0 0 1);
circle(c1; 0 0 1);
circle(c2; 0 0 1);
";
	let mut app = app_with_level(level);
	move_fuzz(&mut app, 4096, 1337133713371337);
}

/// Tries fuzzing a random selection of levels
#[test]
fn stress_test_random_levels() {
	let levels = [
		include_str!("../../assets/levels/1_intro.txt"),
		include_str!("../../assets/levels/2_sort.txt"),
		//include_str!("../../assets/levels/rubik.txt"),
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
