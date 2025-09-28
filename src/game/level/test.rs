use super::{
	backend::builder::{parse_and_run, Error},
	builder::LevelBuildResult,
};

fn load(level: &str) -> Result<LevelBuildResult, Error> {
	parse_and_run(level, |w| println!("Warning: {}", w))
}

macro_rules! expect_fully_ok {
	($result:expr) => {{
		let result = ($result).expect("Level failed to parse and did not return a partial build");
		let no_errors_reported = result.errors.is_ok();
		assert!(
			no_errors_reported,
			"Level builder emited errors: {:?}",
			result.errors.0
		);
		assert!(
			result.level.is_valid,
			"Level builder emited no errors, but level is marked as invalid"
		);
		result.level
	}};
}

mod layout_tests {
	use bevy::math::Vec2;
	use itertools::Itertools;
	use rand::{seq::SliceRandom, SeedableRng};

	use super::load;

	#[test]
	fn basic_metatest() {
		expect_fully_ok!(load(r""));
	}

	#[test]
	fn basic_unicycle() {
		expect_fully_ok!(load(r"circle(cycle(_ _ _ _ _); 3, 5, 20);"));
	}

	#[test]
	fn basic_dicycles_single() {
		expect_fully_ok!(load(
			r"
		v = vertex();
		circle(cycle(v _ _ _ _); -10, 0, 10);
		circle(cycle(v _ _ _ _); +10, 0, 10);
		",
		));

		expect_fully_ok!(load(
			r"
		v = vertex();
		circle(cycle(v _); -7, 0, 10);
		circle(cycle(v); +7, 0, 10);
		",
		));

		expect_fully_ok!(load(
			r"
		v = vertex();
		circle(cycle(v _ _ _ _); 0, 0, 10);
		circle(cycle(v _ _ _ _); 0, 0, 10);
		",
		));
	}

	#[test]
	fn basic_dicycles_double() {
		// TODO: This should fail with an Err
		// load(
		// 	r"
		// a = vertex();
		// b = vertex();
		// circle(cycle(a b _ _ _ _); -10, 0, 10);
		// circle(cycle(b a _ _ _ _); +10, 0, 10);
		// ",
		// )

		expect_fully_ok!(load(
			r"
		a = vertex();
		b = vertex();
		circle(cycle(a b _ _ _ _); -7, 0, 10);
		circle(cycle(b a _ _ _ _); +7, 0, 10);
		",
		));

		expect_fully_ok!(load(
			r"
		a = vertex();
		b = vertex();
		circle(cycle(a b _ _ _ _); -7, 0, 10);
		circle(cycle(b a _ _ _ _); +7, 0, 10);
		",
		));

		expect_fully_ok!(load(
			r"
		a = vertex();
		b = vertex();
		circle(cycle(a b _ _ _); 0, 0, 10);
		circle(cycle(a _ b _ _); 0, 0, 10);
		",
		));
	}

	#[test]
	fn indirect_hints() {
		expect_fully_ok!(load(
			r"
		a = vertex();
		b = vertex();
		rando_vertex = vertex();
		circle(cycle(a b _ _ _ _); -7, 0, 10);
		circle(cycle(b a _ _ rando_vertex _); +7, 0, 10);
		# This hint is enough to decide the ambiguity
		hint_vertex(rando_vertex; 0, +7);
		",
		));
	}

	#[test]
	fn road_rage() {
		// This level can be decided with no hints
		expect_fully_ok!(load(include_str!("../../.././assets/levels/car.txt")));
	}

	#[test]
	fn olympic() {
		// A chain of cycles can also be automatically resolved
		expect_fully_ok!(load(include_str!("../../.././assets/levels/olympic.txt")));
	}

	#[test]
	fn olympic_polymorphic() {
		let header = "R=12;o=10;\n";

		for n_cycles in 3..10 {
			let mut vertices = Vec::new();
			for i in 0..n_cycles - 1 {
				vertices.push(format!("v{i}a=vertex();\n"));
				vertices.push(format!("v{i}b=vertex();\n"));
			}
			let mut cycles = Vec::new();
			for i in 0..n_cycles {
				let extra_vertices = (0..i).map(|_| "_").join(" ");
				let start_vertices = if i > 0 {
					format!("v{}a v{}b ", i - 1, i - 1)
				} else {
					"".to_owned()
				};
				let end_vertices = if i < n_cycles - 1 {
					format!("v{}b v{}a ", i, i)
				} else {
					"".to_owned()
				};
				cycles.push(format!("circle(cycle({start_vertices} {extra_vertices} {end_vertices}); {i}*o, 0, R);\n"));
			}
			let mut rng = rand::rngs::SmallRng::seed_from_u64(101);
			for _ in 0..60 {
				expect_fully_ok!(load(
					&(header.to_owned() + &vertices.join("") + &cycles.join(""))
				));
				vertices.shuffle(&mut rng);
				cycles.shuffle(&mut rng);
			}
		}
	}

	#[test]
	fn olympic_with_holes() {
		// Like olympic, but some cycles have only one shared vertex
		// This can still be laid out by deducing the pairs and then choosing arbitrarily for the singles
		expect_fully_ok!(load(
			r"
a1 = vertex();
a2 = vertex();
a3 = vertex();
a4 = vertex();
a5 = vertex();

b1 = vertex();
b2 = vertex();
# b3 = vertex();
# b4 = vertex();
b5 = vertex();

circle(cycle(a1 b1); 0,0,1.4);
circle(cycle(a2 b2 b1 a1); 0,1,1.4);
circle(cycle(a3 b2 a2); 0,2,1.4);
circle(cycle(a4 a3); 0,3,1.4);
circle(cycle(a5 b5 a4); 0,4,1.4);
circle(cycle(a5 b5); 0,5,1.4);
",
		));
	}

	#[test]
	fn pair_surrounded_by_singles() {
		expect_fully_ok!(load(
			r"
undecided = vertex();
single_1 = vertex();
single_2 = vertex();

circle(cycle(undecided, single_1, single_2); 0, 0, 10);
circle(cycle(undecided); 0, +10, 10);
hint_vertex(single_1; 0,+1);
hint_vertex(single_2; 0,-1);",
		));
	}

	#[test]
	fn twin_partial_interaction() {
		// A single Pair vertex should be enough to resolve a twin pair
		expect_fully_ok!(load(
			r"
undecided = vertex();
twins_a1 = vertex();
twins_b1 = vertex();
twins_a2 = vertex();
twins_b2 = vertex();

circle(cycle(undecided, twins_a1, twins_b1, twins_a2, twins_b2); 0, 0, 10);
# Interleaving twin vertices should not be resolvable
circle(cycle(twins_a1, twins_a2); 0, -10, 10);
circle(cycle(twins_b1, twins_b2); +10, 0, 10);
# This vertex should resolve twins_a which should resolve twins_b which should resolve undecided
circle(cycle(undecided); 0, +10, 10);",
		));
	}

	#[test]
	fn tricycle_interleaved() {
		expect_fully_ok!(load(
			r"
r = 1;
sep = 1.3;

bri = vertex();
rgi = vertex();
bgi = vertex();
bro = vertex();
rgo = vertex();
bgo = vertex();
circle(cycle('manual'; _ _ _ _ bgo bri bgi bro); -sep / 2, 0, r);
circle(cycle('manual'; _ _ _ _ bro rgi bri rgo); 0, -sep / 2 * sqrt(3), r);
circle(cycle('manual'; _ _ _ _ rgo bgi rgi bgo); sep / 2, 0, r);

# Hint that the central vertices go near the center
hint_vertex(bri; 0, 0);
hint_vertex(rgi; 0, 0);
hint_vertex(bgi; 0, 0);
",
		));
	}

	fn get_flower(
		flower_id: &str,
		n_petals: usize,
		n_extra_vertices: usize,
		radius: f32,
		position: Vec2,
	) -> String {
		let mut level = String::new();
		for i in 0..n_petals {
			level.push_str(&format!("v_{flower_id}_{i} = vertex();\n"));
		}
		level.push_str(&format!("center_{flower_id} = vertex();\n"));
		level.push_str(&format!("radius_{flower_id} = {radius};\n"));
		level.push_str(&format!("pos_x_{flower_id} = {};\n", position.x));
		level.push_str(&format!("pos_y_{flower_id} = {};\n", position.y));
		let additional = (0..n_extra_vertices).map(|_| "_").join(" ");
		for i in 0..n_petals {
			let j = (i + 1) % n_petals;
			level.push_str(&format!("circle(cycle(v_{flower_id}_{j} center_{flower_id} v_{flower_id}_{i} {additional}); pos_x_{flower_id} + radius_{flower_id} * cos(-2 * pi * {i} / {n_petals}), pos_y_{flower_id} + radius_{flower_id} * sin(-2 * pi * {i} / {n_petals}), radius_{flower_id});\n"));
		}
		level
	}

	#[test]
	fn flower() {
		let flowers: Vec<String> = (3..8)
			.map(|n_petals| {
				get_flower(&format!("flower_{n_petals}"), n_petals, 3, 10.0, Vec2::ZERO)
			})
			.collect();
		for flower in flowers.iter() {
			expect_fully_ok!(load(flower));
		}
		expect_fully_ok!(load(&flowers.iter().join("")));
	}
}
