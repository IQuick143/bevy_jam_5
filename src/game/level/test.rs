use super::{
	backend::builder::{parse_and_run, Error},
	builder::ResultNonExclusive,
	LevelData,
};

fn load(level: &str) -> ResultNonExclusive<LevelData, Error> {
	parse_and_run(level, |w| println!("Warning: {}", w))
}

mod layout_tests {
	use bevy::math::Vec2;
	use itertools::Itertools;

	use super::load;

	#[test]
	fn basic_metatest() {
		load(r"").unwrap_ok();
	}

	#[test]
	fn basic_unicycle() {
		load(r"circle(cycle(_ _ _ _ _); 3, 5, 20);").unwrap_ok();
	}

	#[test]
	fn basic_dicycles_single() {
		load(
			r"
		v = vertex();
		circle(cycle(v _ _ _ _); -10, 0, 10);
		circle(cycle(v _ _ _ _); +10, 0, 10);
		",
		)
		.unwrap_ok();

		/* TODO
		load(r"
		v = vertex();
		circle(cycle(v _); -7, 0, 10);
		circle(cycle(v); +7, 0, 10);
		").unwrap_ok();
		*/

		load(
			r"
		v = vertex();
		circle(cycle(v _ _ _ _); 0, 0, 10);
		circle(cycle(v _ _ _ _); 0, 0, 10);
		",
		)
		.unwrap_ok();
	}

	#[test]
	fn tricycle_interleaved() {
		load(
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
",
		)
		.unwrap_ok();
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
			load(flower).unwrap_ok();
		}
		load(&flowers.iter().join("")).unwrap_ok();
	}
}
