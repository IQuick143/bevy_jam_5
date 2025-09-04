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

		// TODO: Add test asserting this hint to be necessary
		load(
			r"
		v = vertex();
		circle(cycle(v _); -7, 0, 10);
		circle(cycle(v); +7, 0, 10);
		hint_vertex(v; 0, 1);
		",
		)
		.unwrap_ok();

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

		load(
			r"
		a = vertex();
		b = vertex();
		circle(cycle(a b _ _ _ _); -7, 0, 10);
		circle(cycle(b a _ _ _ _); +7, 0, 10);
		hint_vertex(a; 0, 2);
		",
		)
		.unwrap_ok();

		load(
			r"
		a = vertex();
		b = vertex();
		circle(cycle(a b _ _ _ _); -7, 0, 10);
		circle(cycle(b a _ _ _ _); +7, 0, 10);
		hint_vertex(b; 0, -2);
		",
		)
		.unwrap_ok();

		// TODO: Check that `a` was actually placed near `5,5`
		load(
			r"
		a = vertex();
		b = vertex();
		circle(cycle(a b _ _ _); 0, 0, 10);
		circle(cycle(a _ b _ _); 0, 0, 10);
		hint_vertex(a; 5, 5);
		",
		)
		.unwrap_ok();
	}

	#[test]
	fn indirect_hints() {
		load(
			r"
		a = vertex();
		b = vertex();
		rando_vertex = vertex();
		circle(cycle(a b _ _ _ _); -7, 0, 10);
		circle(cycle(b a _ _ rando_vertex _); +7, 0, 10);
		# This hint is enough to decide the ambiguity
		hint_vertex(rando_vertex; 0, +7);
		",
		)
		.unwrap_ok();
	}

	#[test]
	fn road_rage() {
		// This level can be decided with no hints
		load(
			r"
name = 'Car';
hint = 'Fun fact: Originally this level was thought to be impossible!';

box = box('lr');

a = vertex(box);
b = vertex(box);
c = vertex(box);
d = vertex(box);
e = vertex(box);
f = vertex(button() player());
g = vertex();
h = vertex(box button());
i = vertex();
j = vertex(box flag());
k = vertex();
l = vertex(button());
m = vertex(button());
n = vertex(button());
o = vertex(button());
p = vertex(button());

circle(cycle('manual'; a b h m l f); 0 0 1);
circle(cycle('manual'; i n m g b c); 1 0 1);
circle(cycle('manual'; j o n h c d); 2 0 1);
circle(cycle('manual'; k p o i d e); 3 0 1);",
		)
		.unwrap_ok();
	}

	#[test]
	fn olympic() {
		// A chain of cycles can also be automatically resolved
		load(
			r"
name = 'Olympic';
hint = 'Tip: In levels with a single player and manual cycles, it''s helpful to think about the player''s routes through the crossings.';

box = box('desc_neg');

dx = 4;
dy = 3;
r = 3;

a = vertex(player() flag());
b = vertex();
c = vertex();
d = vertex();
e = vertex();
f = vertex();
g = vertex();
h = vertex();
i = vertex();
circle(cycle('manual'; a box b c box _);          -2 * dx, dy, r);
circle(cycle('manual'; _ _ c b d e);                  -dx,  0, r);
circle(cycle('manual'; _ _ f g e d);                    0, dy, r);
circle(cycle('manual'; _ _ g f h i);                   dx,  0, r);
circle(cycle('manual'; _ button() button() _ i h); 2 * dx, dy, r);
",
		).unwrap_ok();
	}

	#[test]
	fn olympic_polymorphic() {
		// TODO: Stress test the pair-pair resolver
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

# Hint that the central vertices go near the center
hint_vertex(bri; 0, 0);
hint_vertex(rgi; 0, 0);
hint_vertex(bgi; 0, 0);
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
