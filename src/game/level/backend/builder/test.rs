use itertools::Itertools;

use super::{
	super::super::{
		builder::{error::*, LevelBuildResult},
		*,
	},
	finalize::FinalizeError,
	runtime::RuntimeError,
	Error,
};
use crate::epilang::interpreter::{FunctionCallError, InterpreterError, LogicError};
use std::f32::consts::PI;

fn parse(level_file: &str) -> Result<LevelBuildResult, Error> {
	super::parse_and_run(level_file, |_| {})
}

macro_rules! assert_err_eq {
	($left:expr, $right:expr) => {
		let right = $right;
		match $left {
			Ok(result) => {
				assert!(
					result.errors.iter().any(|err| *err == right),
					"Negative test sample did not report the expected error (expected: {right}, got: {:?})",
					result.errors.0,
				);
			}
			Err(err) => {
				let builder_error = match err {
					Error::Runtime(InterpreterError::LogicError(err, _)) => match *err {
						LogicError::FunctionCall(
							FunctionCallError::Domain(RuntimeError::BuilderError(err)),
							_,
						) => err,
						_ => panic!("Negative test sample returned incorrect error!\n{err}"),
					},
					Error::Finalize(FinalizeError::BuilderError(err)) => err,
					_ => panic!("Negative test sample returned incorrect error!\n{err}"),
				};
				assert_eq!(builder_error, right);
			}
		}
	};
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

#[test]
fn basic_test() {
	let data = r"
name = '?!#_AAA 648';
hint = 'This should parse correctly!';

# Here we declare all the vertex names
# The vertex function returns a value of type vertex
# Vertices have reference semantics (a copy of the variable will represent the same vertex)
x = vertex(box());

# Here we declare cycles
# First one declares a cycle, with a modifier specifying whether it needs a player
# Leaving the modifier out defaults to an automatic playerless cycle
# Next comes a list of vertices which lie on the circle in a clockwise order.
# They can be actual vertices, objects, glyphs, or blank values (underscores).
# For all types except a pre-existing vertex, the cycle function creates a new vertex at call time.
# Cycles also have reference semantics
cycle_a = cycle('manual'; box() vertex(player(), button()) _ x);
cycle_b = cycle('auto'; x _ flag() _);

# Oops, we forgot a cycle, let's add one
# This defaults to an automatic cycle
cycle_extra = cycle(q = vertex(), r = vertex(), s = vertex());

# Objects and glaphs can also be added subsequently
set_thing(q r; box());

# Cycles can be linked together
link(cycle_a cycle_extra);

# We have to position the cycles with x y radius data
circle(cycle_a; -100, 0.0, 100);
circle(cycle_b; +100, 0, 100);
circle(cycle_extra; -100, 100, sqrt(41));
";
	let level = expect_fully_ok!(parse(data));
	assert_eq!(level.name, "?!#_AAA 648");
}

#[test]
fn unplaced_cycle() {
	let test_case = "cycle(_ _ _ _);";
	assert_err_eq!(parse(test_case), LevelBuilderError::UnplacedCycle(0));
}

#[test]
fn test_structural_validation() {
	let test_cases = r"
# Linking a cycle to itself is already weird, but legal.
# The link, however, cannot be inverted.
a = cycle(_ _);
circle(a; 0 0 1);
link('invert'; a a);

# Intersecting cycles cannot be linked.
a = cycle(x = vertex(), _);
b = cycle(x _);
circle(a; 0 0 1);
circle(b; 0 0 1);
link(a b);

# Two (declared) links between the same two cycles may exist as well,
# but they cannot be conflicting like this.
a = cycle(_ _);
b = cycle(_ _);
circle(a; 0 0 1);
circle(b; 0 0 1);
link(a b);
link('invert'; b a);

# Three cycles linked in a triangle.
# Again, this is legal, but the links must be compatible.
a = cycle(_ _);
b = cycle(_ _);
c = cycle(_ _);
circle(a; 0 0 1);
circle(b; 0 0 1);
circle(c; 0 0 1);
link(a b c);
link('invert'; a c);

# Same with four cycles
a = cycle(_ _);
b = cycle(_ _);
c = cycle(_ _);
d = cycle(_ _);
circle(a; 0 0 1);
circle(b; 0 0 1);
circle(c; 0 0 1);
circle(d; 0 0 1);
link(a b c d);
link('invert'; a d);

# Cycles 1 and 3 share a vertex.
# They are not linked directly, but the links connect them transitively.
a = cycle(x = vertex(), _);
b = cycle(_ _);
c = cycle(_ x);
circle(a; 0 0 1);
circle(b; 0 0 1);
circle(c; 0 0 1);
link(a b c);
";

	let expected_results = [
		LevelBuilderError::CycleLinkageConflict(0, 0),
		LevelBuilderError::OverlappedLinkedCycles(OverlappedLinkedCyclesError {
			source_cycle: 1,
			dest_cycle: 0,
			shared_vertex: 0,
		}),
		LevelBuilderError::CycleLinkageConflict(1, 0),
		LevelBuilderError::CycleLinkageConflict(0, 2),
		LevelBuilderError::CycleLinkageConflict(0, 3),
		LevelBuilderError::OverlappedLinkedCycles(OverlappedLinkedCyclesError {
			source_cycle: 2,
			dest_cycle: 0,
			shared_vertex: 0,
		}),
	];

	for (data, expected) in test_cases.split("\n\n").zip(expected_results) {
		assert_err_eq!(parse(data), expected);
	}
}

#[test]
fn logical_colors_test() {
	let data = r"
# Default, with no call_color
a = vertex(box());

# Numeric colors are specified with numbers
b = vertex(box(42));

# Pictogram colors are specified by name
c = vertex(box('star'));

# Pictogram colors may also be specified by their id
d = vertex(box(pict(16)));

# Colorless object can be forced by explicit blank color value
e = vertex(box(_));

z = cycle(a b c d e);
circle(z; 0 0 100);
";

	let expected_colors = [
		None,
		Some(LogicalColor::new(42)),
		Some(LogicalColor::pictogram(36)),
		Some(LogicalColor::pictogram(16)),
		None,
	];

	let level = expect_fully_ok!(parse(data));
	for (vertex, expected_color) in level.vertices.iter().zip(expected_colors) {
		let Some(ObjectData::Box(call_color)) = vertex.object else {
			panic!("Vertex does not contain a box.");
		};
		assert_eq!(call_color, expected_color);
	}
}

#[test]
fn color_labels_test() {
	let data = r"
col = color(0);

# Color labels can be placed to a set of
# predefined positions
a = vertex(button(col; 'left'));
b = vertex(button(col; 'right'));
c = vertex(button(col; 'above'));
d = vertex(button(col; 'below'));

# The default position is inside the button.
# This can also be forced manually
g = vertex(button(col; _));
h = vertex(button(col));

# Position can be specified manually as rotation
# (as clock angle in degrees)
i = vertex(button(col; 30));

# Adding the 'rot' flag means the label itself will be
# rotated, not just positioned
j = vertex(button(col; 'rot' 60));

# Finally, any option can be combined with a shape specifier
# to choose between square label (default) or an arrow-tipped one
k = vertex(button(col; 'above' 'square'));
l = vertex(button(col; _ 'arrow'));

# Put them all on a cycle so the level builder passes
z = cycle(a b c d g h i j k l);
circle(z; 0 0 100);

m = vertex(button(col));
n = vertex(button(col));
o = vertex(button(col));
p = vertex(button(col));
q = vertex(button(col));
r = vertex(button(col));
s = vertex(button(col));
t = vertex(button(col));

x = cycle(m, n, o, p);
y = cycle(q, r, s, t);
circle(x; 0, 0, 100);
circle(y; 0, 0, 100);

hint_vertex(m; 1 100);
hint_vertex(q; 1 100);

# Labels can be positioned symmetrically around a cycle,
# with respect to where they are relative to the cycle center
cycle_color_labels(x; 'quad');

# Cycle-wide label placement may also be done inside the cycle
# and with arrow labels if desired
cycle_color_labels(y; 'lr' 'in' 'arrow');
";
	let expected_appearences = [
		ButtonColorLabelAppearence {
			position: ButtonColorLabelPosition::AnglePlaced(-PI / 2.0),
			has_arrow_tip: false,
		},
		ButtonColorLabelAppearence {
			position: ButtonColorLabelPosition::AnglePlaced(PI / 2.0),
			has_arrow_tip: false,
		},
		ButtonColorLabelAppearence {
			position: ButtonColorLabelPosition::AnglePlaced(0.0),
			has_arrow_tip: false,
		},
		ButtonColorLabelAppearence {
			position: ButtonColorLabelPosition::AnglePlaced(PI),
			has_arrow_tip: false,
		},
		ButtonColorLabelAppearence {
			position: ButtonColorLabelPosition::Inside,
			has_arrow_tip: false,
		},
		ButtonColorLabelAppearence {
			position: ButtonColorLabelPosition::Inside,
			has_arrow_tip: false,
		},
		ButtonColorLabelAppearence {
			position: ButtonColorLabelPosition::AnglePlaced(PI / 6.0),
			has_arrow_tip: false,
		},
		ButtonColorLabelAppearence {
			position: ButtonColorLabelPosition::AngleRotated(PI / 3.0),
			has_arrow_tip: false,
		},
		ButtonColorLabelAppearence {
			position: ButtonColorLabelPosition::AnglePlaced(0.0),
			has_arrow_tip: false,
		},
		ButtonColorLabelAppearence {
			position: ButtonColorLabelPosition::Inside,
			has_arrow_tip: true,
		},
		ButtonColorLabelAppearence {
			position: ButtonColorLabelPosition::AnglePlaced(0.0),
			has_arrow_tip: false,
		},
		ButtonColorLabelAppearence {
			position: ButtonColorLabelPosition::AnglePlaced(PI / 2.0),
			has_arrow_tip: false,
		},
		ButtonColorLabelAppearence {
			position: ButtonColorLabelPosition::AnglePlaced(PI),
			has_arrow_tip: false,
		},
		ButtonColorLabelAppearence {
			position: ButtonColorLabelPosition::AnglePlaced(PI * 1.5),
			has_arrow_tip: false,
		},
		ButtonColorLabelAppearence {
			position: ButtonColorLabelPosition::AnglePlaced(PI * 1.5),
			has_arrow_tip: true,
		},
		ButtonColorLabelAppearence {
			position: ButtonColorLabelPosition::AnglePlaced(PI * 1.5),
			has_arrow_tip: true,
		},
		ButtonColorLabelAppearence {
			position: ButtonColorLabelPosition::AnglePlaced(PI / 2.0),
			has_arrow_tip: true,
		},
		ButtonColorLabelAppearence {
			position: ButtonColorLabelPosition::AnglePlaced(PI / 2.0),
			has_arrow_tip: true,
		},
	];

	let level = expect_fully_ok!(parse(data));
	for (vertex, expected_appearence) in level.vertices.iter().zip(expected_appearences) {
		let Some(GlyphData::Button(Some((_, appearence)))) = vertex.glyph else {
			panic!("Vertex does not contain a colored button.");
		};
		assert_eq!(appearence, expected_appearence);
	}
}

#[test]
fn one_way_parse_test() {
	let level_header = r"
v1 = vertex();
v2 = vertex();
v3 = vertex();
v4 = vertex();
v5 = vertex();
v6 = vertex();
vx = vertex();
vy = vertex();
y = cycle(vy);
c1 = cycle(v1);
c2 = cycle(v2);
c3 = cycle(v3);
c4 = cycle(v4);
c5 = cycle(v5);
c6 = cycle(v6);
x = cycle(vx);

link(c6 x);
link('invert'; c1 y);

circle(c1; 0 0 100);
circle(c2; 0 0 100);
circle(c3; 0 0 100);
circle(c4; 0 0 100);
circle(c5; 0 0 100);
circle(c6; 0 0 100);
circle(x; 0 0 100);
circle(y; 0 0 100);
";
	let linkages = r"
# CHAIN
oneway(c1 c2);
oneway(c2 c3);
oneway(c3 c4);
oneway(c4 c5);
oneway(c5 c6);

# CHAIN, inversely written
oneway(c5 c6);
oneway(c4 c5);
oneway(c3 c4);
oneway(c2 c3);
oneway(c1 c2);

# CHAIN, weird order
oneway(c3 c4);
oneway(c2 c3);
oneway(c4 c5);
oneway(c1 c2);
oneway(c5 c6);

# CHAIN, reverse
oneway(c2 c1);
oneway(c3 c2);
oneway(c4 c3);
oneway(c5 c4);
oneway(c6 c5);

# TREE
oneway(c1 c2);
oneway(c1 c3);
oneway(c2 c4);
oneway(c2 c5);
oneway(c3 c6);

# TREE, different order
oneway(c2 c4);
oneway(c2 c5);
oneway(c1 c2);
oneway(c1 c3);
oneway(c3 c6);

# DIAMOND
oneway(c1 c2);
oneway(c1 c3);
oneway(c2 c4);
oneway(c3 c4);

# DIAMOND, different order
oneway(c2 c4);
oneway(c3 c4);
oneway(c1 c2);
oneway(c1 c3);

# COMPLETE GRAPH
oneway(c1 c2);
oneway(c1 c3);
oneway(c1 c4);
oneway(c1 c5);
oneway(c1 c6);
oneway(c2 c3);
oneway(c2 c4);
oneway(c2 c5);
oneway(c2 c6);
oneway(c3 c4);
oneway(c3 c5);
oneway(c3 c6);
oneway(c4 c5);
oneway(c4 c6);
oneway(c5 c6);

# COMPLETE GRAPH, different order
oneway(c1 c2);
oneway(c2 c4);
oneway(c2 c5);
oneway(c1 c6);
oneway(c2 c6);
oneway(c4 c6);
oneway(c1 c3);
oneway(c3 c5);
oneway(c3 c4);
oneway(c5 c6);
oneway(c2 c3);
oneway(c3 c6);
oneway(c4 c5);
oneway(c1 c4);
oneway(c1 c5);

# COMPLETE GRAPH, DOUBLED LINKS
oneway(c1 c2);
oneway(c1 c3);
oneway(c1 c4);
oneway(c1 c5);
oneway(c1 c6);
oneway(c2 c3);
oneway(c2 c4);
oneway(c2 c5);
oneway(c2 c6);
oneway(c3 c4);
oneway(c3 c5);
oneway(c3 c6);
oneway(c4 c5);
oneway(c4 c6);
oneway(c5 c6);
oneway(c1 c2);
oneway(c2 c4);
oneway(c2 c5);
oneway(c1 c6);
oneway(c2 c6);
oneway(c4 c6);
oneway(c1 c3);
oneway(c3 c5);
oneway(c3 c4);
oneway(c5 c6);
oneway(c2 c3);
oneway(c3 c6);
oneway(c4 c5);
oneway(c1 c4);
oneway(c1 c5);
"
	.split("\n\n");

	for data in linkages {
		let level = format!("{level_header}\n{data}");
		expect_fully_ok!(parse(&level));
	}
}

#[test]
fn one_way_validation_test() {
	let level_header = r"
v1 = vertex();
v2 = vertex();
v3 = vertex();
v4 = vertex();
v5 = vertex();
v6 = vertex();
vx = vertex();
vy = vertex();
y = cycle(vy);
c1 = cycle(v1);
c2 = cycle(v2);
c3 = cycle(v3);
c4 = cycle(v4);
c5 = cycle(v5);
c6 = cycle(v6);
x = cycle(vx);

link(c6, x);
link('invert'; c1 y);

circle(c1; 0 0 100);
circle(c2; 0 0 100);
circle(c3; 0 0 100);
circle(c4; 0 0 100);
circle(c5; 0 0 100);
circle(c6; 0 0 100);
circle(x; 0 0 100);
circle(y; 0 0 100);
";
	let linkages = r"
# CHAIN with LOOP
oneway(c1 c2);
oneway(c2 c3);
oneway(c3 c4);
oneway(c4 c5);
oneway(c5 c6);
oneway(c6 c1);

# CHAIN with LINK LOOP
oneway(c1 c2);
oneway(c2 c3);
oneway(c3 c4);
oneway(c4 c5);
oneway(c5 c6);
link('invert'; c2 c5);

# OVERLAPPING LINK AND ONEWAY
oneway(c1 c2);
link(c1 c2);

# OVERLAPPING ONEWAYS
oneway(c1 c2);
oneway(c2 c1);

# TRIANGLE
oneway(c1 c2);
oneway(c3 c1);
oneway(c2 c3);
"
	.split("\n\n");

	for data in linkages {
		let level = format!("{level_header}\n{data}");
		assert_err_eq!(
			parse(&level),
			LevelBuilderError::OneWayLinkLoop(OneWayLinkLoopError)
		);
	}
}

#[test]
fn one_way_output_test() {
	let level = "
name = 'test one way triangle';

A = cycle(player() _);
B = cycle(box() _);
C = cycle(box() _);

circle(A; 0, 0, 1);
circle(B; 0, 0, 1);
circle(C; 0, 0, 1);

oneway(A, B);
oneway(B, C);
oneway(A, C);";
	let output = expect_fully_ok!(parse(level));
	let ordering: Vec<usize> = output
		.execution_order
		.iter()
		.map(|step| match step {
			DetectorOrGroup::Group(id) => *id,
			DetectorOrGroup::Detector(_) => panic!("There are no detectors"),
		})
		.collect();
	let link_1 = OneWayLinkData {
		target_group: ordering[1],
		direction: LinkedCycleDirection::Coincident,
		multiplicity: 1,
	};
	let link_2 = OneWayLinkData {
		target_group: ordering[2],
		direction: LinkedCycleDirection::Coincident,
		multiplicity: 1,
	};
	// First cycle is linked to two groups
	assert!(
		output.groups[ordering[0]].linked_groups == vec![link_1, link_2]
			|| output.groups[ordering[0]].linked_groups == vec![link_2, link_1],
		"{:?}",
		output.groups[ordering[0]]
	);
	// Second cycle is linked to last group
	assert_eq!(output.groups[ordering[1]].linked_groups, vec![link_2]);
	// Last cycle isn't linked
	assert_eq!(output.groups[ordering[2]].linked_groups, vec![]);
}

#[test]
fn one_way_output_test_variadic() {
	let level = "
name = 'test one way line';

A = cycle(player() _);
B = cycle(box() _);
C = cycle(box() _);

circle(A; 0, 0, 1);
circle(B; 0, 0, 1);
circle(C; 0, 0, 1);

oneway(A, B, C);";
	let output = expect_fully_ok!(parse(level));
	let ordering: Vec<usize> = output
		.execution_order
		.iter()
		.map(|step| match step {
			DetectorOrGroup::Group(id) => *id,
			DetectorOrGroup::Detector(_) => panic!("There are no detectors"),
		})
		.collect();
	let link_1 = OneWayLinkData {
		target_group: ordering[1],
		direction: LinkedCycleDirection::Coincident,
		multiplicity: 1,
	};
	let link_2 = OneWayLinkData {
		target_group: ordering[2],
		direction: LinkedCycleDirection::Coincident,
		multiplicity: 1,
	};
	// First cycle is linked to second cycle
	assert_eq!(output.groups[ordering[0]].linked_groups, vec![link_1]);
	// Second cycle is linked to last cycle
	assert_eq!(output.groups[ordering[1]].linked_groups, vec![link_2]);
	// Last cycle isn't linked
	assert_eq!(output.groups[ordering[2]].linked_groups, vec![]);
}

#[test]
fn detector_basic_test() {
	let level = r"
name = 'detector test';

d = detector();

c1 = cycle(d _);
c2 = cycle(_ d);

target = cycle();

oneway(d, target);

circle(c1; 0,0,1);
circle(c2; 0,0,1);
circle(target; 0,0,1);
";
	expect_fully_ok!(parse(level));
}

#[test]
fn detector_oneway_integration_test() {
	let level = r"
name = 'detector+oneway test';

d = detector();
d2 = detector();

source = cycle(_);

c1 = cycle(d _);
c2 = cycle(_ d);

target1 = cycle();
target2 = cycle(_ d2);
target3 = cycle();

oneway(source, c1);
oneway(source, c2);
oneway(d, target1);
oneway(target1, target2, target3);
oneway(d2, target3);

circle(c1; 0,0,1);
circle(c2; 0,0,1);
circle(source; 0,0,1);
circle(target1; 0,0,1);
circle(target2; 0,0,1);
circle(target3; 0,0,1);
";
	expect_fully_ok!(parse(level));
}

#[test]
fn detector_link_integration_test() {
	let level = r"
name = 'detector+oneway+link test';

d0 = detector();
d = detector();

source1 = cycle(_ d0);
source2 = cycle(_ d0);

c1 = cycle(d _);
c2 = cycle(_ d);

target = cycle();

link(source1, source2);

oneway(source1, c1);
oneway(source1, c2);
oneway(d0, c1);
oneway(d, target);

link(c1, c2);

circle(c1; 0,0,1);
circle(c2; 0,0,1);
circle(source1; 0,0,1);
circle(source2; 0,0,1);
circle(target; 0,0,1);
";
	expect_fully_ok!(parse(level));
}

#[test]
fn detector_loop_test() {
	let level = r"
name = 'detector loop test';

d1 = detector();
d2 = detector();
d3 = detector();

c1 = cycle(_ d1);
c2 = cycle(_ d2);
c3 = cycle(_ d3);

oneway(d1, c2);
oneway(d2, c3);
oneway(d3, c1);

circle(c1; 0,0,1);
circle(c2; 0,0,1);
circle(c3; 0,0,1);
";
	assert_err_eq!(
		parse(level),
		LevelBuilderError::OneWayLinkLoop(OneWayLinkLoopError)
	);
}

#[test]
fn detector_loop_complex_test() {
	let level_header = r"
name = 'detector loop stress test';

d1 = detector();
d2 = detector();
d7 = detector();

c1 = cycle(_ d1);
c2 = cycle(_ d2);
c3 = cycle(_);
c4 = cycle(_);
c5 = cycle(_);
c6 = cycle(_);
c7 = cycle(_ d7);

circle(c1; 0,0,1);
circle(c2; 0,0,1);
circle(c3; 0,0,1);
circle(c4; 0,0,1);
circle(c5; 0,0,1);
circle(c6; 0,0,1);
circle(c7; 0,0,1);
";
	let loop_statements = r"oneway(d1, c2);
oneway(d2, c3);
link(c3, c4);
link(c4, c5);
oneway(c5, c6);
link(c6, c7);
oneway(d7, c1);"
		.split("\n")
		.collect::<Vec<_>>();

	// Sanity check so we don't go through a bagintillion permtuations on accident
	assert!(loop_statements.len() <= 7);

	// Test every ordering
	for statements in loop_statements.iter().permutations(loop_statements.len()) {
		let level = format!("{}\n{}", level_header, statements.into_iter().join("\n"));
		assert_err_eq!(
			parse(&level),
			LevelBuilderError::OneWayLinkLoop(OneWayLinkLoopError)
		);
	}
}

#[test]
fn detector_unused_test() {
	let level = r"
name = 'unused detector test';

unused = detector();
unplaced = detector();
unlinked = detector();

c1 = cycle(_ unlinked);

oneway(unplaced, c1);

circle(c1; 0,0,1);
";
	expect_fully_ok!(parse(level));
}

#[test]
fn empty_cycle_detector_test() {
	let level = r"
name = 'empty cycle detector test';

d = detector();

cycle = cycle(d);

circle(cycle; 0,0,1);
";
	assert_err_eq!(parse(level), LevelBuilderError::DetectorOnEmptyCycle);
}

mod layout_tests {
	use super::*;
	use bevy::math::Vec2;
	use rand::{seq::SliceRandom, SeedableRng as _};

	#[test]
	fn basic_metatest() {
		expect_fully_ok!(parse(r""));
	}

	#[test]
	fn basic_unicycle() {
		expect_fully_ok!(parse(r"circle(cycle(_ _ _ _ _); 3, 5, 20);"));
	}

	#[test]
	fn basic_dicycles_single() {
		expect_fully_ok!(parse(
			r"
		v = vertex();
		circle(cycle(v _ _ _ _); -10, 0, 10);
		circle(cycle(v _ _ _ _); +10, 0, 10);
		",
		));

		expect_fully_ok!(parse(
			r"
		v = vertex();
		circle(cycle(v _); -7, 0, 10);
		circle(cycle(v); +7, 0, 10);
		",
		));

		expect_fully_ok!(parse(
			r"
		v = vertex();
		circle(cycle(v _ _ _ _); 0, 0, 10);
		circle(cycle(v _ _ _ _); 0, 0, 10);
		",
		));
	}

	#[test]
	fn basic_dicycles_double() {
		let result = parse(
			r"
		a = vertex();
		b = vertex();
		circle(cycle(a b _ _ _ _); -10, 0, 10);
		circle(cycle(b a _ _ _ _); +10, 0, 10);
		",
		)
		.expect("Level did not produce any partial data");
		assert!(result.errors.iter().any(|err| matches!(
			err,
			LevelBuilderError::VertexSolverError(VertexSolverError::TwoVerticesCollide { .. })
		)));

		let result = parse(
			r"
		a = vertex();
		b = vertex();
		circle(cycle(a b); -10, 0, 10);
		circle(cycle(b a); +10, 0, 10);
		",
		)
		.expect("Level did not produce any partial data");
		assert!(result.errors.iter().any(|err| matches!(
			err,
			LevelBuilderError::VertexSolverError(VertexSolverError::TwoVerticesCollide { .. })
		)));

		expect_fully_ok!(parse(
			r"
		a = vertex();
		b = vertex();
		circle(cycle(a b _ _ _ _); -7, 0, 10);
		circle(cycle(b a _ _ _ _); +7, 0, 10);
		",
		));

		expect_fully_ok!(parse(
			r"
		a = vertex();
		b = vertex();
		circle(cycle(a b _ _ _ _); -7, 0, 10);
		circle(cycle(b a _ _ _ _); +7, 0, 10);
		",
		));

		expect_fully_ok!(parse(
			r"
		a = vertex();
		b = vertex();
		circle(cycle(a b _ _ _); 0, 0, 10);
		circle(cycle(a _ b _ _); 0, 0, 10);
		",
		));
	}

	#[test]
	fn unique_incorrect_solution() {
		expect_fully_ok!(parse(
			r"
name = 'Correct flower';

v1 = vertex();
v2 = vertex();
v3 = vertex();
v4 = vertex();
v5 = vertex();
v6 = vertex();

centre = vertex();

radius = 100;
third_turn = 2*pi/3;

circle(a = cycle('manual'; v1 v2 v3 centre); sin(third_turn * 0) * radius, cos(third_turn * 0) * radius, radius);
circle(b = cycle('manual'; v3 v4 v5 centre); sin(third_turn * 1) * radius, cos(third_turn * 1) * radius, radius);
circle(c = cycle('manual'; v5 v6 v1 centre); sin(third_turn * 2) * radius, cos(third_turn * 2) * radius, radius);
		",
		));

		// This should not parse, because it's a mirrored layout, which makes the vertices go counterclockwise
		// This conflicts with the fact that vertices in their declared order should be clockwise
		let result = parse(
			r"
name = 'Inverted flower';

v1 = vertex();
v2 = vertex();
v3 = vertex();
v4 = vertex();
v5 = vertex();
v6 = vertex();

centre = vertex();

radius = 100;
third_turn = 2*pi/3;

circle(a = cycle('manual'; v3 v2 v1 centre); sin(third_turn * 0) * radius, cos(third_turn * 0) * radius, radius);
circle(b = cycle('manual'; v5 v4 v3 centre); sin(third_turn * 1) * radius, cos(third_turn * 1) * radius, radius);
circle(c = cycle('manual'; v1 v6 v5 centre); sin(third_turn * 2) * radius, cos(third_turn * 2) * radius, radius);
		",
		)
		.expect("Level did not produce any partial data");
		assert!(result.errors.iter().any(|err| matches!(
			err,
			LevelBuilderError::VertexSolverError(VertexSolverError::VerticesNotClockwise { .. })
		)));
	}

	#[test]
	fn put_vertex_in_wrong_order() {
		let result = parse(
			r"
circle(cycle(a = vertex(), b = vertex(), c = vertex()); 0 0 1);
put_vertex(a; 0 1);
put_vertex(b; -1, 0);
put_vertex(c; 1 0);",
		);
		assert_err_eq!(
			result,
			LevelBuilderError::VertexSolverError(VertexSolverError::VerticesNotClockwise {
				cycle: 0,
				vertices: [0, 1, 2]
			})
		);
	}

	#[test]
	fn put_vertex_outside_cycle() {
		let result = parse(r"circle(cycle(a = vertex()); 0 0 1); put_vertex(a; 0 2);");
		assert_err_eq!(
			result,
			LevelBuilderError::VertexSolverError(VertexSolverError::CycleDoesNotContainVertex(
				CycleDoesNotContainVertexError {
					cycle: 0,
					placement: CyclePlacement {
						position: Vec2::ZERO,
						shape: CycleShape::Circle(1.0)
					},
					vertex: 0,
					position: Vec2::new(0.0, 2.0)
				}
			))
		);
	}

	#[test]
	fn vertex_without_cycle() {
		let result = parse("vertex();");
		assert_err_eq!(
			result,
			LevelBuilderError::VertexSolverError(VertexSolverError::VertexHasNoCycle(0))
		);

		let result = parse("put_vertex(vertex(); 0 0);");
		assert_err_eq!(
			result,
			LevelBuilderError::VertexSolverError(VertexSolverError::VertexHasNoCycle(0))
		);
	}

	#[test]
	fn indirect_hints() {
		expect_fully_ok!(parse(
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
		expect_fully_ok!(parse(include_str!("../../../../../assets/levels/car.txt")));
	}

	#[test]
	fn olympic() {
		// A chain of cycles can also be automatically resolved
		expect_fully_ok!(parse(include_str!(
			"../../../../../assets/levels/olympic.txt"
		)));
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
				expect_fully_ok!(parse(
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
		expect_fully_ok!(parse(
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
		expect_fully_ok!(parse(
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
		expect_fully_ok!(parse(
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
		expect_fully_ok!(parse(
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
yh = -sep / 2 / sqrt(3);
hint_vertex(bri; 0, yh);
hint_vertex(rgi; 0, yh);
hint_vertex(bgi; 0, yh);
",
		));
	}

	/// The layout solver should be able to solve the tricycle layout with any combination of the three hints.
	/// In any permutation, and even if a single hint is used (even multiple times)
	#[test]
	fn tricycle_interleaved_hint_permutations() {
		let level = r"
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
yh = -sep / 2 / sqrt(3);
";
		let hints = [
			"hint_vertex(bri; 0, yh);",
			"hint_vertex(rgi; 0, yh);",
			"hint_vertex(bgi; 0, yh);",
		];
		// Single hints
		for hint in hints.iter() {
			expect_fully_ok!(parse(&(level.to_owned() + hint)));
		}
		// Arbitrary hint combination
		for hint_combo in hints.iter().combinations_with_replacement(3) {
			let hint = hint_combo.into_iter().join("");
			println!("{hint}");
			expect_fully_ok!(parse(&(level.to_owned() + &hint)));
		}
	}

	/// The layout solver should be able to solve the tricycle layout independent of vertex order
	#[test]
	fn tricycle_interleaved_vertex_permutations() {
		let vertex_array = [
			"bri = vertex();",
			"rgi = vertex();",
			"bgi = vertex();",
			"bro = vertex();",
			"rgo = vertex();",
			"bgo = vertex();",
		];

		// Pls don't do a trillion permutations
		assert!(vertex_array.len() <= 6);
		for permutation in vertex_array.iter().permutations(vertex_array.len()) {
			let vertices = permutation.into_iter().join("\n");
			expect_fully_ok!(parse(&format!(
				r"
r = 1;
sep = 1.3;

{vertices}
circle(cycle('manual'; _ _ _ _ bgo bri bgi bro); -sep / 2, 0, r);
circle(cycle('manual'; _ _ _ _ bro rgi bri rgo); 0, -sep / 2 * sqrt(3), r);
circle(cycle('manual'; _ _ _ _ rgo bgi rgi bgo); sep / 2, 0, r);

# Hint that the central vertices go near the center
yh = -sep / 2 / sqrt(3);
hint_vertex(bri; 0, yh);
"
			)));
		}
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
			expect_fully_ok!(parse(flower));
		}
		expect_fully_ok!(parse(&flowers.iter().join("")));
	}
}
