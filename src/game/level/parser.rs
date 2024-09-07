use super::{builder::*, lex::*, *};
use bevy::utils::hashbrown::HashMap;
use itertools::Itertools as _;

pub fn parse(level_file: &str) -> Result<LevelData, LevelParsingError> {
	let mut builder = LevelBuilder::new();
	let mut vertex_names = HashMap::new();
	let mut cycle_names = HashMap::new();
	let mut last_line_number = 0;

	for line in lex::parse(level_file) {
		let (line_number, statement) = line?;
		last_line_number = line_number;
		let result = parse_statement(statement, &mut builder, &mut vertex_names, &mut cycle_names);
		if let Err(err) = result {
			return Err(err.at_line(line_number));
		}
	}

	match builder.build() {
		Ok(level) => Ok(level),
		Err(err) => Err(LevelParsingErrorCode::BuilderError(err).at_line(last_line_number)),
	}
}

fn parse_statement(
	statement: RawStatement,
	builder: &mut LevelBuilder,
	vertex_names: &mut HashMap<String, usize>,
	cycle_names: &mut HashMap<String, usize>,
) -> Result<(), LevelParsingErrorCode> {
	match statement {
		RawStatement::Assignment(statement) => {
			match statement.key.to_ascii_lowercase().as_str() {
				"name" => builder.set_level_name(statement.value.to_owned())?,
				"hint" => builder.set_level_hint(statement.value.to_owned())?,
				_ => {
					return Err(LevelParsingErrorCode::InvalidMetaVariable(
						statement.key.to_owned(),
					));
				}
			}
		}
		RawStatement::Action(statement) => {
			match statement.verb {
				"VERTEX" => {
					for name in statement.values {
						if vertex_names.contains_key(name) {
							continue;
						}
						let vertex = builder.add_vertex()?;
						vertex_names.insert(name.to_owned(), vertex);
					}
				}
				"CYCLE" => {
					let turnability = match statement.modifier {
						None => CycleTurnability::Always,
						Some("MANUAL") => CycleTurnability::WithPlayer,
						Some("ENGINE") => CycleTurnability::Always,
						Some("STILL") => CycleTurnability::Never,
						Some(m) => {
							return Err(LevelParsingErrorCode::InvalidModifier(m.to_string()))
						}
					};
					let mut values = statement.values.into_iter();
					let name = values
						.next()
						.ok_or(LevelParsingErrorCode::NotEnoughArguments(1, 0))?;
					if cycle_names.contains_key(name) {
						return Err(LevelParsingErrorCode::RedefinedCycle(name.to_owned()));
					}
					let vertices = values
						.map(|name| {
							vertex_names.get(name).copied().ok_or_else(|| {
								LevelParsingErrorCode::UnknownVertexName(name.to_owned())
							})
						})
						.collect::<Result<Vec<_>, _>>()?;
					let cycle = builder.add_cycle(turnability, vertices)?;
					cycle_names.insert(name.to_owned(), cycle);
				}
				"LINK" => {
					let direction = match statement.modifier {
						None => LinkedCycleDirection::Coincident,
						Some("STRAIGHT") => LinkedCycleDirection::Coincident,
						Some("CROSSED") => LinkedCycleDirection::Inverse,
						Some(m) => {
							return Err(LevelParsingErrorCode::InvalidModifier(m.to_string()))
						}
					};
					let cycles = statement
						.values
						.into_iter()
						.map(|name| {
							cycle_names.get(name).copied().ok_or_else(|| {
								LevelParsingErrorCode::UnknownCycleName(name.to_owned())
							})
						})
						.collect::<Result<Vec<_>, _>>()?;
					for (source, dest) in cycles.into_iter().tuple_windows() {
						builder.link_cycles(source, dest, direction)?;
					}
				}
				"OBJECT" => {
					let Some(modifier) = statement.modifier else {
						return Err(LevelParsingErrorCode::MissingModifier);
					};
					let (object_kind, color) =
						modifier.split_once(':').unwrap_or((modifier, ""));
					let _color_id = if color.is_empty() {
						None
					} else if let Ok(color_id) = color.parse() {
						Some(LogicalColor(color_id))
					} else {
						return Err(LevelParsingErrorCode::InvalidModifier(modifier.to_string()));
					};
					let thing_type = match object_kind {
						"BOX" => ThingType::Object(ObjectType::Box),
						"PLAYER" => ThingType::Object(ObjectType::Player),
						"BUTTON" => ThingType::Glyph(GlyphType::Button),
						"FLAG" => ThingType::Glyph(GlyphType::Flag),
						_ => {
							return Err(LevelParsingErrorCode::InvalidModifier(
								modifier.to_string(),
							))
						}
					};
					for vertex_name in statement.values {
						let Some(vertex) = vertex_names.get(vertex_name).copied() else {
							return Err(LevelParsingErrorCode::UnknownVertexName(
								vertex_name.to_owned(),
							));
						};
						// TODO: Bring back colors
						match thing_type {
							ThingType::Object(object_type) => builder.add_object(
								vertex,
								ObjectData {
									object_type,
									color: None,
								},
							)?,
							ThingType::Glyph(glyph_type) => builder.add_glyph(
								vertex,
								GlyphData {
									glyph_type,
									color: None,
								},
							)?,
						}
					}
				}
				"PLACE" => {
					if let Some(modifier) = statement.modifier {
						return Err(LevelParsingErrorCode::InvalidModifier(modifier.to_owned()));
					}
					if statement.values.len() < 4 {
						return Err(LevelParsingErrorCode::NotEnoughArguments(
							4,
							statement.values.len(),
						));
					}
					let cycle_name = statement.values[0];
					let x_str = statement.values[1].replace(',', ".");
					let y_str = statement.values[2].replace(',', ".");
					let r_str = statement.values[3].replace(',', ".");
					let Some(cycle_id) = cycle_names.get(cycle_name).copied() else {
						return Err(LevelParsingErrorCode::UnknownCycleName(
							cycle_name.to_string(),
						));
					};
					let Ok(x) = x_str.parse::<f32>() else {
						return Err(LevelParsingErrorCode::ArgumentIsNotANumber(x_str));
					};
					let Ok(y) = y_str.parse::<f32>() else {
						return Err(LevelParsingErrorCode::ArgumentIsNotANumber(y_str));
					};
					let Ok(r) = r_str.parse::<f32>() else {
						return Err(LevelParsingErrorCode::ArgumentIsNotANumber(r_str));
					};
					let hints = statement.values[4..]
						.iter()
						.map(|name| {
							vertex_names
								.get(*name)
								.copied()
								.ok_or(LevelParsingErrorCode::UnknownVertexName(name.to_string()))
						})
						.collect::<Result<Vec<_>, _>>()?;
					builder.place_cycle(cycle_id, Vec2::new(x, y), r, &hints)?;
				}
				"PLACE_VERT" => {
					if let Some(modifier) = statement.modifier {
						return Err(LevelParsingErrorCode::InvalidModifier(modifier.to_owned()));
					}
					if statement.values.len() < 2 {
						return Err(LevelParsingErrorCode::NotEnoughArguments(
							2,
							statement.values.len(),
						));
					} else if statement.values.len() > 2 {
						return Err(LevelParsingErrorCode::ExtraneousArguments(
							2,
							statement.values.len(),
						));
					}
					let vertex_name = statement.values[0];
					let angle_str = statement.values[1].replace(',', ".");
					let Some(vertex_id) = vertex_names.get(vertex_name).copied() else {
						return Err(LevelParsingErrorCode::UnknownVertexName(
							vertex_name.to_string(),
						));
					};
					let Ok(angle) = angle_str.parse::<f32>() else {
						return Err(LevelParsingErrorCode::ArgumentIsNotANumber(angle_str));
					};
					builder.place_vertex(vertex_id, angle)?;
				}
				other => return Err(LevelParsingErrorCode::InvalidKeyword(other.to_owned())),
			}
		}
	}
	Ok(())
}

#[derive(Debug, PartialEq)]
pub enum LevelParsingErrorCode {
	InvalidKeyword(String),
	InvalidModifier(String),
	UnknownVertexName(String),
	UnknownCycleName(String),
	RedefinedCycle(String),
	LexError(LexErrorCode),
	MissingModifier,
	ArgumentIsNotANumber(String),
	InvalidMetaVariable(String),
	BuilderError(LevelBuilderError),
	NotEnoughArguments(usize, usize),
	ExtraneousArguments(usize, usize),
}

#[derive(Debug, PartialEq)]
pub struct LevelParsingError {
	pub code: LevelParsingErrorCode,
	pub line_number: usize,
}

impl LevelParsingErrorCode {
	pub fn at_line(self, line_number: usize) -> LevelParsingError {
		LevelParsingError { code: self, line_number }
	}
}

impl From<LexError> for LevelParsingError {
	fn from(value: LexError) -> Self {
		LevelParsingErrorCode::LexError(value.code).at_line(value.line_number)
	}
}

impl From<LevelBuilderError> for LevelParsingErrorCode {
	fn from(value: LevelBuilderError) -> Self {
		Self::BuilderError(value)
	}
}

impl std::error::Error for LevelParsingError {}

impl std::fmt::Display for LevelParsingErrorCode {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::InvalidKeyword(k) => write!(f, "The keyword {} is invalid.", k)?,
			Self::InvalidModifier(m) => write!(f, "The modifier [{}] is invalid.", m)?,
			Self::UnknownVertexName(name) => write!(
				f,
				"A vertex with name {} was referenced, but not defined.",
				name
			)?,
			Self::UnknownCycleName(name) => write!(
				f,
				"A cycle with name {} was referenced, but not defined.",
				name
			)?,
			Self::RedefinedCycle(name) => {
				write!(f, "A cycle with name {} was defined multiple times.", name)?
			}
			Self::LexError(e) => e.fmt(f)?,
			Self::MissingModifier => write!(f, "Statement is missing a modifier.")?,
			Self::ArgumentIsNotANumber(arg) => {
				write!(f, "Expected a numeric argument, got {arg}.")?
			}
			Self::InvalidMetaVariable(name) => {
				write!(f, "{name} is not a valid meta variable name")?
			}
			Self::BuilderError(e) => e.fmt(f)?,
			Self::NotEnoughArguments(needed, got) => write!(
				f,
				"Statement found {got} arguments, needs at least {needed}."
			)?,
			Self::ExtraneousArguments(expected, got) => write!(
				f,
				"Statement found {got} arguments, expected at most {expected}."
			)?,
		}
		Ok(())
	}
}

impl std::fmt::Display for LevelParsingError {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "Line {}: {}", self.line_number + 1, self.code)
	}
}

#[cfg(test)]
mod test {
	use super::{
		parse, LevelBuilderError, LevelParsingErrorCode, LexErrorCode, OverlappedLinkedCyclesError,
	};

	macro_rules! assert_err_eq {
		($left:expr, $right:expr) => {
			let left = $left;
			let right = $right;
			let err = left.expect_err("Negative test sample parrsed without error!").code;
			assert_eq!(err, right);
		};
	}

	#[test]
	fn basic_test() {
		let data = r"
NAME=?!#_AAA 648
HINT=This should parse correctly!

# Here we declare all the vertex names
# Just list them after the VERTEX, separating by space
VERTEX a b c x 1 2 3 k l m

# Here we declare cycles
# First one declares a cycle, with a modifier specifying whether it needs a player
# Leaving the modifier out defaults to an automatic playerless cycle
# Next comes an identifier for the cycle itself and then a list of vertices which lie on the circle
# in a clockwise order.
CYCLE[MANUAL] cycle_a a b c x
CYCLE[ENGINE] cycle_b x 1 2 3

# To place objects we use OBJECT directives, specifying the desired object in the modifier
# Then we list the vertices upon which the objects should get placed
OBJECT[BOX] x a
OBJECT[FLAG] 2
OBJECT[PLAYER] b
OBJECT[BUTTON] b

# Oops, we forgot a cycle, let's add one
# This defaults to an automatic cycle
CYCLE cycle_extra k l m

# Cycles can be linked together
LINK cycle_a cycle_extra

# We have to position the cycles with x y radius data
PLACE cycle_a -100 0.0 100
PLACE cycle_b +100 0,0 100

PLACE cycle_extra -100 100 50
";
		let level = parse(data).expect("Test sample did not parse correctly!");
		assert_eq!(level.name, "?!#_AAA 648");
	}

	#[test]
	fn undeclared_identifiers_test() {
		let data = r"
# Undeclared vertex in cycle definition. This should not parse.
CYCLE a b c
";
		assert_err_eq!(
			parse(data),
			LevelParsingErrorCode::UnknownVertexName("b".to_owned())
		);

		let data = r"
# Undeclared vertex in object definition. This should not parse.
OBJECT[BOX] a b c
";
		assert_err_eq!(
			parse(data),
			LevelParsingErrorCode::UnknownVertexName("a".to_owned())
		);

		let data = r"
# Undeclared vertex in place command. This should not parse.
PLACE_VERT a 1
";
		assert_err_eq!(
			parse(data),
			LevelParsingErrorCode::UnknownVertexName("a".to_owned())
		);

		let data = r"
# Undeclared vertex in cycle placement hint. This should not parse.
VERTEX a b c
CYCLE k a b c
PLACE k 0 0 100 b d
";
		assert_err_eq!(
			parse(data),
			LevelParsingErrorCode::UnknownVertexName("d".to_owned())
		);

		let data = r"
# Undeclared cycle in link definition. This should not parse.
LINK a b c
";
		assert_err_eq!(
			parse(data),
			LevelParsingErrorCode::UnknownCycleName("a".to_owned())
		);

		let data = r"
# Undeclared cycle in place command. This should not parse.
PLACE a 0 0 100
";
		assert_err_eq!(
			parse(data),
			LevelParsingErrorCode::UnknownCycleName("a".to_owned())
		);
	}

	#[test]
	fn garbage_test() {
		let data = "InvalidKeyword";
		assert_err_eq!(
			parse(data),
			LevelParsingErrorCode::InvalidKeyword("InvalidKeyword".to_owned())
		);

		let data = "OBJECT[InvalidModifier]";
		assert_err_eq!(
			parse(data),
			LevelParsingErrorCode::InvalidModifier("InvalidModifier".to_owned())
		);

		let data = "InvalidMetaVariable=";
		assert_err_eq!(
			parse(data),
			LevelParsingErrorCode::InvalidMetaVariable("InvalidMetaVariable".to_owned())
		);

		let data = "\u{202e}";
		assert_err_eq!(
			parse(data),
			LevelParsingErrorCode::LexError(LexErrorCode::NonAsciiLine)
		);

		let data = "@";
		assert_err_eq!(
			parse(data),
			LevelParsingErrorCode::LexError(LexErrorCode::MalformedStatement)
		);
	}

	#[test]
	fn redefined_identifiers_test() {
		let data = r"
VERTEX a b c
CYCLE k a b c
# Define the cycle again. This should not parse.
CYCLE k a b
";
		assert_err_eq!(
			parse(data),
			LevelParsingErrorCode::RedefinedCycle("k".to_owned())
		);

		let data = r"
# Use the same vertex name multiple times.
# This is fine, but it should not create a new vertex.
VERTEX a a b a c
VERTEX a
";
		let level = parse(data).expect("Test sample did not parse correctly!");
		assert_eq!(level.vertices.len(), 3);
	}

	#[test]
	fn bad_command_syntax_test() {
		let test_cases = r"
# Cycle is missing a name
CYCLE

# Place command is missing a vertex
PLACE_VERT

# Place command is missing a value
VERTEX a
CYCLE k a
PLACE k 0 0 100
PLACE_VERT a

# Place command is missing a cycle
PLACE

# Place command is missing a value
VERTEX a
CYCLE k a
PLACE k 0 0

# Object without explicit type is not valid
VERTEX a
OBJECT a

# Placement is not a number
VERTEX a
CYCLE k a
PLACE k 0 0 100
PLACE_VERT a b

# Placement is not a number
VERTEX a
CYCLE k a
PLACE k a b c

# Place command cannot take modifiers
VERTEX a
CYCLE k a
PLACE[InvalidModifier] k 0 0 100

# Place command cannot take modifiers
VERTEX a
CYCLE k a
PLACE k 0 0 100
PLACE_VERT[InvalidModifier] a 0

# Vertex placement command cannot have more arguments
VERTEX a
CYCLE k a
PLACE k 0 0 100
PLACE_VERT a 0 0
";
		let expected_results = [
			LevelParsingErrorCode::NotEnoughArguments(1, 0),
			LevelParsingErrorCode::NotEnoughArguments(2, 0),
			LevelParsingErrorCode::NotEnoughArguments(2, 1),
			LevelParsingErrorCode::NotEnoughArguments(4, 0),
			LevelParsingErrorCode::NotEnoughArguments(4, 3),
			LevelParsingErrorCode::MissingModifier,
			LevelParsingErrorCode::ArgumentIsNotANumber("b".to_owned()),
			LevelParsingErrorCode::ArgumentIsNotANumber("a".to_owned()),
			LevelParsingErrorCode::InvalidModifier("InvalidModifier".to_owned()),
			LevelParsingErrorCode::InvalidModifier("InvalidModifier".to_owned()),
			LevelParsingErrorCode::ExtraneousArguments(2, 3),
		];

		for (data, expected) in test_cases.split("\n\n").zip(expected_results) {
			assert_err_eq!(parse(data), expected);
		}
	}

	#[test]
	fn test_structural_validation() {
		let test_cases = r"
# Linking a cycle to itself is already weird, but legal.
# The link, however, cannot be inverted.
VERTEX a b
CYCLE 1 a b
LINK[CROSSED] 1 1

# Intersecting cycles cannot be linked.
VERTEX a b c
CYCLE 1 a b
CYCLE 2 a c
LINK 1 2

# Two (declared) links between the same two cycles may exist as well,
# but they cannot be conflicting like this.
VERTEX a b c d
CYCLE 1 a b
CYCLE 2 c d
LINK 1 2
LINK[CROSSED] 2 1

# Three cycles linked in a triangle.
# Again, this is legal, but the links must be compatible.
VERTEX a b c d e f
CYCLE 1 a b
CYCLE 2 c d
CYCLE 3 e f
LINK 1 2 3
LINK[CROSSED] 1 3

# Same with four cycles
VERTEX a b c d e f g h
CYCLE 1 a b
CYCLE 2 c d
CYCLE 3 e f
CYCLE 4 g h
LINK 1 2 3 4
LINK[CROSSED] 1 4

# Cycles 1 and 3 share a vertex.
# They are not linked directly, but the links connect them transitively.
VERTEX a b c d e
CYCLE 1 a b
CYCLE 2 c d
CYCLE 3 e a
LINK 1 2 3
";

		let expected_results = [
			LevelBuilderError::CycleLinkageConflict(0, 0),
			LevelBuilderError::OverlappedLinkedCycles(OverlappedLinkedCyclesError {
				source_cycle: 0,
				dest_cycle: 1,
				shared_vertex: 0,
			}),
			LevelBuilderError::CycleLinkageConflict(0, 0),
			LevelBuilderError::CycleLinkageConflict(0, 2),
			LevelBuilderError::CycleLinkageConflict(0, 3),
			LevelBuilderError::OverlappedLinkedCycles(OverlappedLinkedCyclesError {
				source_cycle: 0,
				dest_cycle: 2,
				shared_vertex: 0,
			}),
		];

		for (data, expected) in test_cases.split("\n\n").zip(expected_results) {
			assert_err_eq!(parse(data), expected.into());
		}
	}
}
