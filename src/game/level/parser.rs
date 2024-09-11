use std::f32::consts::PI;

use super::{builder::*, lex::*, *};
use bevy::utils::hashbrown::HashMap;
use itertools::Itertools as _;

pub fn parse(level_file: &str) -> Result<LevelData, LevelParsingError> {
	let mut state = ParserState::new();
	let mut last_line_number = 0;

	for line in lex::parse(level_file) {
		let (line_number, statement) = line?;
		last_line_number = line_number;
		let result = parse_statement(statement, &mut state);
		if let Err(err) = result {
			return Err(err.at_line(line_number));
		}
	}

	match state.builder.build() {
		Ok(level) => Ok(level),
		Err(err) => Err(LevelParsingErrorCode::BuilderError(err).at_line(last_line_number)),
	}
}

/// Global state of the parser, to avoid making functions with many parameters
struct ParserState {
	builder: LevelBuilder,
	vertex_names: HashMap<String, usize>,
	cycle_names: HashMap<String, usize>,
	override_palette: Vec<LogicalColor>,
	default_box_color: Option<LogicalColor>,
	default_button_color: Option<LogicalColor>,
}

impl ParserState {
	fn new() -> Self {
		Self {
			builder: LevelBuilder::new(),
			vertex_names: HashMap::new(),
			cycle_names: HashMap::new(),
			override_palette: Vec::new(),
			default_box_color: None,
			default_button_color: None,
		}
	}
}

fn parse_statement(
	statement: RawStatement,
	state: &mut ParserState,
) -> Result<(), LevelParsingErrorCode> {
	let ParserState {
		builder,
		vertex_names,
		cycle_names,
		override_palette,
		default_box_color,
		default_button_color,
	} = state;
	match statement {
		RawStatement::Assignment(statement) => match statement.key.to_ascii_lowercase().as_str() {
			"name" => builder.set_level_name(statement.value.to_owned())?,
			"hint" => builder.set_level_hint(statement.value.to_owned())?,
			_ => {
				return Err(LevelParsingErrorCode::InvalidMetaVariable(
					statement.key.to_owned(),
				));
			}
		},
		RawStatement::Action(statement) => match statement.verb {
			"VERTEX" => {
				if !statement.modifier.is_empty() {
					return Err(LevelParsingErrorCode::ExtraneousModifier(
						0,
						statement.modifier.len(),
					));
				}
				for name in statement.values {
					if vertex_names.contains_key(name) {
						continue;
					}
					let vertex = builder.add_vertex()?;
					vertex_names.insert(name.to_owned(), vertex);
				}
			}
			"CYCLE" => {
				if statement.modifier.len() > 1 {
					return Err(LevelParsingErrorCode::ExtraneousModifier(
						1,
						statement.modifier.len(),
					));
				}
				let turnability = match statement.modifier.first().copied() {
					None => CycleTurnability::Always,
					Some("MANUAL") => CycleTurnability::WithPlayer,
					Some("ENGINE") => CycleTurnability::Always,
					Some("STILL") => CycleTurnability::Never,
					Some(m) => return Err(LevelParsingErrorCode::InvalidModifier(m.to_string())),
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
				if statement.modifier.len() > 1 {
					return Err(LevelParsingErrorCode::ExtraneousModifier(
						1,
						statement.modifier.len(),
					));
				}
				let direction = match statement.modifier.first().copied() {
					None => LinkedCycleDirection::Coincident,
					Some("STRAIGHT") => LinkedCycleDirection::Coincident,
					Some("CROSSED") => LinkedCycleDirection::Inverse,
					Some(m) => return Err(LevelParsingErrorCode::InvalidModifier(m.to_string())),
				};
				let cycles = statement
					.values
					.into_iter()
					.map(|name| {
						cycle_names
							.get(name)
							.copied()
							.ok_or_else(|| LevelParsingErrorCode::UnknownCycleName(name.to_owned()))
					})
					.collect::<Result<Vec<_>, _>>()?;
				for (source, dest) in cycles.into_iter().tuple_windows() {
					builder.link_cycles(source, dest, direction)?;
				}
			}
			"OBJECT" => {
				let object_kind = match statement.modifier.first().copied() {
					Some("BOX") => ThingType::Object(ObjectType::Box),
					Some("PLAYER") => ThingType::Object(ObjectType::Player),
					Some("BUTTON") => ThingType::Glyph(GlyphType::Button),
					Some("FLAG") => ThingType::Glyph(GlyphType::Flag),
					Some(other) => {
						return Err(LevelParsingErrorCode::InvalidModifier(other.to_owned()))
					}
					None => return Err(LevelParsingErrorCode::MissingModifier),
				};
				let expected_modifiers = match object_kind {
					ThingType::Object(ObjectType::Box) => 2,
					ThingType::Object(ObjectType::Player) => 1,
					ThingType::Glyph(GlyphType::Button) => 2,
					ThingType::Glyph(GlyphType::Flag) => 1,
				};
				if statement.modifier.len() > expected_modifiers {
					return Err(LevelParsingErrorCode::ExtraneousModifier(
						expected_modifiers,
						statement.modifier.len(),
					));
				}
				let color = if let Some(color_name) = statement.modifier.get(1).copied() {
					if let Ok(color) = parse_logical_color(&color_name.to_ascii_lowercase()) {
						color.map(|color| {
							// Override numeric color by palette if present
							if !color.is_pictogram {
								override_palette
									.get(color.color_index)
									.copied()
									.unwrap_or(color)
							} else {
								color
							}
						})
					} else {
						return Err(LevelParsingErrorCode::UnknownColorName(
							color_name.to_owned(),
						));
					}
				} else {
					// If no color is specified, use the default for the given object type
					match object_kind {
						ThingType::Object(ObjectType::Box) => *default_box_color,
						ThingType::Glyph(GlyphType::Button) => *default_button_color,
						_ => None,
					}
				};
				let to_insert = match object_kind {
					ThingType::Object(ObjectType::Box) => ThingData::Object(ObjectData::Box(color)),
					ThingType::Object(ObjectType::Player) => ThingData::Object(ObjectData::Player),
					ThingType::Glyph(GlyphType::Button) => {
						ThingData::Glyph(GlyphData::Button(color.map(|color| (color, default()))))
					}
					ThingType::Glyph(GlyphType::Flag) => ThingData::Glyph(GlyphData::Flag),
				};
				for vertex_name in statement.values {
					let Some(vertex) = vertex_names.get(vertex_name).copied() else {
						return Err(LevelParsingErrorCode::UnknownVertexName(
							vertex_name.to_owned(),
						));
					};
					match to_insert {
						ThingData::Object(object) => builder.add_object(vertex, object)?,
						ThingData::Glyph(glyph) => builder.add_glyph(vertex, glyph)?,
					}
				}
			}
			"PLACE" => {
				if !statement.modifier.is_empty() {
					return Err(LevelParsingErrorCode::ExtraneousModifier(
						0,
						statement.modifier.len(),
					));
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
				if !statement.modifier.is_empty() {
					return Err(LevelParsingErrorCode::ExtraneousModifier(
						0,
						statement.modifier.len(),
					));
				}
				match statement.values.len().cmp(&2) {
					std::cmp::Ordering::Less => {
						return Err(LevelParsingErrorCode::NotEnoughArguments(
							2,
							statement.values.len(),
						))
					}
					std::cmp::Ordering::Greater => {
						return Err(LevelParsingErrorCode::ExtraneousArguments(
							2,
							statement.values.len(),
						))
					}
					std::cmp::Ordering::Equal => {}
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
			"PALETTE" => {
				if statement.modifier.len() > 1 {
					return Err(LevelParsingErrorCode::ExtraneousModifier(
						1,
						statement.modifier.len(),
					));
				}
				let colors = statement
					.values
					.into_iter()
					.map(|color_name| {
						parse_logical_color(&color_name.to_ascii_lowercase())
							.map(|color| {
								color.expect("Lexer ensures that no empty strings are passed")
							})
							.map_err(|_| {
								LevelParsingErrorCode::UnknownColorName(color_name.to_owned())
							})
					})
					.collect::<Result<Vec<_>, _>>()?;
				match statement.modifier.first().copied() {
					None => {}
					Some("DEFAULT_BOX") => *default_box_color = colors.first().copied(),
					Some("DEFAULT_BUTTON") => *default_button_color = colors.first().copied(),
					Some("DEFAULT") => {
						let new_default = colors.first().copied();
						*default_box_color = new_default;
						*default_button_color = new_default;
					}
					Some(other) => {
						return Err(LevelParsingErrorCode::InvalidModifier(other.to_owned()))
					}
				}
				*override_palette = colors;
			}
			"COLORLABEL" => match statement.modifier.first().copied() {
				Some("VERTEX" | "") | None => {
					if statement.modifier.len() > 3 {
						return Err(LevelParsingErrorCode::ExtraneousModifier(
							3,
							statement.modifier.len(),
						));
					}
					let position = match statement.modifier.get(1).copied() {
						Some("inside" | "") | None => ButtonColorLabelPosition::Inside,
						Some("leftbtn") => ButtonColorLabelPosition::LeftButton,
						Some("rightbtn") => ButtonColorLabelPosition::RightButton,
						Some("above") => ButtonColorLabelPosition::AnglePlaced(0.0),
						Some("left") => ButtonColorLabelPosition::AnglePlaced(PI * 1.5),
						Some("below") => ButtonColorLabelPosition::AnglePlaced(PI),
						Some("right") => ButtonColorLabelPosition::AnglePlaced(PI * 0.5),
						Some(other) => {
							if let Ok(angle) = other.parse::<f32>() {
								ButtonColorLabelPosition::AnglePlaced(angle * PI / 180.0)
							} else if let Some(Ok(angle)) =
								other.strip_prefix('r').map(str::parse::<f32>)
							{
								ButtonColorLabelPosition::AngleRotated(angle * PI / 180.0)
							} else {
								return Err(LevelParsingErrorCode::InvalidModifier(
									other.to_owned(),
								));
							}
						}
					};
					let has_arrow_tip = match statement.modifier.get(2).copied() {
						Some("square" | "") | None => false,
						Some("arrow") => true,
						Some(other) => {
							return Err(LevelParsingErrorCode::InvalidModifier(other.to_owned()))
						}
					};
					let appearence = ButtonColorLabelAppearence {
						position,
						has_arrow_tip,
					};
					for vertex_name in statement.values {
						let Some(vertex) = vertex_names.get(vertex_name).copied() else {
							return Err(LevelParsingErrorCode::UnknownVertexName(
								vertex_name.to_owned(),
							));
						};
						builder.set_color_label_appearence(vertex, appearence)?;
					}
				}
				Some("CYCLE") => {
					todo!()
				}
				Some(other) => {
					return Err(LevelParsingErrorCode::InvalidModifier(other.to_owned()))
				}
			},
			other => return Err(LevelParsingErrorCode::InvalidKeyword(other.to_owned())),
		},
	}
	Ok(())
}

fn parse_logical_color(key: &str) -> Result<Option<LogicalColor>, ()> {
	if key.is_empty() {
		Ok(None)
	} else if let Ok(color_index) = key.parse() {
		Ok(Some(LogicalColor::new(color_index)))
	} else if let Some(Ok(color_index)) = key.strip_prefix('p').map(str::parse) {
		Ok(Some(LogicalColor::pictogram(color_index)))
	} else if let Ok(color_index) = pictogram_name_to_id(key) {
		Ok(Some(LogicalColor::pictogram(color_index)))
	} else {
		Err(())
	}
}

fn pictogram_name_to_id(name: &str) -> Result<usize, ()> {
	match name {
		"desc" => Ok(0),                    // -,
		"asc" => Ok(1),                     //  |
		"x" | "cross" => Ok(2),             //  |
		"minus" | "hbar" => Ok(3),          //  |
		"beam" | "vbar" => Ok(4),           //  |
		"plus" => Ok(5),                    //  |  Lateral and diagonal beams
		"desc_neg" => Ok(6),                //  |
		"asc_neg" => Ok(7),                 //  |
		"x_neg" | "cross_neg" => Ok(8),     //  |
		"minus_neg" | "hbar_neg" => Ok(9),  //  |
		"beam_neg" | "vbar_neg" => Ok(10),  //  |
		"plus_neg" => Ok(11),               // -`
		"up" => Ok(12),                     // -,
		"down" => Ok(13),                   //  |
		"left" => Ok(14),                   //  |
		"right" => Ok(15),                  //  | Triangles that span full length of the box
		"up_neg" => Ok(16),                 //  |
		"down_neg" => Ok(17),               //  |
		"left_neg" => Ok(18),               //  |
		"right_neg" => Ok(19),              // -`
		"top" => Ok(20),                    // -,
		"bottom" | "bot" => Ok(21),         //  |
		"start" => Ok(22),                  //  |
		"end" => Ok(23),                    //  | Triangles that reach to the center of the box
		"top_neg" => Ok(24),                //  |
		"bottom_neg" | "bot_neg" => Ok(25), //  |
		"start_neg" => Ok(26),              //  |
		"end_neg" => Ok(27),                // -`
		"tb" => Ok(28),                     // -, Two opposing center-boud triangles
		"lr" | "rl" => Ok(29),              // -`
		"lt" => Ok(30),                     // -,
		"rt" => Ok(31),                     //  | Half of the box filled, diagonally (as a right triangle)
		"lb" => Ok(32),                     //  |
		"rb" => Ok(33),                     // -`
		"wide_diamond" => Ok(34),           // -, Wide diamond (square, not rhombus)
		"corners" => Ok(35),                // -`
		"star" => Ok(36),                   // Star pictogram
		"checker_asc" => Ok(37),            // -, Two opposite quadrants are filled
		"checker_desc" => Ok(38),           // -`
		"heart" => Ok(39),                  // -,
		"spade" => Ok(40),                  //  | Card suites
		"club" => Ok(41),                   //  |
		"diamond" => Ok(42),                // -`
		"one" => Ok(43),                    // -,
		"two" => Ok(44),                    //  |
		"three" => Ok(45),                  //  | Dice dots
		"four" => Ok(46),                   //  |
		"five" => Ok(47),                   //  |
		"six" => Ok(48),                    // -`
		"fill" => Ok(49),                   // Filled box
		_ => Err(()),
	}
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
	ExtraneousModifier(usize, usize),
	UnknownColorName(String),
}

#[derive(Debug, PartialEq)]
pub struct LevelParsingError {
	pub code: LevelParsingErrorCode,
	pub line_number: usize,
}

impl LevelParsingErrorCode {
	pub fn at_line(self, line_number: usize) -> LevelParsingError {
		LevelParsingError {
			code: self,
			line_number,
		}
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
			Self::ExtraneousModifier(expected, got) => write!(
				f,
				"Statement found {got} modifiers, expected at most {expected}."
			)?,
			Self::UnknownColorName(name) => write!(f, "{name} is not a valid color name.")?,
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
		parse, ButtonColorLabelAppearence, ButtonColorLabelPosition, LevelBuilderError,
		LevelParsingErrorCode, LexErrorCode, LogicalColor, OverlappedLinkedCyclesError,
	};
	use std::f32::consts::PI;

	macro_rules! assert_err_eq {
		($left:expr, $right:expr) => {
			let left = $left;
			let right = $right;
			let err = left
				.expect_err("Negative test sample parrsed without error!")
				.code;
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

# Vertex command cannot take modifiers
VERTEX[InvalidModifier] a

# Cycle command cannot have more than one modifier
VERTEX a
CYCLE[MANUAL:ExtraModifier] k a

# Link command cannot have more than one modifier
VERTEX a b
CYCLE k a
CYCLE l b
LINK[STRAIGHT:ExtraModifier] k l

# Player objects cannot have color
VERTEX a
OBJECT[PLAYER:0] a

# Box objects can have color, but not anything past that
VERTEX a
OBJECT[BOX:0:ExtraModifier] a

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
			LevelParsingErrorCode::ExtraneousModifier(0, 1),
			LevelParsingErrorCode::ExtraneousModifier(0, 1),
			LevelParsingErrorCode::ExtraneousModifier(0, 1),
			LevelParsingErrorCode::ExtraneousModifier(1, 2),
			LevelParsingErrorCode::ExtraneousModifier(1, 2),
			LevelParsingErrorCode::ExtraneousModifier(1, 2),
			LevelParsingErrorCode::ExtraneousModifier(2, 3),
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

	#[test]
	fn logical_colors_test() {
		let data = r"
VERTEX a b c d e f g i j k l m n

# Default, with no color
OBJECT[BOX] a

# Numeric colors are specified with numbers
OBJECT[BOX:42] b

# Pictogram colors are specified by name
OBJECT[BOX:star] c

# Pictogram colors may also be specified by their id
OBJECT[BOX:p16] d

# Palette command defines substitutions for numeric color declarations
PALETTE p3 p4 2048
OBJECT[BOX:0] e

# Palettes can substitute numeric colors as well
OBJECT[BOX:2] f

# Anything out of range will remain a number
OBJECT[BOX:3] g

# First color of a palette may be set as the default color
# for when a color specifier is omited
PALETTE[DEFAULT] p12 2
OBJECT[BOX] i

# Colorless object can be forced by explicit empty color specifier
OBJECT[BOX:] j

# Palette commands ignore any pre-existing palettes.
# The '2', as declared above, is actually the numeric color 2
OBJECT[BOX:1] k

# Palette is cleared by omiting the arguments
PALETTE
OBJECT[BOX:1] l

# This does not affect the default color unless the modifier is used again
OBJECT[BOX] m

# Empty palette command with modifier clears default color as well
PALETTE[DEFAULT]
OBJECT[BOX] n
";

		let expected_colors = [
			None,
			Some(LogicalColor::new(42)),
			Some(LogicalColor::pictogram(36)),
			Some(LogicalColor::pictogram(16)),
			Some(LogicalColor::pictogram(3)),
			Some(LogicalColor::new(2048)),
			Some(LogicalColor::new(3)),
			Some(LogicalColor::pictogram(12)),
			None,
			Some(LogicalColor::new(2)),
			Some(LogicalColor::new(1)),
			Some(LogicalColor::pictogram(12)),
			None,
		];

		let level = parse(data).expect("Test sample did not parse correctly!");
		for (vertex, expected_color) in level.vertices.iter().zip(expected_colors) {
			let Some(super::ObjectData::Box(color)) = vertex.object else {
				panic!("Vertex does not contain a box.");
			};
			assert_eq!(color, expected_color);
		}
	}

	#[test]
	fn color_labels_test() {
		let data = r"
PALETTE[DEFAULT] 0
VERTEX a b c d e f g h i j k l
OBJECT[BUTTON] a b c d e f g h i j k l

# Color labels can be repositioned to a set of
# predefined positions
COLORLABEL[:left] a
COLORLABEL[:right] b
COLORLABEL[:above] c
COLORLABEL[:below] d
COLORLABEL[:leftbtn] e
COLORLABEL[:rightbtn] f

# The default position is inside the button.
# This can also be forced manually
COLORLABEL[:inside] g
# (note: vertex h remains at default position)

# Position can be specified manually as rotation
# (as clock angle in degrees)
COLORLABEL[:30] i

# Adding the 'r' prefix means the label itself will be
# rotated, not just positioned
COLORLABEL[:r60] j

# Finally, any option can be combined with a shape specifier
# to choose between square label (default) or an arrow-tipped one
COLORLABEL[:above:square] k
COLORLABEL[::arrow] l
";
		let expected_appearences = [
			ButtonColorLabelAppearence {
				position: ButtonColorLabelPosition::AnglePlaced(PI / 2.0),
				has_arrow_tip: false,
			},
			ButtonColorLabelAppearence {
				position: ButtonColorLabelPosition::AnglePlaced(PI * 1.5),
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
				position: ButtonColorLabelPosition::LeftButton,
				has_arrow_tip: false,
			},
			ButtonColorLabelAppearence {
				position: ButtonColorLabelPosition::RightButton,
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
		];

		let level = parse(data).expect("Test sample did not parse correctly!");
		for (vertex, expected_appearence) in level.vertices.iter().zip(expected_appearences) {
			let Some(super::GlyphData::Button(Some((_, appearence)))) = vertex.glyph else {
				panic!("Vertex does not contain a colored button.");
			};
			assert_eq!(appearence, expected_appearence);
		}
	}
}
