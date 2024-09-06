use super::{builder::*, lex::*, *};
use bevy::utils::hashbrown::HashMap;
use itertools::Itertools as _;

pub fn parse(level_file: &str) -> Result<LevelData, LevelParsingError> {
	let mut builder = LevelBuilder::new();
	let mut vertex_names = HashMap::new();
	let mut cycle_names = HashMap::new();

	for line in lex::parse(level_file) {
		let (line_id, statement) = line?;
		match statement {
			RawStatement::Assignment(statement) => {
				match statement.key.to_ascii_lowercase().as_str() {
					"name" => builder.set_level_name(statement.value.to_owned())?,
					"hint" => builder.set_level_hint(statement.value.to_owned())?,
					other => return Err(LevelParsingError::InvalidMetaVariable(other.to_owned())),
				}
			}
			RawStatement::Action(statement) => {
				match statement.verb {
					"VERTEX" => {
						for name in statement.values {
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
								return Err(LevelParsingError::InvalidModifier(m.to_string()))
							}
						};
						let mut values = statement.values.into_iter();
						let name = values
							.next()
							.ok_or(LevelParsingError::NotEnoughArguments(1, 0))?;
						let vertices = values
							.map(|name| {
								vertex_names.get(name).copied().ok_or_else(|| {
									LevelParsingError::UnknownVertexName(name.to_owned())
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
								return Err(LevelParsingError::InvalidModifier(m.to_string()))
							}
						};
						let cycles = statement
							.values
							.into_iter()
							.map(|name| {
								cycle_names.get(name).copied().ok_or_else(|| {
									LevelParsingError::UnknownVertexName(name.to_owned())
								})
							})
							.collect::<Result<Vec<_>, _>>()?;
						if cycles.len() < 2 {
							return Err(LevelParsingError::NotEnoughArguments(2, cycles.len()));
						}
						for (source, dest) in cycles.into_iter().tuple_windows() {
							builder.link_cycles(source, dest, direction)?;
						}
					}
					"OBJECT" => {
						let Some(modifier) = statement.modifier else {
							return Err(LevelParsingError::MalformedStatement(line_id));
						};
						let (object_kind, color) =
							modifier.split_once(':').unwrap_or((modifier, ""));
						let _color_id = if color.is_empty() {
							None
						} else if let Ok(color_id) = color.parse() {
							Some(LogicalColor(color_id))
						} else {
							return Err(LevelParsingError::InvalidModifier(modifier.to_string()));
						};
						let thing_type = match object_kind {
							"BOX" => ThingType::Object(ObjectType::Box),
							"PLAYER" => ThingType::Object(ObjectType::Player),
							"BUTTON" => ThingType::Glyph(GlyphType::Button),
							"FLAG" => ThingType::Glyph(GlyphType::Flag),
							_ => {
								return Err(LevelParsingError::InvalidModifier(
									modifier.to_string(),
								))
							}
						};
						for vertex_name in statement.values {
							let Some(vertex) = vertex_names.get(vertex_name).copied() else {
								return Err(LevelParsingError::UnknownVertexName(
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
						if statement.values.len() < 4 {
							return Err(LevelParsingError::NotEnoughArguments(
								4,
								statement.values.len(),
							));
						}
						let cycle_name = statement.values[0];
						let x_str = statement.values[1].replace(',', ".");
						let y_str = statement.values[2].replace(',', ".");
						let r_str = statement.values[3].replace(',', ".");
						let Some(cycle_id) = cycle_names.get(cycle_name).copied() else {
							return Err(LevelParsingError::UnknownCycleName(
								cycle_name.to_string(),
							));
						};
						let Ok(x) = x_str.parse::<f32>() else {
							return Err(LevelParsingError::MalformedStatement(line_id));
						};
						let Ok(y) = y_str.parse::<f32>() else {
							return Err(LevelParsingError::MalformedStatement(line_id));
						};
						let Ok(r) = r_str.parse::<f32>() else {
							return Err(LevelParsingError::MalformedStatement(line_id));
						};
						let hints = statement.values[4..]
							.iter()
							.map(|name| {
								vertex_names
									.get(*name)
									.copied()
									.ok_or(LevelParsingError::UnknownVertexName(name.to_string()))
							})
							.collect::<Result<Vec<_>, _>>()?;
						builder.place_cycle(cycle_id, Vec2::new(x, y), r, &hints)?;
					}
					"PLACE_VERT" => {
						if statement.values.len() < 2 {
							return Err(LevelParsingError::NotEnoughArguments(
								2,
								statement.values.len(),
							));
						}
						let vertex_name = statement.values[0];
						let angle_str = statement.values[1].replace(',', ".");
						let Some(vertex_id) = vertex_names.get(vertex_name).copied() else {
							return Err(LevelParsingError::UnknownCycleName(
								vertex_name.to_string(),
							));
						};
						let Ok(angle) = angle_str.parse::<f32>() else {
							return Err(LevelParsingError::MalformedStatement(line_id));
						};
						builder.place_vertex(vertex_id, angle)?;
					}
					other => return Err(LevelParsingError::InvalidKeyword(other.to_owned())),
				}
			}
		}
	}

	Ok(builder.build()?)
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
	let parsed = parse(data);
	println!("{:?}", parsed);
	assert!(parsed.is_ok());
	println!("{:?}", parsed.unwrap());
}

#[allow(dead_code)]
#[derive(Debug)]
pub enum LevelParsingError {
	InvalidKeyword(String),
	InvalidModifier(String),
	UnknownVertexName(String),
	UnknownCycleName(String),
	RedefinedCycle(String),
	ObjectCollision(String),
	LexError(LexError),
	MalformedStatement(usize),
	MissingCycleLayout,
	InvalidMetaVariable(String),
	BuilderError(LevelBuilderError),
	NotEnoughArguments(usize, usize),
}

impl From<LexError> for LevelParsingError {
	fn from(value: LexError) -> Self {
		Self::LexError(value)
	}
}

impl From<LevelBuilderError> for LevelParsingError {
	fn from(value: LevelBuilderError) -> Self {
		Self::BuilderError(value)
	}
}

impl std::error::Error for LevelParsingError {}

impl std::fmt::Display for LevelParsingError {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			LevelParsingError::InvalidKeyword(k) => write!(f, "The keyword {} is invalid.", k)?,
			LevelParsingError::InvalidModifier(m) => write!(f, "The modifier [{}] is invalid.", m)?,
			LevelParsingError::UnknownVertexName(name) => write!(
				f,
				"A vertex with name {} was referenced, but not defined.",
				name
			)?,
			LevelParsingError::UnknownCycleName(name) => write!(
				f,
				"A cycle with name {} was referenced, but not defined.",
				name
			)?,
			LevelParsingError::RedefinedCycle(name) => {
				write!(f, "A cycle with name {} was defined multiple times.", name)?
			}
			LevelParsingError::ObjectCollision(vertex) => write!(
				f,
				"Too many objects were placed onto vertex with name {}",
				vertex
			)?,
			LevelParsingError::LexError(e) => e.fmt(f)?,
			LevelParsingError::MalformedStatement(line_id) => {
				write!(f, "Line {} contains a malformed statement.", line_id + 1)?
			}
			LevelParsingError::MissingCycleLayout => {
				write!(f, "Some cycles don't have a position specified")?
			}
			LevelParsingError::InvalidMetaVariable(name) => {
				write!(f, "{name} is not a valid meta variable name")?
			}
			LevelParsingError::BuilderError(e) => e.fmt(f)?,
			LevelParsingError::NotEnoughArguments(needed, got) => write!(
				f,
				"Statement found {got} arguments, needs at least {needed}."
			)?,
		}
		Ok(())
	}
}
