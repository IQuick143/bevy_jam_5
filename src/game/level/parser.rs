use super::*;

use bevy::utils::hashbrown::HashMap;
use regex::Regex;

struct RawStatement<'a> {
	verb: &'a str,
	modifier: Option<&'a str>,
	values: Vec<&'a str>,
}

enum Statement<'a> {
	Vertex(Vec<&'a str>),
	Cycle(CycleTurnability, Vec<&'a str>),
	Link(LinkedCycleDirection, Vec<&'a str>),
	Object(ObjectKind, Vec<&'a str>),
}

enum ObjectKind {
	Object(ObjectType),
	Glyph(GlyphType),
}

#[allow(dead_code)]
pub fn parse(level_file: &str) -> Result<LevelData, ParsingError> {
	let lines = level_file.split("\n").map(&str::trim).enumerate();
	let lines = lines.filter(|(_line_index, line)| !(line.starts_with("#") || line.is_empty()));

	let statement_regex = Regex::new(r"^(?<VERB>\w+)(\[(?<MODIFIER>\w+)\])?(?<VALUES>.+)?$")
		.expect("I expected to be able to write a valid regex.");
	let mut raw_statements: Vec<(usize, RawStatement)> = Vec::new();

	for (line_id, line) in lines {
		if !line.is_ascii() {
			// TODO: Line number
			return Err(ParsingError::NonAsciiLine);
		}
		let Some(captures) = statement_regex.captures(line) else {
			// TODO: Line number
			return Err(ParsingError::MalformedStatement(line_id));
		};
		raw_statements.push((
			line_id,
			RawStatement {
				verb: captures
					.name("VERB")
					.expect("VERB clause should always be present")
					.as_str(),
				modifier: captures.name("MODIFIER").map(|x| x.as_str()),
				values: match captures.name("VALUES") {
					None => Vec::new(),
					Some(m) => m
						.as_str()
						.split_ascii_whitespace()
						.filter(|x| !x.is_empty())
						.collect(),
				},
			},
		))
	}

	let mut statements = Vec::new();

	for (line_id, raw_statement) in raw_statements {
		statements.push((
			line_id,
			match raw_statement.verb {
				"VERTEX" => Statement::Vertex(raw_statement.values),
				"CYCLE" => {
					let turnability = match raw_statement.modifier {
						None => CycleTurnability::Always,
						Some("MANUAL") => CycleTurnability::WithPlayer,
						Some("ENGINE") => CycleTurnability::Always,
						Some(m) => return Err(ParsingError::InvalidModifier(m.to_string())),
					};
					Statement::Cycle(turnability, raw_statement.values)
				}
				"LINK" => {
					let flip = match raw_statement.modifier {
						None => LinkedCycleDirection::Coincident,
						Some("STRAIGHT") => LinkedCycleDirection::Coincident,
						Some("CROSSED") => LinkedCycleDirection::Inverse,
						Some(m) => return Err(ParsingError::InvalidModifier(m.to_string())),
					};
					Statement::Link(flip, raw_statement.values)
				}
				"OBJECT" => {
					let kind = match raw_statement.modifier {
						None => return Err(ParsingError::MalformedStatement(line_id)),
						Some("BOX") => ObjectKind::Object(ObjectType::Box),
						Some("PLAYER") => ObjectKind::Object(ObjectType::Player),
						Some("BUTTON") => ObjectKind::Glyph(GlyphType::Button),
						Some("FLAG") => ObjectKind::Glyph(GlyphType::Flag),
						Some(m) => return Err(ParsingError::InvalidModifier(m.to_string())),
					};
					Statement::Object(kind, raw_statement.values)
				}
				k => return Err(ParsingError::InvalidKeyword(k.to_string())), // TODO: line number
			},
		));
	}

	let mut vertices = Vec::new();
	let mut vertex_names = HashMap::new();

	for (_, statement) in statements.iter() {
		if let Statement::Vertex(names) = statement {
			if names.is_empty() {
				// TODO: warn
			}
			for name in names {
				if vertex_names.contains_key(*name) {
					// TODO: warn
				} else {
					let new_id = vertices.len();
					vertices.push(VertexData {
						object: None,
						glyph: None,
					});
					vertex_names.insert(name.to_string(), new_id);
				}
			}
		}
	}

	let mut cycles = Vec::new();
	let mut cycle_names = HashMap::new();

	for (line_id, statement) in statements.iter() {
		if let Statement::Cycle(turnability, names) = statement {
			if vertex_names.len() < 3 {
				// TODO: better error
				return Err(ParsingError::MalformedStatement(*line_id));
			}
			let cycle_name = *names.first().unwrap();
			if cycle_names.contains_key(cycle_name) {
				return Err(ParsingError::RedefinedCycle(cycle_name.to_string()));
			}
			let mut cycle = CycleData {
				vertex_indices: Vec::new(),
				cycle_turnability: *turnability,
			};
			for vertex_name in names.iter().skip(1) {
				if let Some(vertex_id) = vertex_names.get(*vertex_name) {
					cycle.vertex_indices.push(*vertex_id);
				} else {
					return Err(ParsingError::UnknownVertexName(vertex_name.to_string()));
				}
			}
			let new_id = cycles.len();
			cycles.push(cycle);
			cycle_names.insert(cycle_name.to_string(), new_id);
		}
	}

	let mut linkages: Vec<LinkageData> = Vec::new();

	for (line_id, statement) in statements.iter() {
		if let Statement::Link(flip, linked_cycle_names) = statement {
			if linked_cycle_names.len() < 2 {
				// TODO: better error
				return Err(ParsingError::MalformedStatement(*line_id));
			}
			let mut last_cycle = None;
			for cycle_name in linked_cycle_names {
				if let Some(cycle_id) = cycle_names.get(*cycle_name) {
					if last_cycle.is_some() {
						linkages.push(LinkageData {
							cycle_a_index: last_cycle.unwrap(),
							cycle_b_index: *cycle_id,
							direction: *flip,
						});
					}
					last_cycle = Some(*cycle_id);
				} else {
					return Err(ParsingError::UnknownCycleName(cycle_name.to_string()));
				}
			}
		}
	}

	for (_, statement) in statements.iter() {
		if let Statement::Object(kind, placed_vertex_names) = statement {
			if placed_vertex_names.is_empty() {
				// TODO: warn
			}
			for name in placed_vertex_names {
				if let Some(vertex_id) = vertex_names.get(*name) {
					let vertex = vertices.get_mut(*vertex_id).unwrap();
					match kind {
						ObjectKind::Object(object) => {
							if vertex.object.is_some() {
								return Err(ParsingError::ObjectCollision(name.to_string()));
							}
							vertex.object = Some(*object);
						}
						ObjectKind::Glyph(glyph) => {
							if vertex.glyph.is_some() {
								return Err(ParsingError::ObjectCollision(name.to_string()));
							}
							vertex.glyph = Some(*glyph);
						}
					}
				} else {
					return Err(ParsingError::UnknownVertexName(name.to_string()));
				}
			}
		}
	}

	Ok(LevelData {
		vertices,
		cycles,
		linkages,
	})
}

#[test]
fn basic_test() {
	let data = r"
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
";
	let parsed = parse(data);
	println!("{:?}", parsed);
	assert!(parsed.is_ok());
	println!("{:?}", parsed.unwrap());
}

#[allow(dead_code)]
#[derive(Debug)]
pub enum ParsingError {
	InvalidKeyword(String),
	InvalidModifier(String),
	UnknownVertexName(String),
	UnknownCycleName(String),
	RedefinedCycle(String),
	ObjectCollision(String),
	NonAsciiLine,
	MalformedStatement(usize),
}

impl std::error::Error for ParsingError {}

impl std::fmt::Display for ParsingError {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			ParsingError::InvalidKeyword(k) => write!(f, "The keyword {} is invalid.", k)?,
			ParsingError::InvalidModifier(m) => write!(f, "The modifier [{}] is invalid.", m)?,
			ParsingError::UnknownVertexName(name) => write!(f, "A vertex with name {} was referenced, but not defined.", name)?,
			ParsingError::UnknownCycleName(name) => write!(f, "A cycle with name {} was referenced, but not defined.", name)?,
			ParsingError::RedefinedCycle(name) => write!(f, "A cycle with name {} was defined multiple times.", name)?,
			ParsingError::ObjectCollision(vertex) => write!(f, "Too many objects were placed onto vertex with name {}", vertex)?,
			ParsingError::NonAsciiLine => write!(f, "Non-ASCII characters in input")?,
			ParsingError::MalformedStatement(line_id) => write!(f, "Line {} contains a malformed statement.", line_id+1)?,
		}
		Ok(())
	}
}
