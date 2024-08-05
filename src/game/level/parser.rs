use super::*;

use bevy::utils::hashbrown::HashMap;
use layout::{CyclePlacement, DeclaredCyclePlacement, DeclaredPlacement, DeclaredVertexPlacement};
use regex::Regex;

#[derive(Clone, Debug)]
pub struct LevelFile {
	pub metadata: HashMap<String, String>,
	pub data: LevelData,
	pub layout: Vec<layout::DeclaredPlacement>,
}

struct RawStatement<'a> {
	verb: &'a str,
	modifier: Option<&'a str>,
	values: Vec<&'a str>,
}

enum Statement<'a> {
	Vertex(Vec<&'a str>),
	Cycle(CycleTurnability, Vec<&'a str>),
	Link(LinkedCycleDirection, Vec<&'a str>),
	Object(ObjectKind, Option<LogicalColor>, Vec<&'a str>),
	Place(Vec<&'a str>),
	PlaceVertex(Vec<&'a str>),
}

enum ObjectKind {
	Object(ObjectType),
	Glyph(GlyphType),
}

pub fn parse(level_file: &str) -> Result<LevelFile, LevelParsingError> {
	let lines = level_file.split('\n').map(&str::trim).enumerate();
	let lines = lines.filter(|(_line_index, line)| !(line.starts_with('#') || line.is_empty()));

	let metadata_regex = Regex::new(r"^(?<KEY>[a-zA-Z0-9_]+)=(?<VALUE>.+)$")
		.expect("I expected to be able to write a valid regex.");
	let statement_regex = Regex::new(r"^(?<VERB>\w+)(\[(?<MODIFIER>[\w\:]+)\])?(?<VALUES>.+)?$")
		.expect("I expected to be able to write a valid regex.");
	let mut raw_statements: Vec<(usize, RawStatement)> = Vec::new();

	let mut metadata = HashMap::new();

	for (line_id, line) in lines {
		if !line.is_ascii() {
			// TODO: Line number
			return Err(LevelParsingError::NonAsciiLine);
		}
		if let Some(captures) = metadata_regex.captures(line) {
			let key = captures
				.name("KEY")
				.expect("KEY clause should always be present")
				.as_str()
				.to_ascii_lowercase();
			let value = captures
				.name("VALUE")
				.expect("VALUE clause should always be present")
				.as_str();
			metadata.insert(key, value.to_string()); // TODO: check for duplicate keywords
			continue;
		}
		let Some(captures) = statement_regex.captures(line) else {
			return Err(LevelParsingError::MalformedStatement(line_id));
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
						Some("STILL") => CycleTurnability::Never,
						Some(m) => return Err(LevelParsingError::InvalidModifier(m.to_string())),
					};
					Statement::Cycle(turnability, raw_statement.values)
				}
				"LINK" => {
					let flip = match raw_statement.modifier {
						None => LinkedCycleDirection::Coincident,
						Some("STRAIGHT") => LinkedCycleDirection::Coincident,
						Some("CROSSED") => LinkedCycleDirection::Inverse,
						Some(m) => return Err(LevelParsingError::InvalidModifier(m.to_string())),
					};
					Statement::Link(flip, raw_statement.values)
				}
				"OBJECT" => {
					let Some(modifier) = raw_statement.modifier else {
						return Err(LevelParsingError::MalformedStatement(line_id));
					};
					let (object_kind, color) = modifier.split_once(':').unwrap_or((modifier, ""));
					let kind = match object_kind {
						"BOX" => ObjectKind::Object(ObjectType::Box),
						"PLAYER" => ObjectKind::Object(ObjectType::Player),
						"BUTTON" => ObjectKind::Glyph(GlyphType::Button),
						"FLAG" => ObjectKind::Glyph(GlyphType::Flag),
						_ => return Err(LevelParsingError::InvalidModifier(modifier.to_string())),
					};
					let color_id = if color.is_empty() {
						None
					} else if let Ok(color_id) = color.parse() {
						Some(LogicalColor(color_id))
					} else {
						return Err(LevelParsingError::InvalidModifier(modifier.to_string()));
					};
					Statement::Object(kind, color_id, raw_statement.values)
				}
				"PLACE" => Statement::Place(raw_statement.values),
				"PLACE_VERT" => Statement::PlaceVertex(raw_statement.values),
				k => return Err(LevelParsingError::InvalidKeyword(k.to_string())), // TODO: line number
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
				return Err(LevelParsingError::MalformedStatement(*line_id));
			}
			let cycle_name = *names.first().unwrap();
			if cycle_names.contains_key(cycle_name) {
				return Err(LevelParsingError::RedefinedCycle(cycle_name.to_string()));
			}
			let mut cycle = CycleData {
				vertex_indices: Vec::new(),
				cycle_turnability: *turnability,
			};
			for vertex_name in names.iter().skip(1) {
				if let Some(vertex_id) = vertex_names.get(*vertex_name) {
					cycle.vertex_indices.push(*vertex_id);
				} else {
					return Err(LevelParsingError::UnknownVertexName(
						vertex_name.to_string(),
					));
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
				return Err(LevelParsingError::MalformedStatement(*line_id));
			}
			let mut last_cycle = None;
			for cycle_name in linked_cycle_names {
				if let Some(cycle_id) = cycle_names.get(*cycle_name) {
					if let Some(previous_cycle_id) = last_cycle {
						linkages.push(LinkageData {
							cycle_a_index: previous_cycle_id,
							cycle_b_index: *cycle_id,
							direction: *flip,
						});
					}
					last_cycle = Some(*cycle_id);
				} else {
					return Err(LevelParsingError::UnknownCycleName(cycle_name.to_string()));
				}
			}
		}
	}

	for (_, statement) in statements.iter() {
		if let Statement::Object(kind, color, placed_vertex_names) = statement {
			if placed_vertex_names.is_empty() {
				// TODO: warn
			}
			for name in placed_vertex_names {
				if let Some(vertex_id) = vertex_names.get(*name) {
					let vertex = vertices.get_mut(*vertex_id).unwrap();
					match kind {
						ObjectKind::Object(object) => {
							if vertex.object.is_some() {
								return Err(LevelParsingError::ObjectCollision(name.to_string()));
							}
							vertex.object = Some(ObjectData {
								object_type: *object,
								color: *color,
							});
						}
						ObjectKind::Glyph(glyph) => {
							if vertex.glyph.is_some() {
								return Err(LevelParsingError::ObjectCollision(name.to_string()));
							}
							vertex.glyph = Some(GlyphData {
								glyph_type: *glyph,
								color: *color,
							});
						}
					}
				} else {
					return Err(LevelParsingError::UnknownVertexName(name.to_string()));
				}
			}
		}
	}

	let mut cycle_layout: Vec<Option<(CyclePlacement, Vec<usize>)>> =
		(0..cycles.len()).map(|_| None).collect();

	for (line_id, statement) in statements.iter() {
		if let Statement::Place(data) = statement {
			if data.len() < 4 {
				return Err(LevelParsingError::MalformedStatement(*line_id));
			}
			let cycle_name = data[0];
			let x_str = data[1].replace(',', ".");
			let y_str = data[2].replace(',', ".");
			let r_str = data[3].replace(',', ".");
			let Some(cycle_id) = cycle_names.get(cycle_name) else {
				return Err(LevelParsingError::UnknownCycleName(cycle_name.to_string()));
			};
			let Ok(x) = x_str.parse::<f32>() else {
				return Err(LevelParsingError::MalformedStatement(*line_id));
			};
			let Ok(y) = y_str.parse::<f32>() else {
				return Err(LevelParsingError::MalformedStatement(*line_id));
			};
			let Ok(r) = r_str.parse::<f32>() else {
				return Err(LevelParsingError::MalformedStatement(*line_id));
			};
			let hints = data[4..]
				.iter()
				.map(|name| {
					vertex_names
						.get(*name)
						.copied()
						.ok_or(LevelParsingError::UnknownVertexName(name.to_string()))
				})
				.collect::<Result<_, _>>()?;
			cycle_layout[*cycle_id] = Some((
				CyclePlacement {
					position: Vec2::new(x, y),
					radius: r,
				},
				hints,
			));
		}
	}

	if !cycle_layout.iter().all(Option::is_some) {
		return Err(LevelParsingError::MissingCycleLayout);
	}

	let mut layout: Vec<DeclaredPlacement> = cycle_layout
		.into_iter()
		.enumerate()
		.map(|(i, x)| {
			let (placement, hints) = x.unwrap();
			DeclaredPlacement::Cycle(DeclaredCyclePlacement {
				cycle_index: i,
				position: placement.position,
				radius: placement.radius,
				double_intersection_hints: hints,
			})
		})
		.collect();

	for (line_id, statement) in statements.iter() {
		if let Statement::PlaceVertex(data) = statement {
			if data.len() < 2 {
				return Err(LevelParsingError::MalformedStatement(*line_id));
			}
			let vertex_name = data[0];
			let angle_str = data[1].replace(',', ".");
			if let Some(vertex_id) = vertex_names.get(vertex_name) {
				let Ok(angle) = angle_str.parse::<f32>() else {
					return Err(LevelParsingError::MalformedStatement(*line_id));
				};
				layout.push(DeclaredPlacement::Vertex(DeclaredVertexPlacement {
					vertex_index: *vertex_id,
					relative_angle: angle,
				}))
			} else {
				return Err(LevelParsingError::UnknownCycleName(vertex_name.to_string()));
			}
		}
	}

	Ok(LevelFile {
		metadata,
		data: LevelData {
			vertices,
			cycles,
			linkages,
		},
		layout,
	})
}

#[test]
fn basic_test() {
	let data = r"
TEST6=?!#_AAA 648
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
	NonAsciiLine,
	MalformedStatement(usize),
	MissingCycleLayout,
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
			LevelParsingError::NonAsciiLine => write!(f, "Non-ASCII characters in input")?,
			LevelParsingError::MalformedStatement(line_id) => {
				write!(f, "Line {} contains a malformed statement.", line_id + 1)?
			}
			LevelParsingError::MissingCycleLayout => {
				write!(f, "Some cycles don't have a position specified")?
			}
		}
		Ok(())
	}
}
