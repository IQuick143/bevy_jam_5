//! General domain shared between the backend implementations

use super::super::*;
use crate::epilang::{interpreter::*, values::*};

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct VertexId(pub usize);

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct DetectorId(pub usize);

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct CycleId(pub usize);

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct LevelId(pub usize);

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct HubId(pub usize);

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum DomainValue {
	Vertex(VertexId),
	Detector(DetectorId),
	Wall,
	Cycle(CycleId),
	Object(ObjectData),
	Glyph(GlyphData),
	Color(LogicalColor),
	Level(LevelId),
	Hub(HubId),
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum DomainType {
	Vertex,
	Detector,
	Wall,
	Cycle,
	Object,
	Glyph,
	Color,
	Level,
	Hub,
}

impl std::fmt::Display for DomainType {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		let display_name = match self {
			Self::Vertex => "vertex",
			Self::Detector => "detector",
			Self::Wall => "wall",
			Self::Cycle => "cycle",
			Self::Object => "object",
			Self::Glyph => "glyph",
			Self::Color => "color",
			Self::Level => "level",
			Self::Hub => "hub",
		};
		f.write_str(display_name)
	}
}

impl DomainVariableValue for DomainValue {
	type Type = DomainType;

	fn get_type(&self) -> Self::Type {
		match self {
			Self::Vertex(_) => Self::Type::Vertex,
			Self::Detector(_) => Self::Type::Detector,
			Self::Wall => Self::Type::Wall,
			Self::Cycle(_) => Self::Type::Cycle,
			Self::Object(_) => Self::Type::Object,
			Self::Glyph(_) => Self::Type::Glyph,
			Self::Color(_) => Self::Type::Color,
			Self::Level(_) => Self::Type::Level,
			Self::Hub(_) => Self::Type::Hub,
		}
	}
}

macro_rules! impl_try_into_for_domain_value {
	( $( $variant:ident ( $type:ty ) $({ $($extra_pat:pat => $extra_expr:expr),* $(,)? })? ),* $(,)? ) => {
		$(
			impl TryFrom<&VariableValue<'_, DomainValue>> for $type {
				type Error = ArgumentError<DomainType>;
				fn try_from(value: &VariableValue<DomainValue>) -> Result<Self, Self::Error> {
					use VariableValue::*;
					match value {
						Domain(DomainValue::$variant(x)) => Ok(*x),
						$($($extra_pat => $extra_expr,)*)?
						_ => Err(value.get_type().into_argument_error()),
					}
				}
			}

			impl TryFrom<VariableValue<'_, DomainValue>> for $type {
				type Error = ArgumentError<DomainType>;
				fn try_from(value: VariableValue<DomainValue>) -> Result<Self, Self::Error> {
					Self::try_from(&value)
				}
			}

			impl From<$type> for VariableValue<'_, DomainValue> {
				fn from(value: $type) -> Self {
					Self::Domain(DomainValue::$variant(value))
				}
			}
		)*
	};
}

impl_try_into_for_domain_value! {
	Vertex(VertexId),
	Detector(DetectorId),
	Cycle(CycleId),
	Object(ObjectData),
	Glyph(GlyphData),
	Color(LogicalColor) {
		Int(i) => (*i)
			.try_into()
			.map(LogicalColor::new)
			.map_err(|_| ArgumentError::InvalidValue),
		String(s) => pictogram_color_name_to_id(s)
			.map(LogicalColor::pictogram)
			.map_err(|_| ArgumentError::InvalidValue),
	},
	Level(LevelId),
	Hub(HubId),
}

pub fn pictogram_color_name_to_id(name: &str) -> Result<usize, ()> {
	match name {
		"desc" | "\\" => Ok(0),             // -,
		"asc" | "/" => Ok(1),               //  |
		"x" | "X" | "cross" => Ok(2),       //  |
		"minus" | "hbar" | "-" => Ok(3),    //  |
		"beam" | "vbar" | "|" => Ok(4),     //  |
		"plus" | "+" => Ok(5),              //  |  Lateral and diagonal beams
		"desc_neg" => Ok(6),                //  |
		"asc_neg" => Ok(7),                 //  |
		"x_neg" | "cross_neg" => Ok(8),     //  |
		"minus_neg" | "hbar_neg" => Ok(9),  //  |
		"beam_neg" | "vbar_neg" => Ok(10),  //  |
		"plus_neg" => Ok(11),               // -`
		"up" | "^" => Ok(12),               // -,
		"down" | "v" => Ok(13),             //  |
		"left" | "<" => Ok(14),             //  |
		"right" | ">" => Ok(15),            //  | Triangles that span full length of the box
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
		"lr" | "rl" | "><" => Ok(29),       // -`
		"lt" => Ok(30),                     // -,
		"rt" => Ok(31),                     //  | Half of the box filled, diagonally (as a right triangle)
		"lb" => Ok(32),                     //  |
		"rb" => Ok(33),                     // -`
		"wide_diamond" => Ok(34),           // -, Wide diamond (square, not rhombus)
		"corners" => Ok(35),                // -`
		"star" | "*" => Ok(36),             // Star pictogram
		"checker_asc" => Ok(37),            // -, Two opposite quadrants are filled
		"checker_desc" => Ok(38),           // -`
		"heart" => Ok(39),                  // -,
		"spade" => Ok(40),                  //  | Card suites
		"club" => Ok(41),                   //  |
		"diamond" => Ok(42),                // -`
		"one" | "d1" => Ok(43),             // -,
		"two" | "d2" => Ok(44),             //  |
		"three" | "d3" => Ok(45),           //  | Dice dots
		"four" | "d4" => Ok(46),            //  |
		"five" | "d5" => Ok(47),            //  |
		"six" | "d6" => Ok(48),             // -`
		"fill" => Ok(49),                   // Filled box
		"lightbulb" => Ok(50),              // Lightbulb picrogram
		"power" => Ok(51),                  // Power symbol
		"sun" => Ok(52),                    // Sun pictogram
		_ => Err(()),
	}
}
