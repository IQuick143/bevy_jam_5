use super::prelude::*;

pub mod asset;
mod builder;
mod lex;
pub mod list;
pub mod list_asset;
pub mod parser;

/// Complete description of a level
#[derive(Debug, Clone, Reflect, Asset)]
pub struct LevelData {
	/// Name of the level
	pub name: String,
	/// Hint or comment that relates to the level, if any
	pub hint: Option<String>,
	/// Data for all vertices in the level
	pub vertices: Vec<VertexData>,
	/// Data for all cycles in the level
	pub cycles: Vec<CycleData>,
	/// Data for all cycle links that have been explicitly declared in the level file.
	/// Will be used for rendering the links
	pub declared_links: Vec<DeclaredLinkData>,
}

/// Description of a single vertex
#[derive(Debug, Clone, Copy, Reflect)]
pub struct VertexData {
	/// Position of the vertex on the screen
	pub position: Vec2,
	/// Object that lies on the vertex, if any
	pub object: Option<ObjectData>,
	/// Glyph that lies on the vertex, if any
	pub glyph: Option<GlyphData>,
}

/// Description of a single cycle
#[derive(Debug, Clone, Reflect)]
pub struct CycleData {
	///  Placement of the cycle
	pub placement: CyclePlacement,
	/// Indices into [`LevelData::vertices`]
	/// that identify the vertices that lie on the cycle, in clockwise order
	pub vertex_indices: Vec<usize>,
	/// When the cycle can be turned
	pub turnability: CycleTurnability,
	/// All cycles that have to turn when this cycle is turned, including itself
	pub link_closure: Vec<(usize, LinkedCycleDirection)>,
}

/// Description of a declared (and visualized) cycle link
#[derive(Debug, Clone, Copy, Reflect)]
pub struct DeclaredLinkData {
	/// Cycle from which the link goes
	pub source_cycle: usize,
	/// Cycle to which the link goes
	pub dest_cycle: usize,
	/// Relative turning direction between the linked cycles
	pub direction: LinkedCycleDirection,
}

/// Computed placement of a cycle
#[derive(Clone, Copy, PartialEq, Debug, Reflect)]
pub struct CyclePlacement {
	/// Position of the center point of the cycle
	pub position: Vec2,
	/// Radius of the cycle
	pub radius: f32,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Reflect)]
pub enum ObjectType {
	Box,
	Player,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Reflect)]
pub enum GlyphType {
	Button,
	Flag,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, Reflect)]
pub struct ObjectData {
	pub object_type: ObjectType,
	pub color: Option<LogicalColor>,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, Reflect)]
pub struct GlyphData {
	pub glyph_type: GlyphType,
	pub color: Option<LogicalColor>,
}

/// Type describing any gameplay object
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Reflect)]
pub enum ThingType {
	Object(ObjectType),
	Glyph(GlyphType),
}

impl std::fmt::Display for CyclePlacement {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(
			f,
			"[x={} y={} r={}]",
			self.position.x, self.position.y, self.radius
		)
	}
}
