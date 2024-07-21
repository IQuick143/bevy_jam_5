use crate::game::prelude::*;

// pub mod parser;

pub enum ObjectType {
	Box, Player
}

pub enum GlyphType {
	Button, Flag
}

pub struct CycleData {
	vertex_indices: Vec<usize>,
	cycle_turnability: CycleTurnability
}

pub struct LinkageData {
	cycle_a_index: usize,
	cycle_b_index: usize,
	inversion: bool,
}

pub struct VertexData {
	object: Option<ObjectType>,
	glyph: Option<GlyphType>,
}

pub struct LevelData {
	vertices: Vec<VertexData>,
	cycles: Vec<CycleData>,
	linkages: Vec<LinkageData>,
}
