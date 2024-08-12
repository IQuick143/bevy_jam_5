use itertools::Itertools;

use super::prelude::*;

pub mod asset;
pub mod layout;
mod lex;
pub mod list;
pub mod list_asset;
pub mod parser;

pub use asset::LevelAsset;

/// How many colors we can paint objects in
pub const LOGICAL_COLORS: usize = 6;

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

#[derive(Debug, Clone, Reflect)]
pub struct CycleData {
	pub vertex_indices: Vec<usize>,
	pub cycle_turnability: CycleTurnability,
}

#[derive(Debug, Clone, Copy, Reflect)]
pub struct LinkageData {
	pub cycle_a_index: usize,
	pub cycle_b_index: usize,
	pub direction: LinkedCycleDirection,
}

#[derive(Debug, Clone, Copy, Default, Reflect)]
pub struct VertexData {
	pub object: Option<ObjectData>,
	pub glyph: Option<GlyphData>,
}

#[derive(Debug, Clone, Reflect)]
pub struct LevelData {
	pub vertices: Vec<VertexData>,
	pub cycles: Vec<CycleData>,
	pub linkages: Vec<LinkageData>,
}

#[allow(dead_code)]
#[derive(Debug, Clone, Copy)]
pub enum LevelDataValidationError {
	VertexIndexOutOfRange(usize),
	CycleIndexOutOfRange(usize),
	CycleLinkageConflict,
	TooFewVerticesInCycle(usize),
	RepeatingVertexInCycle(usize),
	TooManyVerticesInCycleIntersection(usize, usize),
	OverlappedLinkedCycles(usize, usize),
	ColorOutOfRange(usize),
}

/// A sanitized [`LevelData`] instance.
/// It is read-only to ensure that it truly remains valid
#[derive(Debug, Clone, Reflect)]
pub struct ValidLevelData {
	/// The contained level data. Guaranteed to be sanitized
	inner: LevelData,
	/// Computed equivalence closure of links between cycles.
	/// The sanitizer needs to compute this anyway, so it is
	/// included here for convenience
	links: Vec<Vec<Option<LinkedCycleDirection>>>,
}

impl ValidLevelData {
	/// Iterates over indices of cycles that are, directly or indirectly,
	/// linked to a given cycle. The original cycle is not included in the result.
	pub fn cycles_linked_to(
		&self,
		index: usize,
	) -> impl Iterator<Item = (usize, LinkedCycleDirection)> + '_ {
		self.links[index]
			.iter()
			.enumerate()
			.filter_map(|(i, x)| x.map(|x| (i, x)))
			.filter(move |(i, _)| *i != index)
	}
}

impl TryFrom<LevelData> for ValidLevelData {
	type Error = LevelDataValidationError;

	fn try_from(value: LevelData) -> Result<Self, Self::Error> {
		// Object color range
		for vertex in &value.vertices {
			if let Some(object) = vertex.object {
				if let Some(color) = object.color {
					if color.0 >= LOGICAL_COLORS {
						return Err(LevelDataValidationError::ColorOutOfRange(color.0));
					}
				}
			}
			if let Some(glyph) = vertex.glyph {
				if let Some(color) = glyph.color {
					if color.0 >= LOGICAL_COLORS {
						return Err(LevelDataValidationError::ColorOutOfRange(color.0));
					}
				}
			}
		}

		// Everything about integrity of individual cycles
		for (i, cycle) in value.cycles.iter().enumerate() {
			if cycle.vertex_indices.len() < 2 {
				return Err(LevelDataValidationError::TooFewVerticesInCycle(i));
			}
			if !cycle.vertex_indices.iter().all_unique() {
				return Err(LevelDataValidationError::RepeatingVertexInCycle(i));
			}
			if let Some(j) = cycle
				.vertex_indices
				.iter()
				.copied()
				.find(|&j| j >= value.vertices.len())
			{
				return Err(LevelDataValidationError::VertexIndexOutOfRange(j));
			}
		}

		// A very, *very* inefficient way of finding conflicts between cycle links
		// Only reasonably fast (and space-efficient) for 20 cycles or so
		let mut cycle_links = (0..value.cycles.len())
			.map(|i| {
				(0..value.cycles.len())
					.map(|j| {
						if i == j {
							Some(LinkedCycleDirection::Coincident)
						} else {
							None
						}
					})
					.collect::<Vec<_>>()
			})
			.collect::<Vec<_>>();
		fn set_link(
			links: &mut [Vec<Option<LinkedCycleDirection>>],
			i: usize,
			j: usize,
			dir: LinkedCycleDirection,
		) -> Result<(), LevelDataValidationError> {
			if links[i][j].is_some_and(|d| d != dir) {
				return Err(LevelDataValidationError::CycleLinkageConflict);
			}
			links[i][j] = Some(dir);
			links[j][i] = Some(dir);
			Ok(())
		}
		for link in &value.linkages {
			let i = link.cycle_a_index;
			let j = link.cycle_b_index;
			if i >= value.cycles.len() {
				return Err(LevelDataValidationError::CycleIndexOutOfRange(i));
			}
			if j >= value.cycles.len() {
				return Err(LevelDataValidationError::CycleIndexOutOfRange(j));
			}
			for k in 0..value.cycles.len() {
				if let Some(old_link) = cycle_links[i][k] {
					set_link(&mut cycle_links, j, k, link.direction * old_link)?;
				}
				if let Some(old_link) = cycle_links[j][k] {
					set_link(&mut cycle_links, i, k, link.direction * old_link)?;
				}
			}
		}

		// Everything about overlapping cycles
		let vertices_per_cycle = value
			.cycles
			.iter()
			.map(|c| std::collections::BTreeSet::from_iter(c.vertex_indices.iter().copied()))
			.collect::<Vec<_>>();
		for ((i, a), (j, b)) in vertices_per_cycle.iter().enumerate().tuple_combinations() {
			let common_vertices = a.intersection(b).count();
			if common_vertices > 2 {
				return Err(LevelDataValidationError::TooManyVerticesInCycleIntersection(i, j));
			}
			if common_vertices > 0 && cycle_links[i][j].is_some() {
				return Err(LevelDataValidationError::OverlappedLinkedCycles(i, j));
			}
		}

		Ok(Self {
			inner: value,
			links: cycle_links,
		})
	}
}

impl std::ops::Deref for ValidLevelData {
	type Target = LevelData;
	fn deref(&self) -> &Self::Target {
		&self.inner
	}
}

impl std::borrow::Borrow<LevelData> for ValidLevelData {
	fn borrow(&self) -> &LevelData {
		&self.inner
	}
}

impl std::fmt::Display for LevelDataValidationError {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::VertexIndexOutOfRange(i) => write!(
				f,
				"Vertex index {i} is used, but there are not that many vertices."
			),
			Self::CycleIndexOutOfRange(i) => write!(
				f,
				"Cycle index {i} is used, but there are not that many cycles."
			),
			Self::CycleLinkageConflict => write!(f, "Contradictory cycle links."),
			Self::TooFewVerticesInCycle(i) => {
				write!(f, "Cycle {i} contains less than two vertices.")
			}
			Self::RepeatingVertexInCycle(i) => {
				write!(f, "Cycle {i} contains the same vertex multiple times.")
			}
			Self::TooManyVerticesInCycleIntersection(i, j) => {
				write!(f, "Cycles {i} and {j} intersect in more than two vertices.")
			}
			Self::OverlappedLinkedCycles(i, j) => {
				write!(f, "Cycles {i} and {j} are overlapped and linked.")
			}
			Self::ColorOutOfRange(c) => {
				write!(
					f,
					"Color {c} is used, but there are only {LOGICAL_COLORS} colors."
				)
			}
		}
	}
}

impl std::error::Error for LevelDataValidationError {}

#[cfg(test)]
mod test {
	use super::*;
	use LinkedCycleDirection::*;

	macro_rules! level_data {
		($vertices:expr, [$([$($cycles:expr),* $(,)?]),* $(,)?], [$(($a:expr, $b:expr, $dir:expr)),* $(,)?]) => {
			ValidLevelData::try_from(LevelData {
				vertices: vec![VertexData::default(); $vertices],
				cycles: vec![$(CycleData { vertex_indices: vec![$($cycles),*], cycle_turnability: CycleTurnability::Always }),*],
				linkages: vec![$(LinkageData { cycle_a_index: $a, cycle_b_index: $b, direction: $dir }),*]
			})
		};
	}

	#[test]
	fn test_validation() {
		assert!(level_data!(4, [[0, 1], [2, 3]], []).is_ok());
		assert!(level_data!(6, [[0, 1, 2], [2, 3, 4, 5]], []).is_ok());
		assert!(level_data!(6, [[0, 1, 2, 4], [2, 3, 4, 5]], []).is_ok());
		assert!(level_data!(4, [[0, 1], [2, 3]], [(0, 0, Coincident)]).is_ok());
		assert!(level_data!(4, [[0, 1], [2, 3]], [(0, 1, Coincident)]).is_ok());
		assert!(level_data!(
			4,
			[[0, 1], [2, 3]],
			[(0, 1, Coincident), (1, 0, Coincident)]
		)
		.is_ok());
		assert!(level_data!(
			6,
			[[0, 1], [2, 3], [4, 5]],
			[(0, 1, Coincident), (1, 2, Inverse)]
		)
		.is_ok());
		assert!(level_data!(
			6,
			[[0, 1], [2, 3], [4, 5]],
			[(0, 1, Coincident), (1, 2, Inverse), (0, 2, Inverse)]
		)
		.is_ok());
		assert!(level_data!(
			6,
			[[0, 1], [2, 3], [4, 5]],
			[(0, 1, Coincident), (1, 2, Coincident), (0, 2, Coincident)]
		)
		.is_ok());
		assert!(level_data!(5, [[0, 1], [2, 3], [4, 2]], [(0, 1, Coincident)]).is_ok());

		assert!(level_data!(6, [[0, 1, 2, 4], [0, 2, 3, 4, 5]], []).is_err());
		assert!(level_data!(4, [[0, 1], [2, 3]], [(0, 0, Inverse)]).is_err());
		assert!(level_data!(3, [[0, 1], [1, 2]], [(0, 1, Coincident)]).is_err());
		assert!(level_data!(4, [[0, 1], [2, 3]], [(0, 1, Coincident), (1, 0, Inverse)]).is_err());
		assert!(level_data!(
			6,
			[[0, 1], [2, 3], [4, 5]],
			[(0, 1, Coincident), (1, 2, Inverse), (0, 2, Coincident)]
		)
		.is_err());
		assert!(level_data!(
			6,
			[[0, 1], [2, 3], [4, 5]],
			[(0, 1, Inverse), (1, 2, Coincident), (0, 2, Coincident)]
		)
		.is_err());
		assert!(level_data!(
			8,
			[[0, 1], [2, 3], [4, 5], [6, 7]],
			[
				(0, 1, Coincident),
				(1, 2, Coincident),
				(2, 3, Coincident),
				(3, 1, Inverse)
			]
		)
		.is_err());
		assert!(level_data!(
			5,
			[[0, 1], [2, 3], [4, 2]],
			[(0, 1, Coincident), (0, 2, Coincident)]
		)
		.is_err());
	}
}
