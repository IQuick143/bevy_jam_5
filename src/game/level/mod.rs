use itertools::Itertools;

use crate::game::prelude::*;

pub mod parser;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ObjectType {
	Box, Player
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum GlyphType {
	Button, Flag
}

#[derive(Debug, Clone)]
pub struct CycleData {
	vertex_indices: Vec<usize>,
	cycle_turnability: CycleTurnability
}

#[derive(Debug, Clone, Copy)]
pub struct LinkageData {
	cycle_a_index: usize,
	cycle_b_index: usize,
	direction: LinkedCycleDirection,
}

#[derive(Debug, Clone, Copy, Default)]
pub struct VertexData {
	object: Option<ObjectType>,
	glyph: Option<GlyphType>,
}

#[derive(Debug, Clone)]
pub struct LevelData {
	vertices: Vec<VertexData>,
	cycles: Vec<CycleData>,
	linkages: Vec<LinkageData>,
}

#[derive(Debug, Clone, Copy)]
pub enum LevelDataValidationError {
	VertexIndexOutOfRange(usize),
	CycleIndexOutOfRange(usize),
	CycleLinkageConflict,
	TooFewVerticesInCycle(usize),
	RepeatingVertexInCycle(usize),
	TooManyVerticesInCycleIntersection(usize, usize),
	OverlappedLinkedCycles(usize, usize)
}

#[derive(Debug, Clone)]
pub struct ValidLevelData(LevelData);

impl TryFrom<LevelData> for ValidLevelData {
	type Error = LevelDataValidationError;

	fn try_from(value: LevelData) -> Result<Self, Self::Error> {
		// Everything about integrity of individual cycles
		for (i, cycle) in value.cycles.iter().enumerate() {
			if cycle.vertex_indices.len() < 2 {
				return Err(LevelDataValidationError::TooFewVerticesInCycle(i));
			}
			if !cycle.vertex_indices.iter().all_unique() {
				return Err(LevelDataValidationError::RepeatingVertexInCycle(i));
			}
			if let Some(j) = cycle.vertex_indices.iter()
				.copied()
				.find(|&j| j >= value.vertices.len()) {
				return Err(LevelDataValidationError::VertexIndexOutOfRange(j));
			}
		}

		// A very, *very* inefficient way of finding conflicts between cycle links
		// Only reasonably fast (and space-efficient) for 20 cycles or so
		let mut cycle_links = (0..value.cycles.len())
			.map(|i| (0..value.cycles.len())
				.map(|j| if i == j { Some(LinkedCycleDirection::Coincident) } else { None })
				.collect::<Vec<_>>())
			.collect::<Vec<_>>();
		fn set_link(
			links: &mut Vec<Vec<Option<LinkedCycleDirection>>>,
			i: usize,
			j: usize,
			dir: LinkedCycleDirection
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
		let vertices_per_cycle = value.cycles.iter()
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

		Ok(Self(value))
	}
}

impl std::ops::Deref for ValidLevelData {
	type Target = LevelData;
	fn deref(&self) -> &Self::Target {
		&self.0
	}
}

impl std::borrow::Borrow<LevelData> for ValidLevelData {
	fn borrow(&self) -> &LevelData {
		&self.0
	}
}

impl std::fmt::Display for LevelDataValidationError {
	fn fmt(&self, _f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		todo!()
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
		assert!(level_data!(4, [[0, 1], [2, 3]], [(0, 1, Coincident), (1, 0, Coincident)]).is_ok());
		assert!(level_data!(6, [[0, 1], [2, 3], [4, 5]], [(0, 1, Coincident), (1, 2, Inverse)]).is_ok());
		assert!(level_data!(6, [[0, 1], [2, 3], [4, 5]], [(0, 1, Coincident), (1, 2, Inverse), (0, 2, Inverse)]).is_ok());
		assert!(level_data!(6, [[0, 1], [2, 3], [4, 5]], [(0, 1, Coincident), (1, 2, Coincident), (0, 2, Coincident)]).is_ok());
		assert!(level_data!(5, [[0, 1], [2, 3], [4, 2]], [(0, 1, Coincident)]).is_ok());

		assert!(level_data!(6, [[0, 1, 2, 4], [0, 2, 3, 4, 5]], []).is_err());
		assert!(level_data!(4, [[0, 1], [2, 3]], [(0, 0, Inverse)]).is_err());
		assert!(level_data!(3, [[0, 1], [1, 2]], [(0, 1, Coincident)]).is_err());
		assert!(level_data!(4, [[0, 1], [2, 3]], [(0, 1, Coincident), (1, 0, Inverse)]).is_err());
		assert!(level_data!(6, [[0, 1], [2, 3], [4, 5]], [(0, 1, Coincident), (1, 2, Inverse), (0, 2, Coincident)]).is_err());
		assert!(level_data!(6, [[0, 1], [2, 3], [4, 5]], [(0, 1, Inverse), (1, 2, Coincident), (0, 2, Coincident)]).is_err());
		assert!(level_data!(8, [[0, 1], [2, 3], [4, 5], [6, 7]], [(0, 1, Coincident), (1, 2, Coincident), (2, 3, Coincident), (3, 1, Inverse)]).is_err());
		assert!(level_data!(5, [[0, 1], [2, 3], [4, 2]], [(0, 1, Coincident), (0, 2, Coincident)]).is_err());
	}
}
