//! The level list and construction thereof

use super::LevelData;
use bevy::{asset::*, reflect::Reflect, utils::default};
use itertools::Itertools as _;

/// A list of levels split into sections
#[derive(Clone, Debug, Reflect, Asset)]
pub struct LevelList {
	/// Levels in the list
	pub levels: Vec<LevelInfo>,
	/// Sections of the level list
	pub hubs: Vec<HubLevelInfo>,
	/// Index of the root/main hub
	pub root_hub: usize,
}

/// Information about a single level
#[derive(Clone, Debug, Reflect)]
pub struct LevelInfo {
	/// Path to the level asset
	pub path: AssetPath<'static>,
	/// Unique indentifier for persistence purposes
	pub identifier: String,
	/// Handle to the level data asset in the level file
	pub data_handle: Handle<LevelData>,
	/// Index of the hub that this level logically belongs to
	pub parent_hub: usize,
	/// Index of the level that comes after this
	/// in the standard playing order
	pub next_level: Option<usize>,
}

/// Information about a hub and a group of levels it contains
#[derive(Clone, Debug, Default, Reflect)]
pub struct HubLevelInfo {
	/// Index of the hub that this hub logically subdivides
	pub parent_hub: Option<usize>,
	/// List of hubs that subdivide this hub
	pub child_hubs: Vec<usize>,
	/// List of levels that belong directly to the hub (not transitively)
	pub levels: Vec<usize>,
}

/// Helper object for construction of a level list
pub struct LevelListBuilder {
	/// The level list that is being built
	list: LevelList,
	/// Whether a parent hub has been specified for each level
	/// (the level data structures alone do not reflect that)
	has_parent_hub: Vec<bool>,
}

impl LevelListBuilder {
	pub fn new() -> Self {
		Self {
			list: LevelList {
				levels: Vec::new(),
				hubs: Vec::new(),
				root_hub: 0,
			},
			has_parent_hub: Vec::new(),
		}
	}

	pub fn add_level(&mut self, path: &str) -> Result<usize, LevelListBuildError> {
		self.list.levels.push(LevelInfo {
			parent_hub: 0,
			next_level: None,
			data_handle: default(),
			path: AssetPath::try_parse(path)
				.map_err(|err| LevelListBuildError::BadAssetPath(path.to_owned(), err))?
				.into_owned(),
			identifier: path.to_string(),
		});
		self.has_parent_hub.push(false);
		Ok(self.list.levels.len() - 1)
	}

	pub fn add_hub(&mut self) -> Result<usize, LevelListBuildError> {
		self.list.hubs.push(HubLevelInfo {
			parent_hub: None,
			..default()
		});
		Ok(self.list.hubs.len() - 1)
	}

	pub fn set_parent_for_level(
		&mut self,
		level_index: usize,
		parent_index: usize,
	) -> Result<(), LevelListBuildError> {
		if level_index >= self.list.levels.len() {
			return Err(LevelListBuildError::LevelIndexOutOfRange(level_index));
		}
		if parent_index >= self.list.hubs.len() {
			return Err(LevelListBuildError::HubIndexOutOfRange(parent_index));
		}
		self.list.levels[level_index].parent_hub = parent_index;
		self.has_parent_hub[level_index] = true;
		Ok(())
	}

	pub fn set_parent_for_hub(
		&mut self,
		hub_index: usize,
		parent_index: usize,
	) -> Result<(), LevelListBuildError> {
		if hub_index >= self.list.hubs.len() {
			return Err(LevelListBuildError::HubIndexOutOfRange(hub_index));
		}
		if parent_index >= self.list.hubs.len() {
			return Err(LevelListBuildError::HubIndexOutOfRange(parent_index));
		}
		if self.is_hub_ancestor_of(hub_index, parent_index) {
			return Err(LevelListBuildError::CycleInHubHierarchy(
				hub_index,
				parent_index,
			));
		}
		self.list.hubs[hub_index].parent_hub = Some(parent_index);
		Ok(())
	}

	pub fn set_next_level(
		&mut self,
		level_index: usize,
		next_index: usize,
	) -> Result<(), LevelListBuildError> {
		if level_index >= self.list.levels.len() {
			return Err(LevelListBuildError::LevelIndexOutOfRange(level_index));
		}
		if next_index >= self.list.levels.len() {
			return Err(LevelListBuildError::LevelIndexOutOfRange(next_index));
		}
		self.list.levels[level_index].next_level = Some(next_index);
		Ok(())
	}

	pub fn build(
		mut self,
		asset_load_context: &mut LoadContext,
	) -> Result<LevelList, LevelListBuildError> {
		if let Some((i, _)) = self.has_parent_hub.iter().find_position(|x| !**x) {
			return Err(LevelListBuildError::OrphanedLevel(i));
		}
		self.find_and_set_root_hub()?;
		self.fill_hub_child_lists();
		self.load_level_assets(asset_load_context);
		Ok(self.list)
	}

	/// Checks if `ancestor` is the same as or an ancestor of `successor`
	/// in the hub tree hierarchy
	fn is_hub_ancestor_of(&self, ancestor: usize, successor: usize) -> bool {
		let mut current = successor;
		if current == ancestor {
			return true;
		}
		while let Some(parent) = self.list.hubs[current].parent_hub {
			current = parent;
			if current == ancestor {
				return true;
			}
		}
		false
	}

	/// Finds the root hub and sets its index as the [`LevelList::root_hub`].
	/// Fails if there are no hubs or if there is more than one root.
	fn find_and_set_root_hub(&mut self) -> Result<(), LevelListBuildError> {
		let mut root_hubs = self
			.list
			.hubs
			.iter()
			.positions(|hub| hub.parent_hub.is_none());
		let Some(first_root) = root_hubs.next() else {
			return Err(LevelListBuildError::NoHub);
		};
		if let Some(second_root) = root_hubs.next() {
			return Err(LevelListBuildError::MultipleRootHubs([
				first_root,
				second_root,
			]));
		}
		self.list.root_hub = first_root;
		Ok(())
	}

	/// Fills in the child lists of hubs based on parent references
	///
	/// Assumes the child lists start off empty
	fn fill_hub_child_lists(&mut self) {
		for i in 0..self.list.hubs.len() {
			if let Some(parent) = self.list.hubs[i].parent_hub {
				self.list.hubs[parent].child_hubs.push(i);
			}
		}
		for (i, level) in self.list.levels.iter().enumerate() {
			self.list.hubs[level.parent_hub].levels.push(i);
		}
	}

	fn load_level_assets(&mut self, load_context: &mut LoadContext) {
		for level in &mut self.list.levels {
			level.data_handle = load_context.load(&level.path);
		}
	}
}

#[derive(Debug)]
pub enum LevelListBuildError {
	/// Asset path has an invalid format
	BadAssetPath(String, ParseAssetPathError),
	/// Level index is too large
	LevelIndexOutOfRange(usize),
	/// Hub index is too large
	HubIndexOutOfRange(usize),
	/// Adding a parent-child relation would create a cycle
	/// in hub hierarchy
	CycleInHubHierarchy(usize, usize),
	/// No hub has been declared by build time
	NoHub,
	/// More than one hub in root position at build time
	MultipleRootHubs([usize; 2]),
	/// A level does not have a parent set by build time
	OrphanedLevel(usize),
}

impl std::error::Error for LevelListBuildError {}

impl std::fmt::Display for LevelListBuildError {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::BadAssetPath(path, reason) => write!(f, "'{path}' is not a valid path to an asset: {reason}"),
			Self::HubIndexOutOfRange(i) => write!(f, "trying to reference hub {i}, but there are not that many hubs"),
			Self::LevelIndexOutOfRange(i) => write!(f, "trying to reference level {i}, but there are not that many levels"),
			Self::CycleInHubHierarchy(child, parent) => write!(f, "adding hub {child} as child of hub {parent} would create a loop in hierarchy"),
			Self::NoHub => f.write_str("level list is missing a hub"),
			Self::MultipleRootHubs([first, second]) => write!(f, "only one root hub can exist, but hubs {first} and {second} are both in root position"),
			Self::OrphanedLevel(i) => write!(f, "level {i} does not have a parent set"),
		}
	}
}
