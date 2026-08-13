//! Generation of a summary of mechanics that appear in each level

use crate::game::level::{
	CycleTurnability, GlyphData, LevelData, ObjectData, asset::plugin as level_asset_plugin,
	list::LevelList, list_asset::plugin as level_list_asset_plugin,
};
use bevy::{asset::LoadState, prelude::*};
use std::collections::BTreeSet;

#[derive(Debug, Default)]
pub struct MechanicsOverview {
	levels: Vec<LevelMechanics>,
}

impl MechanicsOverview {
	pub fn load() -> Result<Self, MechanicsOverviewError> {
		// Load the level pack via a headless bevy app
		// so we get asset dependencies
		let mut app = App::new();
		app.add_plugins((
			TaskPoolPlugin::default(),
			AssetPlugin::default(),
			level_asset_plugin,
			level_list_asset_plugin,
		));
		let list_handle = app
			.world()
			.resource::<AssetServer>()
			.load::<LevelList>("levels/levels.epilang");
		let level_list = wait_for_level_list(&mut app, list_handle)?;
		let level_assets = app.world().resource::<Assets<LevelData>>();

		// Get the mechanics for each level
		let mut result = Self::default();
		for level in &level_list.levels {
			let level_mechanics = if let Some(level_data) = level_assets.get(&level.data_handle) {
				LevelMechanics {
					level_name: level_data.name.clone(),
					mechanics: Some(MechanicTag::get_from_level(level_data)),
				}
			} else {
				LevelMechanics {
					level_name: level.identifier.clone(),
					mechanics: None,
				}
			};
			result.levels.push(level_mechanics);
		}

		Ok(result)
	}
}

/// Waits until a level list is loaded
///
/// Succeeds even if individual levels fail to load
fn wait_for_level_list(
	app: &mut App,
	handle: Handle<LevelList>,
) -> Result<LevelList, MechanicsOverviewError> {
	while matches!(
		level_list_load_state(app.world(), &handle),
		LoadState::Loading
	) {
		app.update();
	}
	app.world()
		.resource::<Assets<LevelList>>()
		.get(&handle)
		.cloned()
		.ok_or(MechanicsOverviewError::CouldNotLoadList)
}

/// Gets the load state of a level list and its levels
///
/// Marked as loading until all levels are completed,
/// but individual levels are allowed to fail
fn level_list_load_state(world: &World, handle: &Handle<LevelList>) -> LoadState {
	let server = world.resource::<AssetServer>();

	// The list must be loaded first
	let list_load_state = server.load_state(handle);
	if !matches!(list_load_state, LoadState::Loaded) {
		return list_load_state;
	}

	// Check that the levels are also done loading, allow failure
	let level_handles = world
		.resource::<Assets<LevelList>>()
		.get(handle)
		.expect("We just checked it's loaded")
		.levels
		.iter()
		.map(|level| &level.data_handle);
	for handle in level_handles {
		let level_load_state = server.load_state(handle);
		if matches!(level_load_state, LoadState::Loading) {
			return LoadState::Loading;
		}
	}

	LoadState::Loaded
}

impl std::fmt::Display for MechanicsOverview {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		let items = self.levels.iter().map(LevelMechanics::to_json).collect();
		let json_value = serde_json::Value::Array(items);
		write!(f, "{json_value}")
	}
}

/// Error emitted by the mechanics info loader
#[derive(Clone, Debug)]
pub enum MechanicsOverviewError {
	/// Loading of the main level list failed
	CouldNotLoadList,
}

impl std::error::Error for MechanicsOverviewError {}

impl std::fmt::Display for MechanicsOverviewError {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			MechanicsOverviewError::CouldNotLoadList => write!(f, "Failed to load level list"),
		}
	}
}

/// Mechanics report for a single level
#[derive(Debug)]
pub struct LevelMechanics {
	/// Display name or identifier of the level
	level_name: String,
	/// Mechanics that appear in the level, [`None`] if the level could not be loaded
	mechanics: Option<BTreeSet<MechanicTag>>,
}

impl LevelMechanics {
	fn to_json(&self) -> serde_json::Value {
		serde_json::Map::from_iter([
			("name".to_owned(), self.level_name.clone().into()),
			(
				"mechanics".to_owned(),
				self.mechanics
					.as_ref()
					.map(|mechanics| {
						mechanics
							.iter()
							.map(MechanicTag::to_string)
							.collect::<Vec<_>>()
							.into()
					})
					.unwrap_or_default(),
			),
		])
		.into()
	}
}

/// Labels for distinct mechanics that can appear in a level
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum MechanicTag {
	Player,
	MultiPlayer,
	Flag,
	Box,
	Button,
	ColoredBox,
	AutoCycle,
	ManualCycle,
	StillCycle,
	HardLink,
	OneWayLink,
	MultiLink,
	Wall,
	Detector,
}

impl MechanicTag {
	// Analyzes a level to find all appropriate mechanics tags
	pub fn get_from_level(level: &LevelData) -> BTreeSet<Self> {
		let mut tags = BTreeSet::new();

		// Check objects and glyphs on all vertices
		let mut has_colored_box = false;
		let mut has_colored_button = false;
		for vertex in &level.vertices {
			match vertex.object {
				None => {}
				Some(ObjectData::Player) => {
					let has_player_already = !tags.insert(MechanicTag::Player);
					if has_player_already {
						tags.insert(MechanicTag::MultiPlayer);
					}
				}
				Some(ObjectData::Box(color)) => {
					tags.insert(MechanicTag::Box);
					if color.is_some() {
						has_colored_box = true;
					}
				}
			}
			match vertex.glyph {
				None => {}
				Some(GlyphData::Flag) => {
					tags.insert(MechanicTag::Flag);
				}
				Some(GlyphData::Button(color)) => {
					tags.insert(MechanicTag::Button);
					if color.is_some() {
						has_colored_button = true;
					}
				}
			}
		}
		// Only colored boxes or buttons are meaningless,
		// both must be present in a level to be relevan mechanically
		if has_colored_box && has_colored_button {
			tags.insert(MechanicTag::ColoredBox);
		}

		// Check parameters of cycles
		for cycle in &level.cycles {
			match cycle.turnability {
				CycleTurnability::Always => tags.insert(MechanicTag::AutoCycle),
				CycleTurnability::WithPlayer => tags.insert(MechanicTag::ManualCycle),
				CycleTurnability::Never => tags.insert(MechanicTag::StillCycle),
			};
			if !cycle.wall_indices.is_empty() {
				tags.insert(MechanicTag::Wall);
			}
			if !cycle.detector_indices.is_empty() {
				tags.insert(MechanicTag::Detector);
			}
		}

		// Check declared links
		if !level.declared_links.is_empty() {
			tags.insert(MechanicTag::HardLink);
		}
		if !level.declared_one_way_links.is_empty() {
			tags.insert(MechanicTag::OneWayLink);
			if level
				.declared_one_way_links
				.iter()
				.any(|link| link.multiplicity != 1)
			{
				tags.insert(MechanicTag::MultiLink);
			}
		}

		tags
	}
}

impl std::fmt::Display for MechanicTag {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		let key = match self {
			Self::Player => "player",
			Self::MultiPlayer => "multiplayer",
			Self::Flag => "flag",
			Self::Box => "box",
			Self::Button => "button",
			Self::ColoredBox => "colors",
			Self::AutoCycle => "autoCycle",
			Self::ManualCycle => "manualCycle",
			Self::StillCycle => "stillCycle",
			Self::HardLink => "hardLink",
			Self::OneWayLink => "onewayLink",
			Self::MultiLink => "multiLink",
			Self::Wall => "wall",
			Self::Detector => "detector",
		};
		write!(f, "{key}")
	}
}
