//! Structural components for gameplay objects.
//! Components with a more specialized puspose belong to their respective modules

use super::level::{LevelData, LinkedCycleDirection};
use bevy::{ecs::system::SystemParam, prelude::*};

/// [`Object`] entity that represents the player character
#[derive(Component, Debug, Clone, Copy, Default, Reflect)]
pub struct Player;

/// [`Object`] entity that represents a push box
#[derive(Component, Debug, Clone, Copy, Default, Reflect)]
pub struct SokoBox;

/// [`Glyph`] entity that represents the destination of a [`Player`] entity
#[derive(Component, Debug, Clone, Copy, Default, Reflect)]
pub struct Goal;

/// [`Glyph`] entity that represents the destination of a [`Box`] entity
#[derive(Component, Debug, Clone, Copy, Default, Reflect)]
pub struct SokoButton;

/// Movable thing
#[derive(Component, Debug, Clone, Copy, Default, Reflect)]
pub struct Object;

/// Imovable (until we add second layer cycles) thing
#[derive(Component, Debug, Clone, Copy, Default, Reflect)]
pub struct Glyph;

/// A vertex (node) on the circle
#[derive(Component, Debug, Clone, Copy, Default, Reflect)]
pub struct Vertex;

/// A visual detector on the circle
#[derive(Component, Debug, Clone, Copy, Default, Reflect)]
pub struct Detector {
	/// Index into [`LevelData::cycles`]
	pub cycle: usize,
	/// Index into [`CycleData::vertex_positions`](crate::game::level::CycleData::vertex_positions) on the cycle given by [`Self::cycle`]
	pub offset: usize,
	/// Index into [`LevelData::detectors`]
	pub detector_id: usize,
}

/// A visual wall on the circle
#[derive(Component, Debug, Clone, Copy, Default, Reflect)]
pub struct Wall {
	/// Index into [`LevelData::cycles`]
	pub cycle: usize,
	/// Index into [`CycleData::vertex_positions`](crate::game::level::CycleData::vertex_positions) on the cycle given by [`Self::cycle`]
	pub offset: usize,
}

/// A component describing a cycle
#[derive(Component, Debug, Clone, Reflect)]
pub struct Cycle {
	/// ID corresponding to the numerical level index of this cycle
	pub id: usize,
	/// ID corresponding to the group of linked cycles this cycle is a part of
	pub group_id: usize,
	/// The direction this cycle turns in relation to its parent group
	pub orientation_within_group: LinkedCycleDirection,
}

/// A component holding a strong handle to the current level, making sure it stays alive and providing access to it.
/// Is only present if the level is correct and logical systems can run on it.
#[derive(Resource, Clone, Debug, Default, Deref, DerefMut, Reflect)]
pub struct LevelHandle(pub Option<Handle<LevelData>>);

/// Shorthand [`SystemParam`] that accesses the data of the current level
#[derive(SystemParam)]
pub struct PlayingLevelData<'w> {
	handle: Res<'w, LevelHandle>,
	assets: Res<'w, Assets<LevelData>>,
}

impl PlayingLevelData<'_> {
	pub fn get(&self) -> Result<&LevelData, BevyError> {
		let handle = self
			.handle
			.0
			.as_ref()
			.ok_or_else(|| "No level is currently being played".to_owned())?;
		self.assets
			.get(handle)
			.ok_or_else(|| "No data found for active level".to_owned().into())
	}
}

/// Maps entity IDs to all game entities
#[derive(Resource, Clone, Debug, Default, Reflect)]
pub struct GameStateEcsIndex {
	/// Entities that represent cycles, in the same order they appear
	/// in [`LevelData::cycles`]
	pub cycles: Vec<Entity>,
	/// Entities that represent vertices, in the same order they appear
	/// in [`LevelData::vertices`]
	pub vertices: Vec<Entity>,
	/// Entities that represent objects, in the same order their
	/// current owner vertices appear in [`LevelData::vertices`]
	pub objects: Vec<Option<Entity>>,
	/// Entities that represent glyphs, in the same order their
	/// owner vertices appear in [`LevelData::vertices`]
	pub glyphs: Vec<Option<Entity>>,
}

/// Component that identifies an entity that represents a declared hard link
///
/// Parametrized by the index of the link in [`LevelData::declared_links`]
#[derive(Component, Clone, Copy, PartialEq, Eq, Debug, Default, Reflect, Deref, DerefMut)]
pub struct DeclaredLink(pub usize);

/// Component that identifies an entity that represents a declared one-way link
#[derive(Component, Clone, Copy, PartialEq, Debug, Default, Reflect)]
pub struct DeclaredOneWayLink {
	// Whether there is a center sprite at the origin. (false for detectors)
	pub center_sprite: bool,
	pub start_position: Vec2,
	pub end_cycle: usize,
	pub direction: LinkedCycleDirection,
	pub multiplicity: u64,
}

/// A temporary marker object used for showing something to the player
/// Gets invalidated (all these entities despawn)
#[derive(Component, Debug, Clone, Copy, Reflect)]
pub struct TemporaryMarker;
