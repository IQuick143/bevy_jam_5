//! Structural components for gameplay objects.
//! Components with a more specialized puspose belong to their respective modules

use bevy::prelude::*;

use super::level::{LevelData, LinkedCycleDirection};

/// Marker component for entities that belong to a single level
#[derive(Component, Clone, Copy, Debug, Default, Reflect)]
pub struct LevelScoped;

/// [`Object`] entity that represents the player character
#[derive(Component, Debug, Clone, Copy, Default, Reflect)]
pub struct Player;

/// [`Object`] entity that represents a push box
#[derive(Component, Debug, Clone, Copy, Default, Reflect)]
pub struct Box;

/// [`Glyph`] entity that represents the destination of a [`Player`] entity
#[derive(Component, Debug, Clone, Copy, Default, Reflect)]
pub struct Goal;

/// [`Glyph`] entity that represents the destination of a [`Box`] entity
#[derive(Component, Debug, Clone, Copy, Default, Reflect)]
pub struct BoxSlot;

/// Movable thing
#[derive(Component, Debug, Clone, Copy, Default, Reflect)]
pub struct Object;

/// Imovable (until we add second layer cycles) thing
#[derive(Component, Debug, Clone, Copy, Default, Reflect)]
pub struct Glyph;

/// Link to a vertex this object is occupying
#[derive(Component, Debug, Clone, Reflect)]
pub struct VertexPosition(pub Entity);

/// A vertex (node) on the circle
#[derive(Component, Debug, Clone, Copy, Default, Reflect)]
pub struct Vertex;

/// Component of the Vertex representing a link to an object occupying this place
#[derive(Component, Debug, Clone, Copy, Reflect)]
pub struct PlacedObject(pub Option<Entity>);

/// Component of the Vertex representing a link to a glyph occupying this place
#[derive(Component, Debug, Clone, Reflect)]
pub struct PlacedGlyph(pub Option<Entity>);

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

/// A list of [`Vertex`] entities that are part of a single cycle
#[derive(Component, Debug, Clone, Reflect)]
pub struct CycleVertices(pub Vec<Entity>);

/// A component holding a strong handle to the current level, making sure it stays alive and providing access to it.
#[derive(Resource, Debug, Clone, Reflect)]
pub struct LevelHandle(pub Handle<LevelData>);

/// Component carrying the data mapping level indices to cycle entities.
#[derive(Resource, Clone, Debug, Default, Reflect)]
pub struct CycleEntities(pub Vec<Entity>);

/// Reference to the target cycle of a link entity.
/// The source cycle is its [`Parent`].
#[derive(Component, Debug, Clone, Copy, Reflect)]
pub struct LinkTargetCycle(pub Entity);

/// Multiplicity of a one-way link
#[derive(Component, Clone, Copy, Deref, DerefMut, Debug, Reflect)]
pub struct LinkMultiplicity(pub u64);
