//! Structural components for gameplay objects.
//! Components with a more specialized puspose belong to their respective modules

use bevy::prelude::*;

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
#[derive(Component, Debug, Clone, Reflect)]
pub struct PlacedObject(pub Option<Entity>);

/// Component of the Vertex representing a link to a glyph occupying this place
#[derive(Component, Debug, Clone, Reflect)]
pub struct PlacedGlyph(pub Option<Entity>);

/// A list of [`Vertex`] entities that are part of a single cycle
#[derive(Component, Debug, Clone, Reflect)]
pub struct CycleVertices(pub Vec<Entity>);

/// Describes the linkage of multiple cycles such that they will always turn together
#[derive(Component, Debug, Clone, Reflect)]
pub struct LinkedCycles(pub Vec<(Entity, super::level::LinkedCycleDirection)>);
