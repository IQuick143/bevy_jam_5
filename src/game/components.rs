//! Spawn the player.

use bevy::prelude::*;

#[derive(Component, Debug, Clone, Copy, Default, Reflect)]
pub struct Player;

#[derive(Component, Debug, Clone, Copy, Default, Reflect)]
pub struct Box;

/// Movable thing
#[derive(Component, Debug, Clone, Copy, Default, Reflect)]
pub struct Object;

/// Imovable (until we add second layer cycles) thing
#[derive(Component, Debug, Clone, Copy, Default, Reflect)]
pub struct Glyph;

/// Link to a vertex this object is occupying
#[derive(Component, Debug, Clone, Reflect)]
pub struct VertexPosition(Entity);

/// A vertex (node) on the circle
#[derive(Component, Debug, Clone, Copy, Default, Reflect)]
pub struct Vertex;

/// Component of the Vertex representing a link to an object occupying this place
#[derive(Component, Debug, Clone, Reflect)]
pub struct PlacedObject(Entity);

/// Component of the Vertex representing a link to a glyph occupying this place
#[derive(Component, Debug, Clone, Reflect)]
pub struct PlacedGlyph(Entity);
