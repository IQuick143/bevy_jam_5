//! Spawn the player.

use bevy::prelude::*;

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

/// Defines conditions under which a cycle may be turned
#[derive(Component, Debug, Clone, Copy, PartialEq, Eq, Reflect)]
pub enum CycleTurnability {
	/// Cycle may be turned anytime
	Always,
	/// Cycle may be turned when a [`Player`] entity lies on one of its vertices
	WithPlayer,
}

/// Determines whether a cycle may be turned at any given moment
#[derive(Component, Debug, Clone, Reflect)]
pub struct ComputedCycleTurnability(pub bool);

/// Relative direction of two cycles that are to turn together
#[derive(Debug, Clone, Copy, PartialEq, Eq, Reflect)]
pub enum LinkedCycleDirection {
	/// The cycles will turn in the same direction
	Coincident,
	/// The cycles will turn in opposite directions
	Inverse,
}

/// Describes the linkage of multiple cycles such that they will always turn together
#[derive(Component, Debug, Clone, Reflect)]
pub struct LinkedCycles(pub Vec<(Entity, LinkedCycleDirection)>);

impl std::ops::Mul for LinkedCycleDirection {
	type Output = Self;
	fn mul(self, rhs: Self) -> Self::Output {
		if self == rhs {
			LinkedCycleDirection::Coincident
		} else {
			LinkedCycleDirection::Inverse
		}
	}
}
