//! Spawn the player.

use std::f32::consts::TAU;

use bevy::prelude::*;

use super::events::CycleTurningDirection;

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

#[derive(Debug, Clone, Copy, Default, Reflect)]
pub enum RotationDirection {
	#[default]
	Clockwise,
	CounterClockwise,
}

impl From<CycleTurningDirection> for RotationDirection {
	fn from(value: CycleTurningDirection) -> Self {
		match value {
			CycleTurningDirection::Nominal => RotationDirection::Clockwise,
			CycleTurningDirection::Reverse => RotationDirection::CounterClockwise,
		}
	}
}

/// Component for enabling animation behaviour
#[derive(Component, Debug, Clone, Copy, Default, Reflect)]
pub struct AnimatedObject {
	/// Which way the slerp should go.
	pub rotation_direction: RotationDirection,
	/// <0.0-1.0> percentage of the animation progress
	pub progress: f32,
	/// Center of rotation
	pub cycle_center: Vec3,
	/// The direction we're starting from, if None, the animation skips to the end
	pub start_direction: Option<Dir2>,
	pub start_magnitude: f32,
	/// The direction we're ending on, if None, the animation cannot play
	pub final_direction: Option<Dir2>,
	pub final_magnitude: f32,
}

impl AnimatedObject {
	pub fn sample(&self) -> Option<Vec3> {
		let target = self.final_direction?;
		Some(if let Some(source) = self.start_direction {
			let t = self.progress.clamp(0.0, 1.0);
			let dir = {
				let mut angle = Vec2::angle_between(*source, *target);
				match self.rotation_direction {
					RotationDirection::Clockwise => {
						if angle > 0.0 {
							angle -= TAU
						}
					}
					RotationDirection::CounterClockwise => {
						if angle < 0.0 {
							angle += TAU
						}
					}
				}
				Rot2::radians(angle * t) * source
			};
			let magnitude = f32::lerp(self.start_magnitude, self.final_magnitude, t);
			self.cycle_center + (magnitude * dir).extend(0.0)
		} else {
			self.cycle_center + (self.final_magnitude * target).extend(0.0)
		})
	}
}
