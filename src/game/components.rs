//! Spawn the player.

use std::f32::consts::TAU;

use bevy::{
	math::bounding::{Aabb2d, BoundingCircle},
	prelude::*,
};

use super::{events::CycleTurningDirection, level::ThingType};

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

/// Identifier in enum form
#[derive(Component, Debug, Clone, Copy, Reflect)]
pub struct ObjectKind(pub ThingType);

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
#[derive(Component, Debug, Clone, Copy, PartialEq, Eq, Hash, Reflect)]
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

#[derive(Component, Clone, Copy, Debug, Reflect)]
pub struct CycleInterationRadius(pub f32);

#[derive(Component, Clone, Copy, PartialEq, Eq, Debug, Reflect, Default)]
pub enum CycleInteraction {
	#[default]
	None,
	Hover,
	LeftClick,
	RightClick,
}

#[derive(Component, Clone, Copy, Debug, Reflect)]
pub struct HoverText;

#[derive(Component, Clone, Debug, Reflect)]
pub struct Hoverable {
	pub hover_text: &'static str,
	pub hover_bounding_circle: Option<BoundingCircle>,
	pub hover_bounding_box: Option<Aabb2d>,
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
			let t = animation_easing_function(self.progress).clamp(0.0, 1.0);
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

fn animation_easing_function(t: f32) -> f32 {
	// Quadratic ease-out.
	// Looks better that a flat rotation, but is easier
	// to chain like we do that a double-ended easing function
	if t > 0.5 {
		1.0 - (1.0 - t).powi(2) * 4.0 / 3.0
	} else {
		t * 4.0 / 3.0
	}
}

/// A component that causes an entity to rotate steadily
#[derive(Component, Clone, Copy, Debug, Reflect)]
pub struct SpinAnimation {
	pub frequency: f32,
	pub current_phase: f32,
}

impl SpinAnimation {
	pub fn progress(&mut self, delta_seconds: f32) {
		self.current_phase -= delta_seconds * self.frequency;
		if self.current_phase < 0.0 {
			self.current_phase += TAU;
		}
	}

	pub fn sample(&self) -> f32 {
		self.current_phase
	}

	pub const DEFAULT_FREQUENCY: f32 = 0.3;
}

impl Default for SpinAnimation {
	fn default() -> Self {
		Self {
			frequency: Self::DEFAULT_FREQUENCY,
			current_phase: 0.0,
		}
	}
}

/// A component that lets an entity rotate quickly
#[derive(Component, Clone, Copy, Debug, Reflect)]
pub struct JumpTurnAnimation {
	pub current_phase: f32,
	pub jump_animation_progress: f32,
	pub jump_animation_time: f32,
	pub jump_animation_magitude: f32,
}

impl JumpTurnAnimation {
	pub fn progress(&mut self, delta_seconds: f32) {
		if self.jump_animation_progress < 1.0 {
			self.jump_animation_progress += delta_seconds / self.jump_animation_time;
		}
	}

	pub fn make_jump(&mut self, magnitude: f32, animation_time: f32) {
		self.current_phase = self.sample() + magnitude;
		self.jump_animation_time = animation_time;
		self.jump_animation_magitude = magnitude;
		self.jump_animation_progress = 0.0;
	}

	pub fn sample(&self) -> f32 {
		if self.jump_animation_progress >= 1.0 {
			self.current_phase
		} else {
			self.current_phase
				- (1.0 - animation_easing_function(self.jump_animation_progress))
					* self.jump_animation_magitude
		}
	}
}

impl Default for JumpTurnAnimation {
	fn default() -> Self {
		Self {
			current_phase: 0.0,
			jump_animation_progress: 0.0,
			jump_animation_magitude: 0.0,
			jump_animation_time: 0.0,
		}
	}
}
