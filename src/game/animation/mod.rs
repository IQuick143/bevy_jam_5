//! Everything relating to animation of game objects

mod jump_turn;
mod paths;
mod spin;

use crate::game::logic::CycleTurningDirection;
use bevy::prelude::*;

pub fn plugin(app: &mut App) {
	app.init_resource::<TurnAnimationLength>().add_plugins((
		jump_turn::plugin,
		paths::plugin,
		spin::plugin,
	));
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Reflect)]
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

impl RotationDirection {
	pub fn as_number(self) -> f32 {
		match self {
			Self::Clockwise => 1.0,
			Self::CounterClockwise => -1.0,
		}
	}
}

fn animation_easing_function(t: f32) -> f32 {
	// Quadratic ease-out.
	// Looks better than a flat rotation, but is easier
	// to chain like we do than a double-ended easing function
	if t > 0.5 {
		1.0 - (1.0 - t).powi(2) * 4.0 / 3.0
	} else {
		t * 4.0 / 3.0
	}
}

/// Length in seconds of animations that are played when a cycle is turned
#[derive(Resource, Clone, Copy, PartialEq, PartialOrd, Deref, DerefMut, Debug, Reflect)]
pub struct TurnAnimationLength(pub f32);

impl TurnAnimationLength {
	pub const DEFAULT: Self = Self(0.5);
}

impl Default for TurnAnimationLength {
	fn default() -> Self {
		Self::DEFAULT
	}
}
