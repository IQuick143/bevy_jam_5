use super::{components::LinkedCycleDirection, level::asset::LevelAsset};
use bevy::prelude::*;

/// Trigger event that spawns the content entities of a level
#[derive(Event, Debug)]
pub struct SpawnLevel(pub Handle<LevelAsset>);

/// Enumerates directions in which a cycle can turn
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum CycleTurningDirection {
	/// Rotate in nominal direction of the cycle
	Nominal,
	/// Rotate in reverse direction of the cycle
	Reverse,
}

/// Common data for [`RotateSingleCycle`] and [`RotateCycleGroup`]
#[derive(Clone, Copy, Debug)]
pub struct RotateCycle {
	/// Id of the cycle entity to rotate
	pub target_cycle: Entity,
	/// Direction in which the cycle should turn
	pub direction: CycleTurningDirection,
}

/// Internal event sent to a cycle entity to rotate [`super::components::Object`]
/// entities that lie on the cycle, ignores linkages.
#[derive(Event, Clone, Copy, Debug)]
pub struct RotateSingleCycle(pub RotateCycle);

/// Event sent to a cycle entity to rotate [`super::components::Object`]
/// entities that lie on the cycle and all cycles linked to it
/// Should be sent only if it is valid to rotate the given cycle.
#[derive(Event, Clone, Copy, Debug)]
pub struct RotateCycleGroup(pub RotateCycle);

/// Event sent together with a [`RotateCycleGroup`] event
/// if that rotation is eligible for being recorded in move history
#[derive(Event, Clone, Copy, Debug)]
pub struct RecordCycleGroupRotation(pub RotateCycle);

/// Event that is sent when state of the game map changes,
/// usually by turning a cycle
#[derive(Event, Debug)]
pub struct GameLayoutChanged;

/// Event that is sent to signal that a move should be rewound
#[derive(Event, Debug)]
pub struct UndoMove;

impl std::ops::Mul<LinkedCycleDirection> for CycleTurningDirection {
	type Output = Self;
	fn mul(self, rhs: LinkedCycleDirection) -> Self::Output {
		match rhs {
			LinkedCycleDirection::Coincident => self,
			LinkedCycleDirection::Inverse => -self,
		}
	}
}

impl std::ops::Neg for CycleTurningDirection {
	type Output = Self;
	fn neg(self) -> Self::Output {
		match self {
			CycleTurningDirection::Nominal => CycleTurningDirection::Reverse,
			CycleTurningDirection::Reverse => CycleTurningDirection::Nominal,
		}
	}
}
