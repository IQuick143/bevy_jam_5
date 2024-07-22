use bevy::prelude::*;

#[derive(Event, Debug)]
pub struct StartLevel;

/// Enumerates directions in which a cycle can turn
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum CycleTurningDirection {
	/// Rotate in nominal direction of the cycle
	Nominal,
	/// Rotate in reverse direction of the cycle
	Reverse,
}

/// Event sent to a cycle entity to rotate [`super::components::Object`]
/// entities that lie on the cycle
#[derive(Event, Debug)]
pub struct RotateCycle {
	/// Id of the cycle entity to rotate
	pub target_cycle: Entity,
	/// Direction in which the cycle should turn
	pub direction: CycleTurningDirection,
}

/// Event that is sent when state of the game map changes,
/// usualy by turning a cycle
#[derive(Event, Debug)]
pub struct GameLayoutChanged;
