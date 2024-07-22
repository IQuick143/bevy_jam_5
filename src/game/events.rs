use bevy::prelude::*;

#[derive(Event, Debug)]
pub struct StartLevel;

/// Event sent to a cycle entity to rotate [`super::components::Object`]
/// entities that lie on the cycle
#[derive(Event, Debug, Clone, Copy, PartialEq, Eq)]
pub enum RotateCycle {
    /// Rotate in nominal direction of the cycle
    Nominal,
    /// Rotate in reverse direction of the cycle
    Reverse
}

/// Event that is sent when state of the game map changes,
/// usualy by turning a cycle
#[derive(Event, Debug)]
pub struct GameLayoutChanged;
