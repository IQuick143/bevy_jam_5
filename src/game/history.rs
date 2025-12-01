//! Recording and rewinding of moves

use super::{
	logic_relay::{RotateCycle, RotateCycleGroup, RotateCycleGroupWithResult, RotationCause},
	spawn::LevelInitialization,
};
use crate::AppSet;
use bevy::prelude::*;

pub(super) fn plugin(app: &mut App) {
	app.init_resource::<MoveHistory>()
		.add_message::<UndoMove>()
		.add_systems(LevelInitialization, |mut history: ResMut<MoveHistory>| {
			history.clear()
		})
		.add_systems(
			Update,
			(
				record_moves
					.after(AppSet::ExecuteInput)
					.before(AppSet::UpdateVisuals),
				undo_moves
					.after(AppSet::ExecuteInput)
					.before(AppSet::GameLogic),
			),
		);
}

/// Records the history of moves within a single session of a level
#[derive(Resource, Deref, DerefMut, Clone, Debug, Default)]
pub struct MoveHistory(pub Vec<RotateCycle>);

/// Message that is sent to signal that a move should be rewound
#[derive(Message, Debug)]
pub struct UndoMove;

fn record_moves(
	mut history: ResMut<MoveHistory>,
	mut events: MessageReader<RotateCycleGroupWithResult>,
) {
	// Zip the events, they should match eventually
	for event in events.read() {
		if event.action.cause == RotationCause::Manual && !event.result.blocked() {
			history.push(event.action.rotation);
		}
	}
}

fn undo_moves(
	mut events: MessageReader<UndoMove>,
	mut history: ResMut<MoveHistory>,
	mut rotations: MessageWriter<RotateCycleGroup>,
) {
	for _ in events.read() {
		if let Some(mut rotation) = history.pop() {
			rotation.amount *= -1;
			rotations.write(RotateCycleGroup {
				rotation,
				cause: RotationCause::Undo,
			});
		} else {
			log::warn!("Undo command received, but no moves have been recorded");
		}
	}
}
