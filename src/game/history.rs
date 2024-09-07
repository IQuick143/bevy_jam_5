//! Recording and rewinding of moves

use super::logic::{RecordCycleGroupRotation, RotateCycle, RotateCycleGroup};
use crate::AppSet;
use bevy::prelude::*;

pub(super) fn plugin(app: &mut App) {
	app.init_resource::<MoveHistory>()
		.add_event::<UndoMove>()
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

/// Event that is sent to signal that a move should be rewound
#[derive(Event, Debug)]
pub struct UndoMove;

fn record_moves(
	mut history: ResMut<MoveHistory>,
	mut events: EventReader<RecordCycleGroupRotation>,
) {
	for RecordCycleGroupRotation(rotation) in events.read() {
		history.push(*rotation);
	}
}

fn undo_moves(
	mut events: EventReader<UndoMove>,
	mut history: ResMut<MoveHistory>,
	mut rotations: EventWriter<RotateCycleGroup>,
) {
	for _ in events.read() {
		if let Some(mut rotation) = history.pop() {
			rotation.direction = -rotation.direction;
			rotations.send(RotateCycleGroup(rotation));
		} else {
			log::warn!("Undo command received, but no moves have been recorded");
		}
	}
}
