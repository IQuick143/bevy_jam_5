//! Recording and rewinding of moves

use crate::AppSet;

use super::prelude::*;

pub(super) fn plugin(app: &mut App) {
	app.add_systems(
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
