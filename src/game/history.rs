//! Recording and rewinding of moves

use super::{
	logic_relay::{RotateCycle, RotateCycleGroup, RotateCycleGroupWithResult, RotationCause},
	spawn::LevelInitialization,
};
use crate::AppSet;
use bevy::prelude::*;

pub(super) fn plugin(app: &mut App) {
	app.init_resource::<MoveHistory>()
		.add_message::<AlterHistory>()
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
#[derive(Resource, Clone, Debug, Default)]
pub struct MoveHistory {
	/// Moves that have been recorded so far
	moves: Vec<RotateCycle>,
	/// Index into [`Self::moves`] that indicates the current state
	cursor: usize,
}

impl MoveHistory {
	pub fn clear(&mut self) {
		*self = default();
	}

	fn push(&mut self, rotation: RotateCycle) {
		// Drop everything from the old history branch
		self.moves.truncate(self.cursor);
		self.moves.push(rotation);
		self.cursor += 1;
	}

	fn undo(&mut self) -> Option<RotateCycle> {
		if self.cursor == 0 {
			None
		} else {
			self.cursor -= 1;
			// This should always be Some, but play it safe
			// Return the reversed move (the move that has to be done to undo it)
			self.moves.get(self.cursor).copied().map(|mut r| {
				r.amount *= -1;
				r
			})
		}
	}

	fn redo(&mut self) -> Option<RotateCycle> {
		self.moves
			.get(self.cursor)
			.copied()
			.inspect(|_| self.cursor += 1)
	}

	pub fn has_undoable_move(&self) -> bool {
		self.cursor > 0
	}

	pub fn has_redoable_move(&self) -> bool {
		self.cursor < self.moves.len()
	}
}

/// Message that is sent to signal that history traversal should be performed
#[derive(Message, Clone, Copy, PartialEq, Eq, Debug)]
pub enum AlterHistory {
	Undo,
	Redo,
}

impl AlterHistory {
	/// Plays the history traversal action on a history resource
	fn execute(self, history: &mut MoveHistory) -> Option<RotateCycle> {
		match self {
			Self::Undo => history.undo(),
			Self::Redo => history.redo(),
		}
	}

	/// Gets the appropriate [`RotationCause`] for the message
	fn to_rotation_cause(self) -> RotationCause {
		match self {
			Self::Undo => RotationCause::Undo,
			Self::Redo => RotationCause::Redo,
		}
	}
}

fn record_moves(
	mut history: ResMut<MoveHistory>,
	mut events: MessageReader<RotateCycleGroupWithResult>,
) {
	// Zip the events, they should match eventually
	for RotateCycleGroupWithResult { action, result } in events.read() {
		if action.cause == RotationCause::Manual && !result.blocked() && result.objects_moved {
			history.push(action.rotation);
		}
	}
}

fn undo_moves(
	mut events: MessageReader<AlterHistory>,
	mut history: ResMut<MoveHistory>,
	mut rotations: MessageWriter<RotateCycleGroup>,
) {
	for alter in events.read() {
		if let Some(rotation) = alter.execute(&mut history) {
			rotations.write(RotateCycleGroup {
				rotation,
				cause: alter.to_rotation_cause(),
			});
		} else {
			log::warn!("History traversal command received, but no moves are available");
		}
	}
}

#[cfg(test)]
mod test {
	use super::*;

	#[test]
	fn empty_history() {
		let mut history = MoveHistory::default();
		assert!(!history.has_undoable_move());
		assert!(!history.has_redoable_move());
		assert!(history.undo().is_none());
		assert!(history.redo().is_none());
	}

	#[test]
	fn undo_test() {
		let mut history = MoveHistory::default();
		history.push(RotateCycle {
			target_cycle: 0,
			amount: 1,
		});
		history.push(RotateCycle {
			target_cycle: 1,
			amount: 2,
		});
		history.push(RotateCycle {
			target_cycle: 2,
			amount: -4,
		});

		assert!(history.has_undoable_move());
		assert!(!history.has_redoable_move());
		assert!(history.redo().is_none());

		assert_eq!(
			history.undo(),
			Some(RotateCycle {
				target_cycle: 2,
				amount: 4
			})
		);
		assert_eq!(
			history.undo(),
			Some(RotateCycle {
				target_cycle: 1,
				amount: -2
			})
		);
		assert_eq!(
			history.undo(),
			Some(RotateCycle {
				target_cycle: 0,
				amount: -1
			})
		);
		assert_eq!(history.undo(), None);
	}

	#[test]
	fn redo_test() {
		let mut history = MoveHistory::default();
		history.push(RotateCycle {
			target_cycle: 0,
			amount: 1,
		});
		assert!(history.undo().is_some());

		assert!(history.has_redoable_move());
		assert_eq!(
			history.redo(),
			Some(RotateCycle {
				target_cycle: 0,
				amount: 1
			})
		);

		assert!(!history.has_redoable_move());
		assert!(history.has_undoable_move());
		assert_eq!(
			history.undo(),
			Some(RotateCycle {
				target_cycle: 0,
				amount: -1
			})
		);
	}

	#[test]
	fn cut_off_history() {
		let mut history = MoveHistory::default();
		history.push(RotateCycle {
			target_cycle: 0,
			amount: 1,
		});
		history.push(RotateCycle {
			target_cycle: 1,
			amount: 2,
		});
		assert!(history.undo().is_some());
		assert!(history.undo().is_some());

		history.push(RotateCycle {
			target_cycle: 2,
			amount: 1,
		});

		assert!(!history.has_redoable_move());
		assert!(history.redo().is_none());
	}
}
