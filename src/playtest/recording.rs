//! Recording of tester's moves to the persistent log

use super::log::*;
use crate::{
	game::{logic_relay::*, spawn::EnterLevel},
	screen::{PlayingLevelListEntry, Screen},
};
use bevy::prelude::*;

pub(super) fn plugin(app: &mut App) {
	app.add_observer(record_moves).add_systems(
		Update,
		(
			record_level_enter.run_if(on_message::<EnterLevel>),
			save_level_completion.run_if(
				resource_changed::<IsLevelCompleted>.and(resource_equals(IsLevelCompleted(true))),
			),
		)
			.run_if(in_state(Screen::Playing)),
	);
}

fn record_level_enter(
	mut log: ResMut<PlaytestLog>,
	time: Res<Time>,
	playing_level: PlayingLevelListEntry,
) -> Result {
	let level_key = playing_level.get()?.identifier.clone();
	let elapsed = (time.elapsed_secs() * 100.0) as u32;
	let new_log = LevelSessionPlaytestLog::new(log.session_index(), elapsed);
	log.level_mut(level_key).sessions.push(new_log);
	Ok(())
}

fn record_moves(
	action: On<RotateCycleGroupWithResult>,
	mut log: ResMut<PlaytestLog>,
	time: Res<Time>,
	playing_level: PlayingLevelListEntry,
) -> Result {
	let level_key = playing_level.get()?.identifier.clone();
	let elapsed = (time.elapsed_secs() * 100.0) as u32;
	if let Some(session_log) = log.level_mut(level_key).sessions.last_mut() {
		let entry = PlaytestMoveLog {
			time: elapsed,
			turn: match action.action.cause {
				RotationCause::Undo => PlaytestMove::Undo,
				RotationCause::Redo => PlaytestMove::Redo,
				RotationCause::Manual => PlaytestMove::Manual {
					target_cycle: action.action.rotation.target_cycle,
					// Amount is going to be small for user inputs
					amount: action.action.rotation.amount as i32,
				},
			},
		};
		session_log.moves.push(entry);
	} else {
		warn!("Cannot log a move because playing level has no logged session");
	}
	Ok(())
}

fn save_level_completion(
	mut playtest: ResMut<PlaytestLog>,
	playing_level: PlayingLevelListEntry,
) -> Result {
	let level_id = playing_level.get()?.identifier.clone();
	playtest.level_mut(level_id).completed = true;
	Ok(())
}
