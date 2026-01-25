//! Recording of tester's moves to the persistent log

use super::log::*;
use crate::{
	game::{logic_relay::RotateCycleGroupWithResult, spawn::EnterLevel},
	screen::{PlayingLevelListEntry, Screen},
};
use bevy::prelude::*;

pub(super) fn plugin(app: &mut App) {
	app.add_systems(
		Update,
		(
			record_level_enter.run_if(on_message::<EnterLevel>),
			record_moves,
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
	let new_log = LevelSessionPlaytestLog::new(log.session_index(), time.elapsed_secs());
	log.level_mut(level_key).sessions.push(new_log);
	Ok(())
}

fn record_moves(
	mut moves: MessageReader<RotateCycleGroupWithResult>,
	mut log: ResMut<PlaytestLog>,
	time: Res<Time>,
	playing_level: PlayingLevelListEntry,
) -> Result {
	let level_key = playing_level.get()?.identifier.clone();
	if let Some(session_log) = log.level_mut(level_key).sessions.last_mut() {
		for action in moves.read() {
			let entry = PlaytestMoveLog {
				time: time.elapsed_secs(),
				succeeded: !action.result.blocked(),
				rotation: action.action,
			};
			session_log.moves.push(entry);
		}
	} else {
		warn!("Cannot log a move because playing level has no logged session");
	}
	Ok(())
}
