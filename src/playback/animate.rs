//! Execution of playback scripts

use super::script::*;
use crate::{
	AppSet,
	assets::LoadedLevelList,
	game::{
		components::{GameStateEcsIndex, PlayingLevelData},
		inputs::HoveredCycle,
		level::list::LevelList,
		logic_relay::{RotateCycle, RotateCycleGroup, RotationCause},
	},
	screen::{LoadLevel, PlayingLevel, Screen},
	ui::hover::IsHovered,
};
use bevy::prelude::*;
use itertools::Itertools as _;

pub(super) fn plugin(app: &mut App) {
	app.add_systems(
		Update,
		tick_animation
			.run_if(is_playback_running)
			.in_set(AppSet::ExecuteInput),
	);
}

fn tick_animation(
	sequence: Res<PlaybackSequence>,
	mut playback: ResMut<PlaybackState>,
	time: Res<Time>,
	level: PlayingLevelData,
	mut commands: Commands,
	mut hovered_cycle: ResMut<HoveredCycle>,
	mut cycle_q: Query<&mut IsHovered>,
	ecs_index: Option<Res<GameStateEcsIndex>>,
	mut next_screen: ResMut<NextState<Screen>>,
	mut next_level: ResMut<NextState<PlayingLevel>>,
	level_list: Res<LoadedLevelList>,
	level_lists: Res<Assets<LevelList>>,
	mut load_level_messages: MessageWriter<LoadLevel>,
) {
	let Some(current_primitive) = sequence.get(playback.step_index) else {
		// The animation has ended
		playback.stop();
		return;
	};

	match current_primitive {
		PlaybackPrimitive::WaitSeconds(timeout) => {
			playback.seconds_elapsed += time.delta_secs();
			// Keep waiting if the timeout is still running
			if playback.seconds_elapsed < *timeout {
				return;
			}
		}
		&PlaybackPrimitive::TurnCycle {
			target_cycle,
			amount,
		} => {
			if let Ok(level) = level.get()
				&& let Some(cycle_data) = level.cycles.get(target_cycle)
			{
				let rotation = RotateCycle {
					target_cycle,
					target_group: cycle_data.group,
					amount,
				};
				commands.trigger(RotateCycleGroup {
					rotation,
					cause: RotationCause::Playback,
				});
			}
		}
		PlaybackPrimitive::SelectCycle(target_cycle) => {
			if let Some(ecs_index) = ecs_index {
				// Get the target cycle entity
				let target_cycle = target_cycle.and_then(|c| ecs_index.cycles.get(c)).copied();
				// Update state of cycles gaining and losing focus
				if let Some(id) = hovered_cycle.0
					&& let Ok(mut is_hovered) = cycle_q.get_mut(id)
				{
					is_hovered.set_if_neq(IsHovered(false));
				}
				if let Some(id) = target_cycle
					&& let Ok(mut is_hovered) = cycle_q.get_mut(id)
				{
					is_hovered.set_if_neq(IsHovered(true));
				}
				// Update the global marker of hovered cycle
				hovered_cycle.set_if_neq(HoveredCycle(target_cycle));
			} else {
				error!("playback: game object ECS index is not available");
			}
		}
		PlaybackPrimitive::EnterLevel(level_id) => {
			if let Some(level_list) = level_lists.get(&level_list.0) {
				let matched_level = level_list
					.levels
					.iter()
					.find_position(|level| level.identifier == *level_id);
				if let Some((index, _)) = matched_level {
					next_screen.set(Screen::Playing);
					next_level.set(PlayingLevel(Some(index)));
					load_level_messages.write(LoadLevel::default());
				} else {
					error!("playback: level {level_id} not found");
				}
			}
		}
	}

	// Move on to the next action in the next tick
	// Only run one per tick, since they tend to need some time to resolve
	playback.seconds_elapsed = 0.0;
	playback.step_index += 1;
}
