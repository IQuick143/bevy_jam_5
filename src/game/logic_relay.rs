//! Reflects the game's logic from [`super::logic`] into ECS

use super::{components::*, logic::TurnCycleResult, prelude::*};
use crate::{send_event, AppSet};

pub fn plugin(app: &mut App) {
	app.init_resource::<LevelCompletionConditions>()
		.init_resource::<GameState>()
		.init_resource::<IsLevelCompleted>()
		.add_event::<TurnCycleResult>()
		.add_event::<GameLayoutChanged>()
		.add_event::<RotateCycleGroup>()
		.add_event::<RotateSingleCycle>()
		.add_event::<RecordCycleGroupRotation>()
		.add_event::<TurnBlockedByGroupConflict>()
		.add_systems(
			LevelInitialization,
			(
				|mut is_completed: ResMut<IsLevelCompleted>| is_completed.0 = false,
				|mut completion: ResMut<LevelCompletionConditions>| *completion = default(),
				send_event(GameLayoutChanged),
			),
		)
		.add_systems(
			Update,
			(
				cycle_group_rotation_system.run_if(on_event::<RotateCycleGroup>),
				(
					button_trigger_check_system,
					level_completion_check_system,
					cycle_turnability_update_system,
				)
					.run_if(on_event::<GameLayoutChanged>)
					.after(cycle_group_rotation_system),
			)
				.in_set(AppSet::GameLogic),
		);
}

/// Determines whether a cycle may be turned at any given moment
#[derive(Component, PartialEq, Eq, Debug, Clone, Reflect)]
pub struct ComputedCycleTurnability(pub bool);

/// Denotes whether an [`Object`] or [`Glyph`] entity is currently
/// on the same vertex as a matching entity of the other kind.
///
/// A boolean flag is used instead of a marker to enable change detection.
#[derive(Component, Clone, Copy, PartialEq, Eq, Default, Debug, Reflect)]
pub struct IsTriggered(pub bool);

/// Common data for [`RotateSingleCycle`] and [`RotateCycleGroup`]
#[derive(Clone, Copy, Debug)]
pub struct RotateCycle {
	/// Index of the cycle to rotate
	pub target_cycle: usize,
	/// How many steps the cycle should rotate by
	pub amount: i64,
}

/// Internal event sent to a cycle entity to rotate [`super::components::Object`]
/// entities that lie on the cycle, ignores linkages.
///
/// Signals a rotation of a cycle occuring.
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
#[derive(Event, Clone, Copy, Default, Debug)]
pub struct GameLayoutChanged;

/// Event indicating that a pair of groups that cannot be turned together
/// blocked the execution of a turn.
/// Emitted for each pair of conflicting groups.
/// The value indexes into [`LevelData::forbidden_group_pairs`]
#[derive(Event, Clone, Copy, Default, Debug)]
pub struct TurnBlockedByGroupConflict(pub usize);

/// Contains an information whether the level being played has been completed
/// in this session (making moves after completion does not matter)
#[derive(Resource, Clone, Copy, PartialEq, Eq, Debug, Default, Deref, DerefMut)]
pub struct IsLevelCompleted(pub bool);

/// Rotates cycles in game state and sends out events to other systems
fn cycle_group_rotation_system(
	mut group_events: EventReader<RotateCycleGroup>,
	mut single_events: EventWriter<RotateSingleCycle>,
	mut update_event: EventWriter<GameLayoutChanged>,
	mut blocked_event: EventWriter<TurnBlockedByGroupConflict>,
	mut turn_events: EventWriter<TurnCycleResult>,
	mut game_state: ResMut<GameState>,
	active_level: PlayingLevelData,
) -> Result<(), BevyError> {
	let level = active_level.get()?;
	for event in group_events.read() {
		let target_cycle = event.0.target_cycle;
		let rotate_by = event.0.amount;
		match game_state.turn_cycle_with_links(level, target_cycle, rotate_by) {
			Err(err) => warn!("Could not turn cycle: {err}"),
			Ok(result) => {
				for clash in &result.clashes {
					blocked_event.write(TurnBlockedByGroupConflict(*clash));
				}
				if !result.blocked() && result.layout_changed() {
					update_event.write(GameLayoutChanged);
					for (target_cycle, amount) in result.cycles_turned_by(level) {
						single_events.write(RotateSingleCycle(RotateCycle {
							target_cycle,
							amount,
						}));
					}
				}
				turn_events.write(result);
			}
		}
	}
	Ok(())
}

fn cycle_turnability_update_system(
	mut cycles_q: Query<(&Cycle, &mut ComputedCycleTurnability)>,
	level: PlayingLevelData,
	game_state: Res<GameState>,
) {
	let Ok(level) = level.get() else {
		error!("Current playing level data is not available");
		return;
	};

	for (cycle, mut computed_turnability) in &mut cycles_q {
		match game_state.is_cycle_turnable(level, cycle.id) {
			Ok(is_turnable) => {
				computed_turnability.set_if_neq(ComputedCycleTurnability(is_turnable));
			}
			Err(err) => warn!("Error when updating cycle turnability: {err}"),
		}
	}
}

fn button_trigger_check_system(
	mut things_q: Query<&mut IsTriggered>,
	level: PlayingLevelData,
	game_state: Res<GameState>,
	entity_index: Res<GameStateEcsIndex>,
) -> Result<(), BevyError> {
	let level = level.get()?;
	let vertices = game_state
		.trigger_states(level)
		.zip(&game_state.objects_by_vertex)
		.zip(&entity_index.glyphs);
	for ((is_triggered, object_index), glyph_id) in vertices {
		let object_id = object_index
			.and_then(|i| entity_index.objects.get(i))
			.copied();
		for id in [object_id, *glyph_id].into_iter().flatten() {
			match things_q.get_mut(id) {
				Ok(mut is_entity_triggered) => {
					is_entity_triggered.set_if_neq(IsTriggered(is_triggered));
				}
				Err(err) => {
					warn!("Object or glyph referenced by entity index not found in ECS: {err}")
				}
			}
		}
	}
	Ok(())
}

fn level_completion_check_system(
	level: PlayingLevelData,
	game_state: ResMut<GameState>,
	mut completion: ResMut<LevelCompletionConditions>,
	mut is_completed: ResMut<IsLevelCompleted>,
) -> Result<(), BevyError> {
	*completion = game_state.get_completion(level.get()?);
	if completion.is_level_completed() {
		// This stays true until a different level is loaded
		is_completed.set_if_neq(IsLevelCompleted(true));
	}
	Ok(())
}
