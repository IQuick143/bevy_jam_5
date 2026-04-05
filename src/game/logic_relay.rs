//! Reflects the game's logic from [`super::logic`] into ECS

use super::{components::*, logic::TurnCycleResult, prelude::*};
use crate::send_event;

pub fn plugin(app: &mut App) {
	app.init_resource::<LevelCompletionConditions>()
		.init_resource::<GameState>()
		.init_resource::<IsLevelCompleted>()
		.add_systems(
			LevelInitialization,
			(
				|mut is_completed: ResMut<IsLevelCompleted>| is_completed.0 = false,
				|mut completion: ResMut<LevelCompletionConditions>| *completion = default(),
				send_event(GameLayoutChanged),
			),
		)
		.add_observer(cycle_group_rotation_system)
		.add_observer(button_trigger_check_system)
		.add_observer(level_completion_check_system)
		.add_observer(cycle_turnability_update_system);
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

/// Common data for [`RotateCycleGroup`] and the like
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct RotateCycle {
	/// Index of the cycle to rotate
	pub target_cycle: usize,
	/// Index of the group that is rotated
	pub target_group: usize,
	/// How many steps the cycle should rotate by
	pub amount: i64,
}

impl RotateCycle {
	/// Whether two rotations represent equivalent actions
	///
	/// Actions are equivalent if the same cycle group turns
	/// the same number of times, regardless of which cycle triggered it
	pub fn equivalent(&self, other: &Self) -> bool {
		(self.target_group, self.amount) == (other.target_group, other.amount)
	}
}

/// Enumerates the possible causes of a rotation action
#[derive(Clone, Copy, PartialEq, Eq, Debug, Default)]
pub enum RotationCause {
	/// Rotation caused by a user directly by triggering it
	#[default]
	Manual,
	/// Rotation command issued to undo a previous action
	Undo,
	/// Rotation command issued to replay a previously undone action
	Redo,
}

/// [`Event`] sent to rotate a cycle with all cycles linked to it.
/// Should be sent only if it is valid to rotate the given cycle.
#[derive(Event, Clone, Copy, Debug)]
pub struct RotateCycleGroup {
	pub rotation: RotateCycle,
	pub cause: RotationCause,
}

/// Event that is sent when state of the game map changes,
/// usually by turning a cycle
#[derive(Event, Clone, Copy, Default, Debug)]
pub struct GameLayoutChanged;

/// [`Event`] emited after a turn has been handled,
/// including its result
#[derive(Event, Clone, Debug)]
pub struct RotateCycleGroupWithResult {
	pub action: RotateCycleGroup,
	pub result: TurnCycleResult,
}

/// Contains an information whether the level being played has been completed
/// in this session (making moves after completion does not matter)
#[derive(Resource, Clone, Copy, PartialEq, Eq, Debug, Default, Deref, DerefMut)]
pub struct IsLevelCompleted(pub bool);

/// Rotates cycles in game state and sends out events to other systems
fn cycle_group_rotation_system(
	event: On<RotateCycleGroup>,
	mut game_state: ResMut<GameState>,
	mut entity_index: ResMut<GameStateEcsIndex>,
	active_level: PlayingLevelData,
	mut commands: Commands,
) -> Result<(), BevyError> {
	let level = active_level.get()?;
	let target_cycle = event.rotation.target_cycle;
	let rotate_by = event.rotation.amount;
	match game_state.turn_cycle_with_links(level, target_cycle, rotate_by) {
		Err(err) => warn!("Could not turn cycle: {err}"),
		Ok(result) => {
			// TODO: Events?
			if !result.blocked() && result.layout_changed() {
				commands.trigger(GameLayoutChanged);
				result.reorder_sequence_by_all_cycle_turns(level, &mut entity_index.objects)?;
			}
			commands.trigger(RotateCycleGroupWithResult {
				action: *event,
				result,
			});
		}
	}
	Ok(())
}

fn cycle_turnability_update_system(
	_trigger: On<GameLayoutChanged>,
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
	_trigger: On<GameLayoutChanged>,
	mut things_q: Query<&mut IsTriggered>,
	level: PlayingLevelData,
	game_state: Res<GameState>,
	entity_index: If<Res<GameStateEcsIndex>>,
) -> Result<(), BevyError> {
	let level = level.get()?;
	let vertices = entity_index
		.objects
		.iter()
		.zip(&entity_index.glyphs)
		.enumerate();
	for (vertex_index, (object_id, glyph_id)) in vertices {
		let is_triggered = game_state.is_vertex_triggered(level, vertex_index)?;
		for id in [*object_id, *glyph_id].into_iter().flatten() {
			things_q.get_mut(id)?.set_if_neq(IsTriggered(is_triggered));
		}
	}
	Ok(())
}

fn level_completion_check_system(
	_trigger: On<GameLayoutChanged>,
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
