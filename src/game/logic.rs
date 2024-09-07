use super::{components::*, level::*, prelude::*};
use crate::AppSet;

pub fn plugin(app: &mut App) {
	app.init_resource::<LevelCompletionConditions>()
		.init_resource::<IsLevelCompleted>()
		.add_event::<GameLayoutChanged>()
		.add_event::<RotateCycleGroup>()
		.add_event::<RotateSingleCycle>()
		.add_event::<RecordCycleGroupRotation>()
		.add_systems(
			Update,
			(
				cycle_group_rotation_relay_system.run_if(on_event::<RotateCycleGroup>()),
				cycle_rotation_system.run_if(on_event::<RotateSingleCycle>()),
				(
					level_completion_check_system,
					cycle_turnability_update_system,
				)
					.run_if(on_event::<GameLayoutChanged>()),
			)
				.chain()
				.in_set(AppSet::GameLogic),
		);
}

/// Determines whether a cycle may be turned at any given moment
#[derive(Component, Debug, Clone, Reflect)]
pub struct ComputedCycleTurnability(pub bool);

/// Enumerates directions in which a cycle can turn
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum CycleTurningDirection {
	/// Rotate in nominal direction of the cycle
	Nominal,
	/// Rotate in reverse direction of the cycle
	Reverse,
}

/// Common data for [`RotateSingleCycle`] and [`RotateCycleGroup`]
#[derive(Clone, Copy, Debug)]
pub struct RotateCycle {
	/// Id of the cycle entity to rotate
	pub target_cycle: Entity,
	/// Direction in which the cycle should turn
	pub direction: CycleTurningDirection,
}

/// Internal event sent to a cycle entity to rotate [`super::components::Object`]
/// entities that lie on the cycle, ignores linkages.
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
#[derive(Event, Debug)]
pub struct GameLayoutChanged;

/// Contains an overview of conditions that are needed to complete the level
#[derive(Resource, Debug, Clone, Copy, Reflect, Default)]
pub struct LevelCompletionConditions {
	pub buttons_present: u32,
	pub buttons_triggered: u32,
	pub flags_present: u32,
	pub flags_occupied: u32,
}

impl LevelCompletionConditions {
	/// Whether the level has been completed
	pub fn is_level_completed(&self) -> bool {
		self.is_goal_unlocked() && self.flags_occupied == self.flags_present
	}

	/// Whether all secondary completion criteria have been met,
	/// and the level will be completed as soon as all players travel to a goal
	pub fn is_goal_unlocked(&self) -> bool {
		self.buttons_present == self.buttons_triggered
	}
}

/// Contains an information whether the level being played has been completed
/// in this session (making moves after completion does not matter)
#[derive(Resource, Clone, Copy, PartialEq, Eq, Debug, Default)]
pub struct IsLevelCompleted(pub bool);

/// Relays rotation events on a cycle group to the individual cycles
fn cycle_group_rotation_relay_system(
	mut group_events: EventReader<RotateCycleGroup>,
	mut single_events: EventWriter<RotateSingleCycle>,
	mut update_event: EventWriter<GameLayoutChanged>,
	cycles_q: Query<&LinkedCycles>,
) {
	for group_rotation in group_events.read() {
		// We assume that the RotateCycleGroup event always targets a valid target and a rotation happens.
		update_event.send(GameLayoutChanged);
		single_events.send_batch(
			cycles_q
				.get(group_rotation.0.target_cycle)
				.into_iter()
				.flat_map(|links| &links.0)
				.map(|&(id, relative_direction)| RotateCycle {
					target_cycle: id,
					direction: group_rotation.0.direction * relative_direction,
				})
				.map(RotateSingleCycle),
		);
	}
}

fn cycle_rotation_system(
	cycles_q: Query<&CycleVertices>,
	mut events: EventReader<RotateSingleCycle>,
	mut vertices_q: Query<&mut PlacedObject>,
	mut objects_q: Query<&mut VertexPosition>,
) {
	for event in events.read() {
		if let Ok(cycle_vertices) = cycles_q
			.get(event.0.target_cycle)
			.inspect_err(|e| log::warn!("{e}"))
		{
			let vertex_ids_wrapped =
				get_flipped_wrapped_iterator(&cycle_vertices.0, event.0.direction);
			let mut cached_object_id = None;
			for vertex_id in vertex_ids_wrapped {
				if let Some(cached_object_id) = cached_object_id {
					if let Ok(mut object_pos) = objects_q
						.get_mut(cached_object_id)
						.inspect_err(|e| log::warn!("{e}"))
					{
						object_pos.0 = vertex_id;
					}
				}
				if let Ok(mut placed_object_id) = vertices_q
					.get_mut(vertex_id)
					.inspect_err(|e| log::warn!("{e}"))
				{
					std::mem::swap(&mut cached_object_id, &mut placed_object_id.0);
				}
			}
		}
	}
}

/// Returns an iterator which wraps the first element to the end and also flips the direction based on `direction`
pub fn get_flipped_wrapped_iterator<T: Copy>(
	array: &[T],
	direction: CycleTurningDirection,
) -> impl Iterator<Item = T> + '_ {
	// Visit the first vertex again at the end to close the loop
	let vertex_ids_wrapped = array.iter().copied().chain(std::iter::once(array[0]));
	// Messy but simple way of chosing the iterator direction at run time
	// https://stackoverflow.com/a/52064434
	match direction {
		CycleTurningDirection::Nominal => Some(vertex_ids_wrapped)
			.into_iter()
			.flatten()
			.chain(None.into_iter().flatten()),
		CycleTurningDirection::Reverse => None
			.into_iter()
			.flatten()
			.chain(Some(vertex_ids_wrapped.rev()).into_iter().flatten()),
	}
}

fn cycle_turnability_update_system(
	vertices_q: Query<&PlacedObject>,
	players_q: Query<(), With<Player>>,
	mut cycles_q: Query<(
		&CycleVertices,
		&CycleTurnability,
		&mut ComputedCycleTurnability,
	)>,
) {
	'next_cycle: for (vertex_ids, turnability, mut computed_turnability) in &mut cycles_q {
		match *turnability {
			CycleTurnability::Always => computed_turnability.0 = true,
			CycleTurnability::Never => computed_turnability.0 = false,
			CycleTurnability::WithPlayer => {
				for &vertex_id in &vertex_ids.0 {
					let player_is_present = vertices_q
						.get(vertex_id)
						.inspect_err(|e| log::warn!("{e}"))
						.ok()
						.and_then(|object_id| object_id.0.and_then(|id| players_q.get(id).ok()))
						.is_some();
					if player_is_present {
						computed_turnability.0 = true;
						continue 'next_cycle;
					}
				}
				computed_turnability.0 = false;
			}
		}
	}
}

fn level_completion_check_system(
	vertices_q: Query<(&PlacedObject, &PlacedGlyph)>,
	objects_q: Query<(Option<&Player>, Option<&Box>, Option<&LogicalColor>), With<Object>>,
	glyphs_q: Query<(Option<&Goal>, Option<&BoxSlot>, Option<&LogicalColor>), With<Glyph>>,
	mut completion: ResMut<LevelCompletionConditions>,
	mut is_completed: ResMut<IsLevelCompleted>,
) {
	let mut new_completion = LevelCompletionConditions {
		buttons_present: 0,
		buttons_triggered: 0,
		flags_present: 0,
		flags_occupied: 0,
	};

	for (object, glyph) in &vertices_q {
		let (contains_player, contains_box, object_color) = object
			.0
			.and_then(|id| objects_q.get(id).inspect_err(|e| log::warn!("{e}")).ok())
			.map(|(a, b, c)| (a.is_some(), b.is_some(), c))
			.unwrap_or((false, false, None));
		let (contains_goal, contains_button, glyph_color) = glyph
			.0
			.and_then(|id| glyphs_q.get(id).inspect_err(|e| log::warn!("{e}")).ok())
			.map(|(a, b, c)| (a.is_some(), b.is_some(), c))
			.unwrap_or((false, false, None));

		let colors_compatible = object_color
			.and_then(|object_color| glyph_color.map(|glyph_color| *object_color == *glyph_color))
			// If either thing is colorless, they are considered compatible
			.unwrap_or(true);

		if contains_button {
			new_completion.buttons_present += 1;
			if contains_box && colors_compatible {
				new_completion.buttons_triggered += 1;
			}
		}
		if contains_goal {
			new_completion.flags_present += 1;
			if contains_player && colors_compatible {
				new_completion.flags_occupied += 1;
			}
		}
	}

	if new_completion.is_level_completed() {
		// This stays true until a different level is loaded
		is_completed.set_if_neq(IsLevelCompleted(true));
	}

	*completion = new_completion;
}

impl std::ops::Mul<LinkedCycleDirection> for CycleTurningDirection {
	type Output = Self;
	fn mul(self, rhs: LinkedCycleDirection) -> Self::Output {
		match rhs {
			LinkedCycleDirection::Coincident => self,
			LinkedCycleDirection::Inverse => -self,
		}
	}
}

impl std::ops::Neg for CycleTurningDirection {
	type Output = Self;
	fn neg(self) -> Self::Output {
		match self {
			CycleTurningDirection::Nominal => CycleTurningDirection::Reverse,
			CycleTurningDirection::Reverse => CycleTurningDirection::Nominal,
		}
	}
}
