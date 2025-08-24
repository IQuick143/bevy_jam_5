//! Reflects the game's logic from [`super::logic`] into ECS

use super::{components::*, level::*, prelude::*};
use crate::{send_event, AppSet};

pub fn plugin(app: &mut App) {
	app.init_resource::<LevelCompletionConditions>()
		.init_resource::<GameState>()
		.init_resource::<IsLevelCompleted>()
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
				update_vertex_and_object_relations_in_ecs.run_if(on_event::<RotateSingleCycle>),
				(
					(button_trigger_check_system, level_completion_check_system).chain(),
					cycle_turnability_update_system,
				)
					.run_if(on_event::<GameLayoutChanged>),
			)
				.chain()
				.in_set(AppSet::GameLogic),
		);
}

/// Determines whether a cycle may be turned at any given moment
#[derive(Component, Debug, Clone, Reflect)]
pub struct ComputedCycleTurnability(pub bool);

/// Denotes whether an [`Object`] or [`Glyph`] entity is currently
/// on the same vertex as a matching entity of the other kind.
///
/// A boolean flag is used instead of a marker to enable change detection.
#[derive(Component, Clone, Copy, PartialEq, Eq, Default, Debug, Reflect)]
pub struct IsTriggered(pub bool);

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
	/// How many steps the cycle should rotate by
	pub amount: usize,
}

impl RotateCycle {
	pub fn from_flat_amount(target_cycle: Entity, amount: i64) -> Self {
		Self {
			target_cycle,
			direction: if amount > 0 {
				CycleTurningDirection::Nominal
			} else {
				CycleTurningDirection::Reverse
			},
			amount: amount.unsigned_abs() as usize,
		}
	}

	pub fn flat_amount(&self) -> i64 {
		let amount = self.amount as i64;
		match self.direction {
			CycleTurningDirection::Nominal => amount,
			CycleTurningDirection::Reverse => -amount,
		}
	}
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
	mut game_state: ResMut<GameState>,
	cycles_q: Query<&Cycle>,
	entity_index: Res<GameStateEcsIndex>,
	active_level: PlayingLevelData,
) -> Result<(), BevyError> {
	let level = active_level.get()?;
	for event in group_events.read() {
		let Ok(target_cycle) = cycles_q.get(event.0.target_cycle) else {
			warn!("Cycle referenced by rotation event not found in ECS");
			continue;
		};
		let rotate_by = event.0.flat_amount();
		match game_state.turn_cycle_with_links(level, target_cycle.id, rotate_by) {
			Err(err) => warn!("Could not turn cycle: {err}"),
			Ok(result) => {
				for clash in &result.clashes {
					blocked_event.write(TurnBlockedByGroupConflict(*clash));
				}
				if !result.blocked() && result.layout_changed() {
					update_event.write(GameLayoutChanged);
					for (cycle, amount) in result.cycles_turned_by(level) {
						let Some(cycle_id) = entity_index.cycles.get(cycle) else {
							warn!("Cycle referenced by rotation event not found in ECS");
							continue;
						};
						single_events.write(RotateSingleCycle(RotateCycle::from_flat_amount(
							*cycle_id, amount,
						)));
					}
				}
			}
		}
	}
	Ok(())
}

/// System that carries out the rotations on cycles hit by an event queueing a rotation.
fn update_vertex_and_object_relations_in_ecs(
	mut vertices_q: Query<&mut PlacedObject>,
	mut objects_q: Query<&mut VertexPosition>,
	entity_index: Res<GameStateEcsIndex>,
	game_state: Res<GameState>,
) {
	for (object_index, vertex_id) in game_state
		.objects_by_vertex
		.iter()
		.zip(&entity_index.vertices)
	{
		let Ok(mut vertex_object_ref) = vertices_q.get_mut(*vertex_id) else {
			warn!("Vertex referenced by game state not found in ECS");
			continue;
		};
		if let Some(object_index) = object_index {
			let Some(object_id) = entity_index.objects.get(*object_index) else {
				warn!("Mismatched GameStateEcsIndex and GameState");
				continue;
			};
			let Ok(mut object_vertex_ref) = objects_q.get_mut(*object_id) else {
				warn!("Object referenced by game state not found in ECS");
				continue;
			};
			vertex_object_ref.set_if_neq(PlacedObject(Some(*object_id)));
			object_vertex_ref.set_if_neq(VertexPosition(*vertex_id));
		} else {
			vertex_object_ref.set_if_neq(PlacedObject(None));
		}
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

fn button_trigger_check_system(
	vertices_q: Query<(&PlacedObject, &PlacedGlyph)>,
	mut objects_q: Query<(&mut IsTriggered, &ObjectData)>,
	mut glyphs_q: Query<(&mut IsTriggered, &GlyphData), Without<ObjectData>>,
) {
	for (object, glyph) in &vertices_q {
		let mut object = object.0.and_then(|id| {
			objects_q
				.get_mut(id)
				.inspect_err(|e| log::warn!("{e}"))
				.ok()
		});
		let mut glyph = glyph
			.0
			.and_then(|id| glyphs_q.get_mut(id).inspect_err(|e| log::warn!("{e}")).ok());

		match (&mut object, &mut glyph) {
			(
				Some((ref mut object_triggered, ObjectData::Player)),
				Some((ref mut glyph_triggered, GlyphData::Flag)),
			) => {
				object_triggered.set_if_neq(IsTriggered(true));
				glyph_triggered.set_if_neq(IsTriggered(true));
			}
			(
				Some((ref mut object_triggered, ObjectData::Box(box_color))),
				Some((ref mut glyph_triggered, GlyphData::Button(button_color))),
			) => {
				let colors_compatible = (*box_color)
					.and_then(|box_color| {
						button_color.map(|button_color| box_color == button_color.0)
					})
					// If either thing is colorless, they are considered compatible
					.unwrap_or(true);
				object_triggered.set_if_neq(IsTriggered(colors_compatible));
				glyph_triggered.set_if_neq(IsTriggered(colors_compatible));
			}
			_ => {
				if let Some((mut is_triggered, _)) = object {
					is_triggered.set_if_neq(IsTriggered(false));
				}
				if let Some((mut is_triggered, _)) = glyph {
					is_triggered.set_if_neq(IsTriggered(false));
				}
			}
		};
	}
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
