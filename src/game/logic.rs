use super::{components::*, level::*, prelude::*};
use crate::{send_event, AppSet};

pub fn plugin(app: &mut App) {
	app.init_resource::<LevelCompletionConditions>()
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
				cycle_group_rotation_relay_system.run_if(on_event::<RotateCycleGroup>),
				cycle_rotation_system.run_if(on_event::<RotateSingleCycle>),
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
	mut blocked_event: EventWriter<TurnBlockedByGroupConflict>,
	cycles_q: Query<(&Cycle, &CycleVertices)>,
	vertex_q: Query<(&Vertex, &PlacedObject)>,
	cycle_index: Res<CycleEntities>,
	level_asset: Res<Assets<LevelData>>,
	level_handle: Res<LevelHandle>,
) {
	let Some(level) = level_asset.get(&level_handle.0) else {
		log::error!("Non-existent level asset being referenced.");
		return;
	};
	let mut detector_rotations = vec![0i64; level.detectors.len()];
	let mut group_rotations = vec![0i64; level.groups.len()];
	for group_rotation in group_events.read() {
		let Ok((source_cycle, _)) = cycles_q.get(group_rotation.0.target_cycle) else {
			continue;
		};
		// We assume that the RotateCycleGroup event always targets a valid target and a rotation happens.
		// Queue the rotation
		match group_rotation.0.direction * source_cycle.orientation_within_group {
			CycleTurningDirection::Nominal => {
				group_rotations[source_cycle.group_id] += group_rotation.0.amount as i64
			}
			CycleTurningDirection::Reverse => {
				group_rotations[source_cycle.group_id] -= group_rotation.0.amount as i64
			}
		}
	}

	// Propagate one-way links
	for step in level.execution_order.iter().copied() {
		match step {
			DetectorOrGroup::Group(group_id) => {
				if group_rotations[group_id] == 0 {
					continue;
				}
				for link in level.groups[group_id].linked_groups.iter() {
					group_rotations[link.target_group] +=
						link.direction * group_rotations[group_id] * link.multiplicity as i64
				}
				for &detector_cycle_id in level.groups[group_id].outgoing_detector_cycles.iter() {
					// Gather data
					let detector_cycle = level.cycles.get(detector_cycle_id).unwrap();
					let n_vertices = detector_cycle.vertex_indices.len();
					let n_detectors = detector_cycle.detector_indices.len();
					if n_vertices == 0 || n_detectors == 0 {
						#[cfg(any(debug_assertions, test))]
						unreachable!("Cycle with no vertices or no detectors is somehow in the `outgoing_detector_cycles` list.");
						#[cfg(not(any(debug_assertions, test)))]
						{
							warn!("Cycle with no vertices or no detectors is somehow in the `outgoing_detector_cycles` list.");
							continue;
						}
					}
					// Grab vertex occupancy data from ECS
					let Some((_, cycle_vertices)) = cycle_index
						.0
						.get(detector_cycle_id)
						.and_then(|entity| cycles_q.get(*entity).ok())
					else {
						#[cfg(any(debug_assertions, test))]
						unreachable!("Nonexistent cycle!!");
						#[cfg(not(any(debug_assertions, test)))]
						{
							error!(
								"Cycle {} missing in ECS but referenced in logic!",
								detector_cycle_id
							);
							return;
						}
					};
					if n_vertices != cycle_vertices.0.len() {
						#[cfg(any(debug_assertions, test))]
						panic!("Incorrect amount of vertices on a cycle.");
						#[cfg(not(any(debug_assertions, test)))]
						{
							error!("Incorrect amount of vertices on a cycle.");
							return;
						}
					}
					let vertex_occupation: Vec<bool> = (0..n_vertices)
						.map(|index| {
							let Ok((_, occupancy)) = vertex_q.get(cycle_vertices.0[index]) else {
								#[cfg(any(debug_assertions, test))]
								unreachable!("Nonexistent vertex!!");
								#[cfg(not(any(debug_assertions, test)))]
								{
									error!("Nonexistent vertex!!");
									return;
								}
							};
							occupancy.0.is_some()
						})
						.collect();
					let n_objects = vertex_occupation.iter().filter(|x| **x).count();
					// Nothing to detect
					if n_objects == 0 {
						continue;
					}
					// Compute rotation characteristics
					let n_rotations =
						detector_cycle.orientation_within_group * group_rotations[group_id];
					let full_turns = n_rotations.signum()
						* i64::div_euclid(n_rotations.abs(), n_vertices as i64);
					let partial_turns = (n_rotations.signum()
						* i64::rem_euclid(n_rotations.abs(), n_vertices as i64))
						as isize;
					for (detector_id, _) in detector_cycle.detector_indices.iter() {
						detector_rotations[*detector_id] += (n_objects as i64) * full_turns;
					}
					if partial_turns != 0 {
						// How long of a strip of vertices needs to be scanned for objects
						let interval_length = partial_turns.unsigned_abs();
						// Counts how many objects are in a interval <i - interval_length, i) (accounting for looping)
						let mut objects_in_interval = vec![0; n_vertices];
						let mut running_total: i32 = 0;
						#[allow(clippy::needless_range_loop)]
						for i in (n_vertices - interval_length)..n_vertices {
							if vertex_occupation[i] {
								running_total += 1;
							}
						}
						for i in 0..n_vertices {
							// Write the value
							objects_in_interval[i] = running_total;
							// Move the interval
							let back_index = (n_vertices - interval_length + i) % n_vertices;
							if vertex_occupation[back_index] {
								running_total -= 1;
							}
							if vertex_occupation[i] {
								running_total += 1;
							}
						}
						for &(detector_id, offset) in detector_cycle.detector_indices.iter() {
							let detections = if partial_turns > 0 {
								objects_in_interval[(offset + 1) % n_vertices]
							} else {
								-objects_in_interval[(offset + 1 + interval_length) % n_vertices]
							} as i64;
							detector_rotations[detector_id] += detections;
						}
					}
				}
			}
			DetectorOrGroup::Detector(detector_id) => {
				if detector_rotations[detector_id] == 0 {
					continue;
				}
				for link in level.detectors[detector_id].linked_groups.iter() {
					match link.direction {
						LinkedCycleDirection::Coincident => {
							group_rotations[link.target_group] +=
								detector_rotations[detector_id] * link.multiplicity as i64
						}
						LinkedCycleDirection::Inverse => {
							group_rotations[link.target_group] -=
								detector_rotations[detector_id] * link.multiplicity as i64
						}
					}
				}
			}
		}
	}

	// Decide if the turns is valid
	let forbidden = {
		// TODO: Change this code, so that it takes into account the modulo rotations to enable certain mechanisms, Unless this is decided against.
		let mut forbidden = false;
		let mut pair_index = 0;
		for group_a_id in 0..level.groups.len() {
			if group_rotations[group_a_id] != 0 {
				while pair_index < level.forbidden_group_pairs.len() {
					let (a, b, _) = level.forbidden_group_pairs[pair_index];
					if a > group_a_id {
						break;
					}
					if a == group_a_id && group_rotations[b] != 0 {
						forbidden = true;
						blocked_event.write(TurnBlockedByGroupConflict(pair_index));
					}
					pair_index += 1;
				}
			}
		}
		forbidden
	};

	if forbidden {
		// TODO: Emit an event informing other systems the rotation hadn't went through.
		return;
	}

	// Apply rotations
	for (group_data, &rotation) in level.groups.iter().zip(group_rotations.iter()) {
		if rotation == 0 {
			continue;
		}
		let direction = if rotation > 0 {
			CycleTurningDirection::Nominal
		} else {
			CycleTurningDirection::Reverse
		};
		update_event.write(GameLayoutChanged);
		single_events.write_batch(
			group_data
				.cycles
				.iter()
				.map(|&(id, relative_direction)| {
					// println!("Sending {}", id);
					RotateCycle {
						target_cycle: cycle_index.0[id],
						direction: direction * relative_direction,
						amount: rotation.unsigned_abs() as usize,
					}
				})
				.map(RotateSingleCycle),
		);
	}
}

/// System that carries out the rotations on cycles hit by an event queueing a rotation.
fn cycle_rotation_system(
	cycles_q: Query<&CycleVertices>,
	mut events: EventReader<RotateSingleCycle>,
	mut vertices_q: Query<&mut PlacedObject>,
	mut objects_q: Query<&mut VertexPosition>,
) {
	for event in events.read() {
		if event.0.amount == 0 {
			log::warn!("0 rotation was emitted.");
			continue;
		}
		if let Ok(cycle_vertices) = cycles_q
			.get(event.0.target_cycle)
			.inspect_err(|e| log::warn!("{e}"))
		{
			let n_vertices = cycle_vertices.0.len();
			if n_vertices == 0 {
				// Nothing to rotate
				continue;
			}
			let amount = {
				let step = event.0.amount % n_vertices;
				if step == 0 {
					// NOOP, we are done
					continue;
				}
				match event.0.direction {
					CycleTurningDirection::Nominal => step,
					CycleTurningDirection::Reverse => n_vertices - step,
				}
			};
			fn gcd(mut a: usize, mut b: usize) -> usize {
				while a > 0 {
					(a, b) = (b % a, a);
				}
				b
			}
			let n_loops = gcd(amount, n_vertices);
			let loop_len = n_vertices / n_loops;
			for loop_id in 0..n_loops {
				let mut cached_object_id = None;
				for i in 0..loop_len + 1 {
					let index = (loop_id + amount * i) % n_vertices;
					let vertex_id = cycle_vertices.0[index];
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
}

/// Returns an iterator which wraps the first element to the end and also flips the direction based on `direction`
pub fn get_flipped_wrapped_iterator<T: Copy>(
	array: &[T],
	direction: CycleTurningDirection,
) -> impl Iterator<Item = T> + '_ {
	// Visit the first vertex again at the end to close the loop
	// Leave it empty if the input is empty
	let vertex_ids_wrapped = array
		.iter()
		.copied()
		.chain(array.first().into_iter().copied());
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
	flags_q: Query<&IsTriggered, With<Goal>>,
	buttons_q: Query<&IsTriggered, With<SokoButton>>,
	mut completion: ResMut<LevelCompletionConditions>,
	mut is_completed: ResMut<IsLevelCompleted>,
) {
	let mut new_completion = LevelCompletionConditions {
		buttons_present: 0,
		buttons_triggered: 0,
		flags_present: 0,
		flags_occupied: 0,
	};

	for is_triggered in &flags_q {
		new_completion.flags_present += 1;
		if is_triggered.0 {
			new_completion.flags_occupied += 1;
		}
	}
	for is_triggered in &buttons_q {
		new_completion.buttons_present += 1;
		if is_triggered.0 {
			new_completion.buttons_triggered += 1;
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
