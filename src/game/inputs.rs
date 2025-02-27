use crate::{ui::freeze::ui_not_frozen, AppSet};

use super::{
	level::{CyclePlacement, CycleShape},
	logic::*,
	prelude::*,
};

pub(super) fn plugin(app: &mut App) {
	app.add_systems(
		Update,
		(
			cycle_inputs_system.in_set(AppSet::RecordInput),
			cycle_rotation_with_inputs_system.in_set(AppSet::ExecuteInput),
		)
			.run_if(ui_not_frozen),
	);
}

#[derive(Component, Clone, Copy, PartialEq, Eq, Debug, Reflect, Default)]
pub enum CycleInteraction {
	#[default]
	None,
	Hover,
	LeftClick,
	RightClick,
}

fn cycle_inputs_system(
	input_mouse: Res<ButtonInput<MouseButton>>,
	input_key: Res<ButtonInput<KeyCode>>,
	window_q: Query<&Window>,
	camera_q: Query<(&Camera, &GlobalTransform)>,
	mut cycles_q: Query<(
		Entity,
		&CyclePlacement,
		&GlobalTransform,
		&ComputedCycleTurnability,
		&mut CycleInteraction,
	)>,
) {
	// This system may get called when exiting the app, after these entities
	// have been despawned, we do not want to crash in that case
	if window_q.is_empty() || camera_q.is_empty() {
		return;
	}

	let lmb = input_mouse.just_pressed(MouseButton::Left) || input_key.just_pressed(KeyCode::KeyD);
	let rmb = input_mouse.just_pressed(MouseButton::Right) || input_key.just_pressed(KeyCode::KeyA);
	let new_interaction = match (lmb, rmb) {
		(true, true) => CycleInteraction::Hover,
		(true, false) => CycleInteraction::LeftClick,
		(false, true) => CycleInteraction::RightClick,
		(false, false) => CycleInteraction::Hover,
	};
	let window = window_q.single();
	let (camera, camera_transform) = camera_q.single();
	let cursor_pos = window
		.cursor_position()
		.and_then(|p| camera.viewport_to_world_2d(camera_transform, p).ok());
	if let Some(cursor_pos) = cursor_pos {
		let (nearest_cycle, _) = cycles_q
			.iter()
			.filter_map(|(e, placement, transform, turnability, _)| {
				// Include turnability this early in the input handling,
				// because we want to ignore locked cycles in case they overlap
				// a turnable cycle
				if !turnability.0 {
					return None;
				}
				let d_sq = transform.translation().xy().distance_squared(cursor_pos);

				match placement.shape {
					CycleShape::Circle(radius) => {
						if d_sq <= radius.powi(2) {
							Some((e, d_sq))
						} else {
							None
						}
					}
				}
			})
			// Cannot just call min, because IEEE754
			.fold((None, f32::INFINITY), |(e1, d_sq_1), (e2, d_sq_2)| {
				if d_sq_1 > d_sq_2 {
					(Some(e2), d_sq_2)
				} else {
					(e1, d_sq_1)
				}
			});
		// Now that we have found the cycle to interact with, commit all of them
		// Use checked assignment, we do not want to flood the schedules
		// that filter by Changed<CycleInteraction>
		for (e, _, _, _, mut interaction) in &mut cycles_q {
			if nearest_cycle == Some(e) {
				interaction.set_if_neq(new_interaction);
			} else {
				interaction.set_if_neq(CycleInteraction::None);
			}
		}
	}
}

fn cycle_rotation_with_inputs_system(
	query: Query<(Entity, &CycleInteraction), Changed<CycleInteraction>>,
	mut rot_events: EventWriter<RotateCycleGroup>,
	mut record_events: EventWriter<RecordCycleGroupRotation>,
) {
	for (id, interaction) in &query {
		let direction = match interaction {
			CycleInteraction::LeftClick => CycleTurningDirection::Nominal,
			CycleInteraction::RightClick => CycleTurningDirection::Reverse,
			_ => return,
		};
		let rotation = RotateCycle {
			target_cycle: id,
			direction,
			amount: 1,
		};
		rot_events.send(RotateCycleGroup(rotation));
		record_events.send(RecordCycleGroupRotation(rotation));
	}
}
