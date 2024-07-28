use crate::ui::freeze::ui_not_frozen;

use super::{logic::LogicSystemSet, prelude::*};

/// System set that updates [`CycleInteraction`] components
#[derive(SystemSet, Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub struct InputsUpdateSet;

pub(super) fn plugin(app: &mut App) {
	app.add_systems(
		Update,
		(
			cycle_inputs_system.in_set(InputsUpdateSet),
			cycle_rotation_with_inputs_system
				.after(InputsUpdateSet)
				.before(LogicSystemSet),
		)
			.run_if(ui_not_frozen),
	);
}

fn cycle_inputs_system(
	input_mouse: Res<ButtonInput<MouseButton>>,
	input_key: Res<ButtonInput<KeyCode>>,
	window_q: Query<&Window>,
	camera_q: Query<(&Camera, &GlobalTransform)>,
	mut cycles_q: Query<(
		Entity,
		&CycleInterationRadius,
		&GlobalTransform,
		&ComputedCycleTurnability,
		&mut CycleInteraction,
	)>,
) {
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
		.and_then(|p| camera.viewport_to_world_2d(camera_transform, p));
	if let Some(cursor_pos) = cursor_pos {
		let (nearest_cycle, _) = cycles_q
			.iter()
			.filter_map(|(e, radius, transform, turnability, _)| {
				// Include turnability this early in the input handling,
				// because we want to ignore locked cycles in case they overlap
				// a turnable cycle
				if !turnability.0 {
					return None;
				}
				let d_sq = transform.translation().xy().distance_squared(cursor_pos);
				let r_sq = radius.0.powi(2);
				if d_sq <= r_sq {
					Some((e, d_sq))
				} else {
					None
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
	mut events: EventWriter<RotateCycleGroup>,
) {
	for (id, interaction) in &query {
		let direction = match interaction {
			CycleInteraction::LeftClick => CycleTurningDirection::Nominal,
			CycleInteraction::RightClick => CycleTurningDirection::Reverse,
			_ => return,
		};
		events.send(RotateCycleGroup(RotateCycle {
			target_cycle: id,
			direction,
		}));
	}
}
