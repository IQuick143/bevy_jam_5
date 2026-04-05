use super::{
	level::{CyclePlacement, CycleShape},
	logic_relay::*,
	prelude::*,
};
use crate::{
	AppSet,
	camera::CameraHarness,
	game::components::Cycle,
	graphics::RING_SHADOW_BLEED,
	input::*,
	ui::{freeze::ui_not_frozen, hover::IsHovered},
};

pub(super) fn plugin(app: &mut App) {
	app.init_resource::<HoveredCycle>()
		.add_systems(ProcessInputs, cycle_inputs_system.run_if(ui_not_frozen))
		.add_systems(
			Update,
			cycle_hover_system
				.in_set(AppSet::RecordInput)
				.run_if(ui_not_frozen),
		);
}

#[derive(Resource, Default, Clone, Copy, PartialEq, Eq)]
pub struct HoveredCycle(pub Option<Entity>);

fn cycle_hover_system(
	mut hovered_cycle: ResMut<HoveredCycle>,
	window: Single<&Window>,
	camera: Single<(&Camera, &GlobalTransform), With<CameraHarness>>,
	mut cycles_q: Query<(
		Entity,
		&CyclePlacement,
		&GlobalTransform,
		&ComputedCycleTurnability,
		&mut IsHovered,
	)>,
) {
	let (camera, camera_transform) = *camera;
	let cursor_pos = window
		.cursor_position()
		.and_then(|p| camera.viewport_to_world_2d(camera_transform, p).ok());
	let Some(cursor_pos) = cursor_pos else {
		return;
	};

	let (nearest_cycle, _, _, _) = cycles_q
		.iter()
		.filter_map(|(e, placement, transform, turnability, _)| {
			let is_turnable = turnability.0;
			let d_sq = transform.translation().xy().distance_squared(cursor_pos);

			match placement.shape {
				CycleShape::Circle(radius) => {
					if d_sq <= (radius + RING_SHADOW_BLEED).powi(2) {
						Some((e, d_sq, radius.powi(2), is_turnable))
					} else {
						None
					}
				}
			}
		})
		// Cannot just call min, because IEEE754
		.fold(
			(None, f32::INFINITY, -1.0, false),
			|(e1, d_sq_1, r_sq_1, t1), (e2, d_sq_2, r_sq_2, t2)| {
				// Sort by turnability first, because we want to select a turnable
				// cycle if at all possible, even if it is overlapped by locked ones
				// that would otherwise be a better match
				if (!t1, r_sq_1 < d_sq_1, r_sq_1, d_sq_1) > (!t2, r_sq_2 < d_sq_2, r_sq_2, d_sq_2) {
					(Some(e2), d_sq_2, r_sq_2, t2)
				} else {
					(e1, d_sq_1, r_sq_1, t1)
				}
			},
		);

	hovered_cycle.set_if_neq(HoveredCycle(nearest_cycle));

	// Now that we have found the cycle to interact with, commit all of them
	// Use checked assignment, we do not want to flood the systems
	// that filter by `Changed<IsHovered>`
	for (e, _, _, _, mut is_hovered) in &mut cycles_q {
		if nearest_cycle == Some(e) {
			is_hovered.set_if_neq(IsHovered(true));
		} else {
			is_hovered.set_if_neq(IsHovered(false));
		}
	}
}

fn cycle_inputs_system(
	input: Res<CurrentAction>,
	hovered_cycle: Res<HoveredCycle>,
	cycles_q: Query<&Cycle>,
	mut commands: Commands,
) {
	let HoveredCycle(Some(hovered_cycle)) = *hovered_cycle else {
		return;
	};
	let CurrentAction(Some(InputAction::Turn(turn_direction))) = *input else {
		return;
	};
	let turn_direction = turn_direction.clamp(-1, 1);

	if turn_direction == 0 {
		return;
	}

	if let Ok(cycle) = &cycles_q.get(hovered_cycle) {
		let rotation = RotateCycle {
			target_cycle: cycle.id,
			target_group: cycle.group_id,
			amount: turn_direction as i64,
		};
		commands.trigger(RotateCycleGroup {
			rotation,
			cause: RotationCause::Manual,
		});
	}
}
