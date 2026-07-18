use super::{
	components::Cycle,
	level::{CycleCenterSpriteAppearence, CycleShape},
	logic_relay::*,
	prelude::*,
};
use crate::{
	AppSet,
	camera::CameraHarness,
	graphics::{RING_SHADOW_BLEED, SPRITE_LENGTH},
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

const CENTER_SPRITE_HITBOX_RADIUS: f32 = SPRITE_LENGTH / 2.0;

fn cycle_hover_system(
	mut hovered_cycle: ResMut<HoveredCycle>,
	window: Single<&Window>,
	camera: Single<(&Camera, &GlobalTransform), With<CameraHarness>>,
	mut cycles_q: Query<(
		Entity,
		&CycleShape,
		&CycleCenterSpriteAppearence,
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

	let (nearest_cycle, is_turnable) = cycles_q
		.iter()
		.filter_map(|(e, placement, center, turnability, _)| {
			let is_turnable = turnability.0;

			// Distance to the center sprite, if it actually hit
			let dc_sq = center.0.and_then(|center_offset| {
				let dc_sq = cursor_pos.distance_squared(placement.center() + center_offset);
				if dc_sq <= CENTER_SPRITE_HITBOX_RADIUS.powi(2) {
					Some(dc_sq)
				} else {
					None
				}
			});

			match placement {
				CycleShape::Circle { center, radius } => {
					let d_sq = center.distance_squared(cursor_pos);
					if dc_sq.is_some() || d_sq <= (radius + RING_SHADOW_BLEED).powi(2) {
						Some((e, d_sq, radius.powi(2), is_turnable, dc_sq))
					} else {
						None
					}
				}
				CycleShape::Line { start, end } => {
					let d = (end - start).normalize().perp_dot(cursor_pos - start);
					if dc_sq.is_some() || d.abs() <= RING_SHADOW_BLEED {
						Some((e, d.powi(2), 0.0, is_turnable, dc_sq))
					} else {
						None
					}
				}
			}
		})
		// Cannot just call min, because IEEE754
		.reduce(
			|(e1, d_sq_1, r_sq_1, t1, dc_sq_1), (e2, d_sq_2, r_sq_2, t2, dc_sq_2)| {
				// Sort by turnability first, because we want to select a turnable
				// cycle if at all possible, even if it is overlapped by locked ones
				// that would otherwise be a better match
				if (!t1, dc_sq_1.unwrap_or(f32::INFINITY), r_sq_1, d_sq_1)
					> (!t2, dc_sq_2.unwrap_or(f32::INFINITY), r_sq_2, d_sq_2)
				{
					(e2, d_sq_2, r_sq_2, t2, dc_sq_2)
				} else {
					(e1, d_sq_1, r_sq_1, t1, dc_sq_1)
				}
			},
		)
		.map(|(e, _, _, t, _)| (Some(e), t))
		.unwrap_or_default();

	if is_turnable {
		hovered_cycle.set_if_neq(HoveredCycle(nearest_cycle));
	} else {
		hovered_cycle.set_if_neq(HoveredCycle(None));
	}

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
