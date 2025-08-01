use crate::{ui::freeze::ui_not_frozen, AppSet};

use super::{
	camera::CameraHarness,
	level::{CyclePlacement, CycleShape},
	logic::*,
	prelude::*,
};

/// An event sent to move the camera
///
/// Units are fraction of the camera top speed, [-1, 1]
/// Sent during [`AppSet::RecordInput`]
#[derive(Event)]
pub struct MoveCameraEvent(pub Vec2);

/// An event describing a camera zoom input
///
/// Sent during [`AppSet::RecordInput`]
#[derive(Event)]
pub enum ZoomCameraEvent {
	In,
	Out,
}

pub(super) fn plugin(app: &mut App) {
	app.add_event::<MoveCameraEvent>()
		.add_event::<ZoomCameraEvent>()
		.init_resource::<LastCursorPosition>()
		.add_systems(Update, camera_movement_inputs.in_set(AppSet::RecordInput))
		.add_systems(
			Update,
			(
				cycle_inputs_system.in_set(AppSet::RecordInput),
				cycle_rotation_with_inputs_system.in_set(AppSet::ExecuteInput),
			)
				.run_if(ui_not_frozen),
		);
}

/// Position of the cursor at the previous time a panning key input was issued
///
/// Once set, no panning by cursor position can take place until the cursor moves
#[derive(Resource, Clone, Copy, PartialEq, Debug, Deref, DerefMut, Default)]
struct LastCursorPosition(Option<Vec2>);

fn camera_movement_inputs(
	window: Single<&Window>,
	input_key: Res<ButtonInput<KeyCode>>,
	mut last_cursor_position: ResMut<LastCursorPosition>,
	mut camera_move: EventWriter<MoveCameraEvent>,
	mut camera_zoom: EventWriter<ZoomCameraEvent>,
) {
	let cursor_pos = window.cursor_position();
	let camera_direction = camera_pan_from_keys(&input_key);
	if camera_direction == Vec2::ZERO {
		if **last_cursor_position != cursor_pos {
			// The cursor has moved, so we allow it to trigger panning
			**last_cursor_position = None;
			if let Some(cursor_pos) = cursor_pos {
				let camera_direction = camera_pan_from_cursor(cursor_pos, window.size());
				camera_move.write(MoveCameraEvent(camera_direction));
			}
		}
	} else {
		// A key was touched, so we freeze panning by the cursor until it moves
		**last_cursor_position = cursor_pos;
		camera_move.write(MoveCameraEvent(camera_direction));
	}

	if input_key.pressed(KeyCode::KeyP) {
		camera_zoom.write(ZoomCameraEvent::In);
	}
	if input_key.pressed(KeyCode::KeyO) {
		camera_zoom.write(ZoomCameraEvent::Out);
	}
}

/// Size of the margins around the borders of the window
/// where the cursor automatically pans the viewport,
/// in logical pixels
const WINDOW_PAN_MARGIN_SIZE: f32 = 100.0;

fn camera_pan_from_cursor(cursor_pos: Vec2, viewport_size: Vec2) -> Vec2 {
	let adjusted_cursor_pos = cursor_pos / WINDOW_PAN_MARGIN_SIZE;
	let negative_cursor_pos = (viewport_size - cursor_pos) / WINDOW_PAN_MARGIN_SIZE;
	let mut camera_direction = Vec2::ZERO;
	if adjusted_cursor_pos.x < 1.0 {
		camera_direction -= Vec2::X * (1.0 - adjusted_cursor_pos.x);
	}
	if negative_cursor_pos.x < 1.0 {
		camera_direction += Vec2::X * (1.0 - negative_cursor_pos.x);
	}
	if adjusted_cursor_pos.y < 1.0 {
		camera_direction += Vec2::Y * (1.0 - adjusted_cursor_pos.y);
	}
	if negative_cursor_pos.y < 1.0 {
		camera_direction -= Vec2::Y * (1.0 - negative_cursor_pos.y);
	}
	camera_direction
}

fn camera_pan_from_keys(input_key: &ButtonInput<KeyCode>) -> Vec2 {
	let mut camera_direction = Vec2::ZERO;
	if input_key.pressed(KeyCode::ArrowUp) {
		camera_direction += Vec2::Y;
	}
	if input_key.pressed(KeyCode::ArrowDown) {
		camera_direction -= Vec2::Y;
	}
	if input_key.pressed(KeyCode::ArrowLeft) {
		camera_direction -= Vec2::X;
	}
	if input_key.pressed(KeyCode::ArrowRight) {
		camera_direction += Vec2::X;
	}
	camera_direction
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
	window: Single<&Window>,
	camera: Single<(&Camera, &GlobalTransform), With<CameraHarness>>,
	mut cycles_q: Query<(
		Entity,
		&CyclePlacement,
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
	let (camera, camera_transform) = *camera;
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
		rot_events.write(RotateCycleGroup(rotation));
		record_events.write(RecordCycleGroupRotation(rotation));
	}
}
