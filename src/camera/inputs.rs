//! User controls for camera movement

use crate::AppSet;
use bevy::prelude::*;

pub(super) fn plugin(app: &mut App) {
	app.add_event::<MoveCameraEvent>()
		.add_event::<ZoomCameraEvent>()
		.init_resource::<LastCursorPosition>()
		.add_systems(Update, camera_movement_inputs.in_set(AppSet::RecordInput));
}

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

	if input_key.pressed(KeyCode::NumpadAdd) {
		camera_zoom.write(ZoomCameraEvent::In);
	}
	if input_key.pressed(KeyCode::NumpadSubtract) {
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
