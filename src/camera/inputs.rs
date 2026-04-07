//! User controls for camera movement

use crate::{AppSet, input::prelude::*, screen::Screen};
use bevy::prelude::*;

pub(super) fn plugin(app: &mut App) {
	app.add_message::<MoveCameraMessage>()
		.add_message::<ZoomCameraMessage>()
		.init_resource::<EnableCursorPanning>()
		.init_resource::<LastCursorPosition>()
		.add_systems(
			Update,
			camera_movement_inputs
				.in_set(AppSet::RecordInput)
				.run_if(in_state(Screen::Playing)),
		);
}

#[derive(Resource, Clone, Copy, PartialEq, Eq, Debug, Deref, DerefMut)]
pub struct EnableCursorPanning(pub bool);

impl Default for EnableCursorPanning {
	fn default() -> Self {
		// Panning is enabled by default
		Self(true)
	}
}

/// A message sent to move the camera
///
/// Units are fraction of the camera top speed, [-1, 1]
/// Sent during [`AppSet::RecordInput`]
#[derive(Message)]
pub struct MoveCameraMessage(pub Vec2);

/// A message describing a camera zoom input
///
/// Sent during [`AppSet::RecordInput`]
#[derive(Message)]
pub enum ZoomCameraMessage {
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
	enable_cursor_panning: Res<EnableCursorPanning>,
	held_inputs: Res<PressedActions>,
	mut last_cursor_position: ResMut<LastCursorPosition>,
	mut camera_move: MessageWriter<MoveCameraMessage>,
	mut camera_zoom: MessageWriter<ZoomCameraMessage>,
) {
	let cursor_pos = window.cursor_position();
	let mut camera_direction = Vec2::ZERO;
	for &input in held_inputs.0.iter() {
		match input {
			InputAction::Direction(dir) => {
				camera_direction += camera_pan_from_direction(dir);
			}
			InputAction::Zoom(value) => {
				if value > 0 {
					camera_zoom.write(ZoomCameraMessage::In);
				}
				if value < 0 {
					camera_zoom.write(ZoomCameraMessage::Out);
				}
			}
			_ => {}
		}
	}
	if camera_direction == Vec2::ZERO && **enable_cursor_panning {
		if **last_cursor_position != cursor_pos {
			// The cursor has moved, so we allow it to trigger panning
			**last_cursor_position = None;
			if let Some(cursor_pos) = cursor_pos {
				let camera_direction = camera_pan_from_cursor(cursor_pos, window.size());
				camera_move.write(MoveCameraMessage(camera_direction));
			}
		}
	} else {
		// A key was touched, so we freeze panning by the cursor until it moves
		**last_cursor_position = cursor_pos;
		camera_move.write(MoveCameraMessage(camera_direction));
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

fn camera_pan_from_direction(input_direction: crate::input::Direction) -> Vec2 {
	match input_direction {
		crate::input::Direction::Up => Vec2::Y,
		crate::input::Direction::Down => -Vec2::Y,
		crate::input::Direction::Left => -Vec2::X,
		crate::input::Direction::Right => Vec2::X,
	}
}
