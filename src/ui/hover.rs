use bevy::{
	math::bounding::{Aabb2d, BoundingCircle, BoundingVolume},
	prelude::*,
};

pub const BOX: &str = "A box. Can be moved around and placed onto buttons.";
pub const BUTTON: &str = "A button, needs a box.";
pub const FLAG: &str = "A flag, needs a player.";
pub const PLAYER: &str = "It's a you!";
pub const CYCLE_MANUAL: &str = "This cycle can only be turned while you stand on it.";
pub const CYCLE_AUTOMATIC: &str = "This cycle can be turned by left/right clicking its center.";
pub const CYCLE_STILL: &str = "This cycle cannot be turned on its own.";
pub const WALL: &str = "A wall, does not let objects through.";
pub const DETECTOR: &str = "A detector, triggers oneways when something goes through";

pub const BLOCKADE_WARNING: &str =
	"The last turn did not execute because multiple cycles tried to move this vertex, resulting in a conflict that jammed the system.";

pub const WALL_HIT_WARNING: &str =
	"The last turn did not execute because this wall would've been hit by an object on this cycle.";

use crate::{camera::CameraHarness, screen::Screen};

#[derive(Component, Clone, Copy, Debug, Reflect)]
pub struct HoverText;

#[derive(Component, Clone, Debug, Reflect)]
pub struct Hoverable {
	pub hover_text: &'static str,
	pub hover_bounding_circle: Option<BoundingCircle>,
	pub hover_bounding_box: Option<Aabb2d>,
}

/// Contains an overview of conditions that are needed to complete the level
#[derive(Resource, Debug, Clone, Reflect, Default)]
pub struct HintText {
	pub hover_text: Option<String>,
	pub hint_text: Option<String>,
}

pub fn plugin(app: &mut App) {
	app.init_resource::<HintText>()
		.add_systems(Update, (update_hover, update_hover_text));
}

fn update_hover(
	query: Query<(Entity, &Hoverable, &GlobalTransform)>,
	window: Single<&Window>,
	camera: Single<(&Camera, &GlobalTransform), With<CameraHarness>>,
	mut hint_text: ResMut<HintText>,
) {
	let (camera, camera_transform) = *camera;
	let cursor_pos = window
		.cursor_position()
		.and_then(|p| camera.viewport_to_world_2d(camera_transform, p).ok());
	if let Some(cursor_pos) = cursor_pos {
		let mut closest_hoverable: Option<Entity> = None;
		let mut closest_distance = f32::MAX;
		for (entity, hoverable, transform) in query.iter() {
			let translation = transform.translation();
			// This is a formula to extract the z rotation from the quaternion without trig functions
			// Trust me
			let rotation = transform.rotation();
			let rotation = Rot2 {
				cos: rotation.w,
				sin: rotation.z,
			}
			.try_normalize()
			.unwrap_or_default();
			let rotation = rotation * rotation;
			// Cursor position in local coordinates
			let transformed_cursor = rotation.inverse() * (cursor_pos - translation.xy());
			let mut hovered = false;
			let mut distance = f32::MAX;
			if let Some(bounding_circle) = hoverable.hover_bounding_circle {
				let circle_distance = (transformed_cursor - bounding_circle.center).length();
				if circle_distance < bounding_circle.radius() {
					hovered = true;
					distance = distance.min(circle_distance);
				}
			}
			if let Some(bounding_box) = hoverable.hover_bounding_box {
				if Vec2::cmplt(bounding_box.min, transformed_cursor).all()
					&& Vec2::cmpgt(bounding_box.max, transformed_cursor).all()
				{
					hovered = true;
					// Evil hack approximation, does not actually compute the box inside distance
					let box_distance = (transformed_cursor - bounding_box.center())
						.abs()
						.max_element();
					distance = distance.min(box_distance);
				}
			}

			if hovered && distance < closest_distance {
				closest_distance = distance;
				closest_hoverable = Some(entity);
			}
		}
		if let Some(target_hover) = closest_hoverable {
			if let Ok((_, hoverable, _)) = query.get(target_hover) {
				hint_text.hover_text = Some(hoverable.hover_text.into());
			}
		} else {
			hint_text.hover_text = None;
		}
	}
}

fn update_hover_text(
	mut text_query: Query<(&mut Text, &mut Visibility), With<HoverText>>,
	hint_text: Res<HintText>,
	state: Res<State<Screen>>,
) {
	let should_be_visible = *state.get() == Screen::Playing;

	let chosen_text = match hint_text.clone() {
		HintText {
			hover_text: Some(hover),
			hint_text: _,
		} => hover,
		HintText {
			hover_text: None,
			hint_text: Some(hint),
		} => hint,
		_ => "".into(),
	};
	for (mut text, mut visibility) in text_query.iter_mut() {
		*visibility = match should_be_visible {
			true => Visibility::Visible,
			false => Visibility::Hidden,
		};
		text.0.clone_from(&chosen_text);
	}
}
