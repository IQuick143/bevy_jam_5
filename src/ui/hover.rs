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

pub const HINT_BOX: &str = "Hi!! I'm the BOTTOM TEXT, I tell you about stuff if you hover on it!";

use crate::{
	assets::GlobalFont,
	graphics::{layers, GAME_AREA, HINT_TEXT_SIZE},
	screen::Screen,
};

use super::widgets::Containers;

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
		.add_systems(Update, (update_hover, update_hover_text))
		.add_systems(Startup, spawn_hover_text);
}

fn spawn_hover_text(mut commands: Commands, font: Res<GlobalFont>) {
	let margin = 10.0;
	//	let text = "Click to rotate the wheels clockwise! Right click rotates them anti-clockwise! Get the boxes on the buttons and the player to the flag!";
	commands.spawn((
		bevy::text::TextBounds {
			width: Some(HINT_TEXT_SIZE.x - margin * 2.0),
			height: Some(HINT_TEXT_SIZE.y - margin * 2.0),
		},
		Transform::from_xyz(
			0.0,
			-GAME_AREA.y / 2.0 + HINT_TEXT_SIZE.y / 2.0,
			layers::HINT_TEXT_PANEL,
		),
		bevy::sprite::Anchor::Center,
		Text2d::default(),
		get_text_style(&font),
		TextLayout {
			justify: JustifyText::Left,
			..default()
		},
		HoverText,
		Hoverable {
			hover_text: HINT_BOX,
			hover_bounding_circle: None,
			hover_bounding_box: Some(Aabb2d::new(Vec2::ZERO, HINT_TEXT_SIZE / 2.0)),
		},
	));
}

fn get_text_style(font: &GlobalFont) -> (TextFont, TextColor) {
	(
		TextFont {
			font: font.0.clone_weak(),
			font_size: 32.0,
			..default()
		},
		TextColor(super::palette::LABEL_TEXT),
	)
}

fn update_hover(
	query: Query<(Entity, &Hoverable, &GlobalTransform)>,
	window_q: Query<&Window>,
	camera_q: Query<(&Camera, &GlobalTransform)>,
	mut hint_text: ResMut<HintText>,
) {
	// This system may get called when exiting the app, after these entities
	// have been despawned, we do not want to crash in that case
	if window_q.is_empty() || camera_q.is_empty() {
		return;
	}

	let window = window_q.single();
	let (camera, camera_transform) = camera_q.single();
	let cursor_pos = window
		.cursor_position()
		.and_then(|p| camera.viewport_to_world_2d(camera_transform, p).ok());
	if let Some(cursor_pos) = cursor_pos {
		let mut closest_hoverable: Option<Entity> = None;
		let mut closest_distance = f32::MAX;
		for (entity, hoverable, transform) in query.iter() {
			let translation = transform.translation();
			// Cursor position in local coordinates
			let transformed_cursor = cursor_pos - translation.xy();
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
	mut text_query: Query<(&mut Text2d, &mut Visibility), With<HoverText>>,
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
