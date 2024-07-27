use bevy::math::bounding::{Aabb2d, BoundingVolume};

use crate::{
	game::{graphics::{GAME_AREA, HINT_TEXT_SIZE}, prelude::*},
	screen::Screen,
};

pub fn plugin(app: &mut App) {
	app.init_resource::<HintText>()
		.add_systems(Update, (update_hover, update_hover_text))
		.add_systems(Startup, spawn_hover_text);
}

fn spawn_hover_text(mut commands: Commands, asset_server: Res<AssetServer>) {
	let text_box_z = -100.0;
	let margin = 10.0;
	//	let text = "Click to rotate the wheels clockwise! Right click rotates them anti-clockwise! Get the boxes on the buttons and the player to the flag!";
	commands.spawn((
		Text2dBundle {
			text_2d_bounds: bevy::text::Text2dBounds {
				size: Vec2::new(
					HINT_TEXT_SIZE.x - margin * 2.0,
					HINT_TEXT_SIZE.y - margin * 2.0,
				),
			},
			transform: Transform::from_xyz(
				0.0,
				-GAME_AREA.y / 2.0 + HINT_TEXT_SIZE.y / 2.0,
				text_box_z,
			),
			text_anchor: bevy::sprite::Anchor::Center,
			text: Text::from_section("", get_text_style(&asset_server))
				.with_justify(JustifyText::Left),
			..default()
		},
		HoverText,
		Hoverable {
			hover_text: "Hi!! I'm the BOTTOM TEXT, I tell you about stuff if you hover on it!",
			hover_bounding_circle: None,
			hover_bounding_box: Some(Aabb2d::new(Vec2::ZERO, HINT_TEXT_SIZE / 2.0)),
		},
	));
}

fn get_text_style(_asset_server: &Res<AssetServer>) -> TextStyle {
	TextStyle {
		//font: asset_server.load("fonts/your_font_here.ttf"),
		font_size: 32.0,
		color: Color::BLACK,
		..default()
	}
}

fn update_hover(
	query: Query<(Entity, &Hoverable, &GlobalTransform)>,
	window_q: Query<&Window>,
	camera_q: Query<(&Camera, &GlobalTransform)>,
	mut hint_text: ResMut<HintText>,
) {
	let window = window_q.single();
	let (camera, camera_transform) = camera_q.single();
	let cursor_pos = window
		.cursor_position()
		.and_then(|p| camera.viewport_to_world_2d(camera_transform, p));
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
				hint_text.hover_text = Some(hoverable.hover_text);
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
	asset_server: Res<AssetServer>,
) {
	let should_be_visible = match state.get() {
		Screen::Splash => false,
		Screen::Loading => false,
		Screen::Title => false,
		Screen::Credits => false,
		Screen::LevelSelect => false,
		Screen::Level(_) => true,
	};

	let chosen_text = match *hint_text {
		HintText {
			hover_text: Some(hover),
			hint_text: _,
		} => hover,
		HintText {
			hover_text: None,
			hint_text: Some(hint),
		} => hint,
		_ => "",
	};
	for (mut text, mut visibility) in text_query.iter_mut() {
		*visibility = match should_be_visible {
			true => Visibility::Visible,
			false => Visibility::Hidden,
		};
		text.sections = vec![TextSection::new(chosen_text, get_text_style(&asset_server))];
	}
}
