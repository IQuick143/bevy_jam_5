//! The slider UI element

use super::{interaction::InteractionQuery, palette::*};
use crate::{graphics::*, ui::freeze::ui_not_frozen};
use bevy::{prelude::*, ui::RelativeCursorPosition};
use Val::*;

pub(super) fn plugin(app: &mut App) {
	app.add_systems(
		Update,
		(
			create_slider_children,
			(apply_slider_interaction_palettes, move_sliders).run_if(ui_not_frozen),
		),
	);
}

/// Marker component for slider UI nodes
#[derive(Component, Clone, Copy, Debug, Default, Reflect)]
#[reflect(Component)]
#[require(Node, Interaction, RelativeCursorPosition)]
pub struct Slider {
	/// Number of positions that the slider can be in
	step_count: u32,
	/// Current position of the slider
	position: u32,
}

impl Slider {
	pub fn new(step_count: u32, position: u32) -> Self {
		Self {
			step_count,
			position,
		}
	}
}

/// References to nodes that comprise the visible part of a slider
#[derive(Component, Clone, Copy, Debug, Reflect)]
#[reflect(Component)]
struct SliderChildren {
	body: Entity,
	handle: Entity,
}

fn create_slider_children(mut commands: Commands, query: Query<Entity, Added<Slider>>) {
	for id in &query {
		let bar_wrapper_id = commands
			.spawn((
				Name::new("Slider Wrapper"),
				Node {
					padding: UiRect::all(Px(NODE_RADIUS - RING_HALF_WIDTH)),
					width: Percent(100.0),
					..default()
				},
				ChildOf(id),
			))
			.id();

		// Bar outline
		commands.spawn((
			Name::new("Slider Outline"),
			Node {
				height: Px((RING_HALF_WIDTH + RING_OUTLINE_WIDTH) * 2.0),
				flex_grow: 1.0,
				..default()
			},
			BorderRadius::all(Px((RING_HALF_WIDTH + RING_OUTLINE_WIDTH) * 2.0)),
			BackgroundColor(SLIDER_OUTLINE),
			ChildOf(bar_wrapper_id),
		));

		// Handle
		let handle_id = commands
			.spawn((
				Name::new("Slider Handle"),
				Node {
					left: Px(0.0),
					top: Px(0.0),
					position_type: PositionType::Absolute,
					width: Px(NODE_RADIUS * 2.0),
					height: Px(NODE_RADIUS * 2.0),
					border: UiRect::all(Px(RING_OUTLINE_WIDTH)),
					box_sizing: BoxSizing::ContentBox,
					..default()
				},
				BorderColor(SLIDER_OUTLINE),
				BackgroundColor(SLIDER_FILL),
				BorderRadius::all(Px(NODE_RADIUS + RING_OUTLINE_WIDTH)),
				ChildOf(bar_wrapper_id),
			))
			.id();

		// Bar body wrapper
		let body_wrapper_id = commands
			.spawn((
				Name::new("Slider Body Wrapper"),
				Node {
					position_type: PositionType::Absolute,
					padding: UiRect::axes(
						Px(NODE_RADIUS - RING_HALF_WIDTH + RING_OUTLINE_WIDTH),
						Px(RING_OUTLINE_WIDTH),
					),
					left: Px(0.0),
					width: Percent(100.0),
					..default()
				},
				ChildOf(bar_wrapper_id),
			))
			.id();

		// Bar body
		let body_id = commands
			.spawn((
				Name::new("Slider Body"),
				Node {
					height: Px(RING_HALF_WIDTH * 2.0),
					flex_grow: 1.0,
					..default()
				},
				BorderRadius::all(Px(RING_HALF_WIDTH * 2.0)),
				BackgroundColor(SLIDER_FILL),
				ChildOf(body_wrapper_id),
			))
			.id();

		// Subnodes have already been inserted as children,
		// but we need them to be referenced via the semantic component as well
		commands.entity(id).insert(SliderChildren {
			body: body_id,
			handle: handle_id,
		});
	}
}

fn apply_slider_interaction_palettes(
	slider_q: InteractionQuery<&SliderChildren>,
	mut node_q: Query<&mut BackgroundColor>,
) {
	for (interaction, children) in &slider_q {
		let new_color = match interaction {
			Interaction::None => SLIDER_FILL,
			Interaction::Hovered => SLIDER_HOVERED_FILL,
			Interaction::Pressed => SLIDER_PRESSED_FILL,
		};
		for node_id in [children.handle, children.body] {
			let Ok(mut background_color) = node_q.get_mut(node_id) else {
				warn!("Entity referenced by SliderChildren does not have a background color");
				continue;
			};
			background_color.0 = new_color;
		}
	}
}

fn move_sliders(
	mut slider_q: Query<
		(
			&mut Slider,
			&SliderChildren,
			&Interaction,
			&ComputedNode,
			&RelativeCursorPosition,
		),
		Changed<RelativeCursorPosition>,
	>,
	mut node_q: Query<&mut Node>,
) {
	for (mut slider, children, interaction, node, cursor_pos) in &mut slider_q {
		if *interaction != Interaction::Pressed {
			continue;
		}
		let Some(cursor_pos) = cursor_pos.normalized else {
			continue;
		};
		let node_width = node.size.x * node.inverse_scale_factor;
		let bar_offset = NODE_RADIUS + RING_OUTLINE_WIDTH;
		let bar_width = node_width - 2.0 * bar_offset;
		let corrected_cursor_offset = (cursor_pos.x * node_width - bar_offset) / bar_width;
		let new_position = (corrected_cursor_offset * slider.step_count as f32)
			.round()
			.clamp(0.0, slider.step_count as f32) as u32;
		if new_position != slider.position {
			slider.position = new_position;
			let Ok(mut handle) = node_q.get_mut(children.handle) else {
				warn!("Entity referenced by SliderChildren is not a Node");
				continue;
			};
			handle.left = Px(bar_width * new_position as f32 / slider.step_count as f32);
		}
	}
}
