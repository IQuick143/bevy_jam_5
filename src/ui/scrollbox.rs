//! Scroll box UI node behavior

use bevy::{
	input::mouse::{MouseScrollUnit, MouseWheel},
	picking::hover::Hovered,
	prelude::*,
};

pub(super) fn plugin(app: &mut App) {
	app.add_systems(Update, scrollbox_movement);
}

/// Marker component that adds scroll behavior to a UI node
#[derive(Component, Clone, Copy, Debug, Default, Reflect)]
#[reflect(Component)]
#[require(Node, Hovered, ScrollboxInertia)]
pub struct Scrollbox {
	pub step: f32,
}

/// Support component for [`Scrollbox`] that keeps track
/// of inertia in scroll movement
///
/// Inputs from mouse-like and touchpad-like devices are
/// tracked separately
#[derive(Component, Clone, Copy, Debug, Default, Reflect)]
#[reflect(Component)]
struct ScrollboxInertia {
	line: f32,
	touch: f32,
}

/// Fraction of inertia that is conserved after one second
const SCROLL_FRICTION: f32 = 0.0002;

fn scrollbox_movement(
	mut inputs: MessageReader<MouseWheel>,
	mut query: Query<(
		&mut ScrollPosition,
		&mut ScrollboxInertia,
		&ComputedNode,
		&Scrollbox,
		&Hovered,
	)>,
	time: Res<Time>,
) {
	// Find the active scrollbox
	let target = query
		.iter_mut()
		.find(|(_, _, _, _, is_hovered)| is_hovered.get());
	let Some((mut scroll, mut inertia, node, scrollbox, _)) = target else {
		return;
	};

	let max_offset = (node.content_size().y - node.size().y).max(0.0) * node.inverse_scale_factor();
	let delta_t = time.delta_secs();
	let mut touch_input_is_held = false;

	// Update the scroll offset using inputs
	for msg in inputs.read() {
		match msg.unit {
			MouseScrollUnit::Line => {
				// Line-based devices like the mouse send the scrollbox
				// into movement, but do not move it directly
				inertia.line += scrollbox.step * msg.y * SCROLL_FRICTION.ln();
			}
			MouseScrollUnit::Pixel => {
				// Devices like touchpad drag the scrollbox directly,
				// but are left with some inertia at the end
				scroll.y -= msg.y;
				if !touch_input_is_held {
					inertia.touch = 0.0;
					touch_input_is_held = true;
				}
				inertia.touch -= msg.y / delta_t;
			}
		}
	}

	// Cap inertia so the scrolling is smooth near the edges
	if inertia.line != 0.0 {
		let max_inertia = (scroll.y - max_offset) * SCROLL_FRICTION.ln();
		let min_inertia = scroll.y * SCROLL_FRICTION.ln();
		inertia.line = inertia.line.clamp(min_inertia, max_inertia);
	}

	// Additional scroll offset due to inertia
	scroll.y += inertia.line * delta_t;
	if !touch_input_is_held {
		scroll.y += inertia.touch * delta_t;
	}

	// Dissipate inertia
	let retained_inertia = SCROLL_FRICTION.powf(delta_t);
	inertia.line *= retained_inertia;
	inertia.touch *= retained_inertia;

	// Clamp to the edges
	scroll.y = scroll.y.clamp(0.0, max_offset);
}
