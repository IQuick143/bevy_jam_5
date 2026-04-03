//! Scroll box UI node behavior

use bevy::{
	input::mouse::{MouseScrollUnit, MouseWheel},
	picking::hover::Hovered,
	prelude::*,
};

pub(super) fn plugin(app: &mut App) {
	app.add_systems(Update, (scrollbox_input, scrollbox_movement).chain());
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
	/// Amount of scroll that we're lagging behind with.
	lines_to_move: f32,
	/// How long it takes to finish the scroll (in fractions of scroll time)
	progress: f32,
	touch: f32,
	touch_input_is_held: bool,
}

impl ScrollboxInertia {
	/// How many lines will be scrolled at a given progress
	#[expect(
		clippy::neg_cmp_op_on_partial_ord,
		reason = "Treating IEEE754 NaN's and such this way."
	)]
	fn lines_scrolled_at(&self, progress: f32) -> f32 {
		// The velocity profile follows `2 * (1-progress)`
		// The progress is thus `1 - (1-progress)^2`
		self.lines_to_move
			* if !(progress > 0.0) {
				0.0
			} else if progress >= 1.0 {
				1.0
			} else {
				1.0 - (1.0 - progress) * (1.0 - progress)
			}
	}

	/// How many lines of scroll are remaining
	fn remaining_scroll(&self) -> f32 {
		self.lines_to_move - self.lines_scrolled_at(self.progress)
	}
}

/// Time (in seconds) it takes for the scrolling to complete
const SCROLL_TIME: f32 = 0.2;

/// Fraction of inertia that is conserved after one second
const SCROLL_FRICTION: f32 = 0.0002;

fn scrollbox_input(
	mut inputs: MessageReader<MouseWheel>,
	mut query: Query<(
		&mut ScrollPosition,
		&mut ScrollboxInertia,
		&Scrollbox,
		&Hovered,
	)>,
	time: Res<Time>,
) {
	// Find the active scrollbox
	let target = query
		.iter_mut()
		.find(|(_, _, _, is_hovered)| is_hovered.get());
	let Some((mut scroll, mut inertia, scrollbox, _)) = target else {
		return;
	};

	let delta_t = time.delta_secs();
	inertia.touch_input_is_held = false;

	// Update the scroll offset using inputs
	for msg in inputs.read() {
		match msg.unit {
			MouseScrollUnit::Line => {
				// Line-based devices like the mouse send the scrollbox
				// into movement, but do not move it directly
				inertia.lines_to_move = inertia.remaining_scroll() - scrollbox.step * msg.y;
				// Getting mouse input puts us back into 0 progress
				inertia.progress = 0.0;
			}
			MouseScrollUnit::Pixel => {
				// Devices like touchpad drag the scrollbox directly,
				// but are left with some inertia at the end
				scroll.y -= msg.y;
				if !inertia.touch_input_is_held {
					inertia.touch = 0.0;
					inertia.touch_input_is_held = true;
				}
				inertia.touch -= msg.y / delta_t;
			}
		}
	}
}

fn scrollbox_movement(
	mut query: Query<(&mut ScrollPosition, &mut ScrollboxInertia, &ComputedNode)>,
	time: Res<Time>,
) {
	let delta_t = time.delta_secs();

	for (mut scroll, mut inertia, node) in query.iter_mut() {
		let max_offset =
			(node.content_size().y - node.size().y).max(0.0) * node.inverse_scale_factor();

		// Cap inertia so the scrolling is smooth near the edges
		if inertia.lines_to_move != 0.0 {
			let max_scroll = max_offset - scroll.y;
			let min_scroll = -scroll.y;
			inertia.lines_to_move = inertia.lines_to_move.clamp(min_scroll, max_scroll);
		}

		if inertia.progress < 1.0 {
			let scrolled_before = inertia.lines_scrolled_at(inertia.progress);
			inertia.progress += delta_t / SCROLL_TIME;
			inertia.progress = inertia.progress.clamp(0.0, 1.0);
			let scrolled_after = inertia.lines_scrolled_at(inertia.progress);
			scroll.y += scrolled_after - scrolled_before;
		}

		// Additional scroll offset due to inertia
		if !inertia.touch_input_is_held {
			scroll.y += inertia.touch * delta_t;
		}

		// Dissipate inertia
		let retained_inertia = SCROLL_FRICTION.powf(delta_t);
		inertia.touch *= retained_inertia;

		// Clamp to the edges
		scroll.y = scroll.y.clamp(0.0, max_offset);

		// If we hit the edge, the scroll finishes
		if (scroll.y == 0.0 && inertia.lines_to_move < 0.0)
			|| (scroll.y == max_offset && inertia.lines_to_move > 0.0)
		{
			inertia.lines_to_move = 0.0;
			inertia.progress = 0.0;
		}
	}
}
