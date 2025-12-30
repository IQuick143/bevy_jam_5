//! Popup/down animation

use super::animation_easing_function;
use crate::AppSet;
use bevy::prelude::*;

pub(super) fn plugin(app: &mut App) {
	app.add_systems(Update, tick_popup_animation.in_set(AppSet::UpdateVisuals));
}

#[derive(Component, Clone, Copy, Debug, Reflect)]
pub struct PopupAnimation {
	progress: f32,
	animation_time: f32,
	starting_scale: f32,
	is_reversed: bool,
}

impl PopupAnimation {
	pub fn new(animation_time: f32, starting_scale: f32) -> Self {
		Self {
			progress: 0.0,
			animation_time,
			starting_scale,
			is_reversed: false,
		}
	}

	pub fn set_reversed(&mut self, is_reversed: bool) {
		self.is_reversed = is_reversed;
	}

	pub fn is_reversed(&self) -> bool {
		self.is_reversed
	}

	pub fn is_finished(&self) -> bool {
		if self.is_reversed {
			self.progress == 0.0
		} else {
			self.progress == 1.0
		}
	}
}

fn tick_popup_animation(
	mut query: Query<(&mut PopupAnimation, &mut Transform, &mut Sprite)>,
	time: Res<Time>,
) {
	for (mut animation, mut transform, mut sprite) in &mut query {
		if animation.is_finished() {
			continue;
		}

		let delta = time.delta_secs() / animation.animation_time;
		if animation.is_reversed {
			animation.progress -= delta;
		} else {
			animation.progress += delta;
		}
		animation.progress = animation.progress.clamp(0.0, 1.0);

		let adjusted_progress = animation_easing_function(animation.progress);
		let current_scale = animation.starting_scale.lerp(1.0, adjusted_progress);
		transform.scale = Vec3::splat(current_scale);
		sprite.color.set_alpha(adjusted_progress);
	}
}
