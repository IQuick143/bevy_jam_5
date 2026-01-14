//! Steady rotation animations

use crate::game::{
	drawing::CycleCenterVisualEntities,
	spawn::{LevelInitialization, LevelInitializationSet},
};
use bevy::prelude::*;
use rand::Rng as _;
use std::f32::consts::TAU;

pub(super) fn plugin(app: &mut App) {
	app.add_systems(
		LevelInitialization,
		init_cycle_spin_animation.after(LevelInitializationSet::SpawnVisuals),
	)
	.add_systems(Update, spin_animation_system);
}

/// A component that causes an entity to rotate steadily
#[derive(Component, Clone, Copy, Debug, Reflect)]
pub struct SpinAnimation {
	pub frequency: f32,
	pub current_phase: f32,
}

impl SpinAnimation {
	pub fn progress(&mut self, delta_seconds: f32) {
		self.current_phase -= delta_seconds * self.frequency;
		if self.current_phase < 0.0 {
			self.current_phase += TAU;
		}
	}

	pub fn sample(&self) -> f32 {
		self.current_phase
	}

	pub const DEFAULT_FREQUENCY: f32 = 0.3;
}

impl Default for SpinAnimation {
	fn default() -> Self {
		Self {
			frequency: Self::DEFAULT_FREQUENCY,
			current_phase: 0.0,
		}
	}
}

fn init_cycle_spin_animation(
	mut commands: Commands,
	query: Query<&CycleCenterVisualEntities, Added<CycleCenterVisualEntities>>,
) {
	for visuals in &query {
		commands.entity(visuals.arrow).insert(SpinAnimation {
			current_phase: rand::rng().random_range(0.0..TAU),
			..default()
		});
	}
}

fn spin_animation_system(
	mut query: Query<(&mut SpinAnimation, &mut Transform)>,
	time: Res<Time<Real>>,
) {
	let delta_seconds = time.delta_secs();
	for (mut animation, mut transform) in &mut query {
		animation.progress(delta_seconds);
		transform.rotation = Quat::from_axis_angle(Vec3::Z, animation.sample());
	}
}
