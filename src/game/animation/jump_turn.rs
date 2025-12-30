//! One-off rotation animations of cycle centers

use super::{animation_easing_function, TurnAnimationLength};
use crate::{
	game::{
		components::GameStateEcsIndex,
		drawing::CycleCenterVisualEntities,
		logic_relay::RotateCycleGroupWithResult,
		prelude::PlayingLevelData,
		spawn::{LevelInitialization, LevelInitializationSet},
	},
	AppSet,
};
use bevy::prelude::*;

pub(super) fn plugin(app: &mut App) {
	app.add_systems(
		LevelInitialization,
		init_cycle_jump_turn_animation.after(LevelInitializationSet::SpawnVisuals),
	)
	.add_systems(
		Update,
		(
			jump_turn_animation_system,
			cycle_turning_animation_system
				.run_if(on_message::<RotateCycleGroupWithResult>)
				.in_set(AppSet::UpdateVisuals),
		),
	);
}

/// A component that lets an entity rotate quickly
#[derive(Component, Clone, Copy, Debug, Reflect)]
pub struct JumpTurnAnimation {
	pub current_phase: f32,
	pub jump_animation_progress: f32,
	pub jump_animation_time: f32,
	pub jump_animation_magitude: f32,
	pub bounce_midway: bool,
}

impl JumpTurnAnimation {
	pub fn progress(&mut self, delta_seconds: f32) {
		if self.jump_animation_progress < 1.0 {
			self.jump_animation_progress += delta_seconds / self.jump_animation_time;

			// Reverse the animation at the midpoint to return to its original position
			if self.bounce_midway && animation_easing_function(self.jump_animation_progress) >= 0.5
			{
				self.bounce_midway = false;
				self.jump_animation_magitude *= -1.0;
				self.current_phase -= self.jump_animation_magitude;
			}
		}
	}

	pub fn make_jump(&mut self, magnitude: f32, animation_time: f32) {
		self.current_phase = self.sample() + magnitude;
		self.jump_animation_time = animation_time;
		self.jump_animation_magitude = magnitude;
		self.jump_animation_progress = 0.0;
		self.bounce_midway = false;
	}

	pub fn sample(&self) -> f32 {
		if self.jump_animation_progress >= 1.0 {
			self.current_phase
		} else {
			self.current_phase
				- (1.0 - animation_easing_function(self.jump_animation_progress))
					* self.jump_animation_magitude
		}
	}
}

impl Default for JumpTurnAnimation {
	fn default() -> Self {
		Self {
			current_phase: 0.0,
			jump_animation_progress: 0.0,
			jump_animation_magitude: 0.0,
			jump_animation_time: 0.0,
			bounce_midway: false,
		}
	}
}

fn init_cycle_jump_turn_animation(
	mut commands: Commands,
	query: Query<&CycleCenterVisualEntities, Added<CycleCenterVisualEntities>>,
) {
	for visuals in &query {
		commands
			.entity(visuals.sprite)
			.insert(JumpTurnAnimation::default());
	}
}

const CYCLE_CENTER_ANIMATION_ANGLE: f32 = std::f32::consts::PI / 2.0;

fn cycle_turning_animation_system(
	cycles_q: Query<&CycleCenterVisualEntities>,
	mut jump_q: Query<&mut JumpTurnAnimation>,
	mut events: MessageReader<RotateCycleGroupWithResult>,
	animation_time: Res<TurnAnimationLength>,
	entity_index: Res<GameStateEcsIndex>,
	level_data: PlayingLevelData,
) {
	let level_data = match level_data.get() {
		Ok(level) => level,
		Err(err) => {
			error!("Playing level data not available: {err}");
			return;
		}
	};

	for event in events.read() {
		for (group_id, &rotate_amount) in event.result.groups_turned_by.iter().enumerate() {
			// Skip groups that did not turn at all
			if rotate_amount == 0 {
				continue;
			}
			// Get the group data
			let Some(group) = level_data.groups.get(group_id) else {
				warn!("Rotation target group is out of range");
				continue;
			};
			for &(cycle_id, direction) in &group.cycles {
				// Get the animation component of the center sprite for each cycle
				let Some(target_cycle) = entity_index.cycles.get(cycle_id) else {
					warn!("Rotation target cycle is out of range");
					continue;
				};
				let Ok(visuals) = cycles_q.get(*target_cycle) else {
					warn!("RotateSingleCycle event does not target a cycle entity");
					continue;
				};
				let Ok(mut animation) = jump_q.get_mut(visuals.sprite) else {
					warn!("Cycle center sprite does not have JumpTurnAnimation component");
					continue;
				};
				// Start a new jump animation for all affected cycles
				let direction_multiplier = -(direction * rotate_amount).signum() as f32;
				animation.make_jump(
					direction_multiplier * CYCLE_CENTER_ANIMATION_ANGLE,
					**animation_time,
				);
				// If the turn failed, set the animation to bounce back
				animation.bounce_midway = event.result.blocked();
			}
		}
	}
}

fn jump_turn_animation_system(
	mut query: Query<(&mut JumpTurnAnimation, &mut Transform)>,
	time: Res<Time<Real>>,
) {
	let delta_seconds = time.delta_secs();
	for (mut animation, mut transform) in &mut query {
		animation.progress(delta_seconds);
		transform.rotation = Quat::from_axis_angle(Vec3::Z, animation.sample());
	}
}
