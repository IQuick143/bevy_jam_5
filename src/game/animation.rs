use std::f32::consts::TAU;

use super::{components::*, level::ThingData, logic::*, prelude::*};
use crate::AppSet;
use bevy::utils::hashbrown::HashMap;
use rand::Rng as _;

pub fn plugin(app: &mut App) {
	app.add_systems(
		LevelInitialization,
		(
			init_thing_animation.after(LevelInitializationSet::SpawnPrimaryEntities),
			init_cycle_animation.after(LevelInitializationSet::SpawnVisuals),
		),
	);
	app.add_systems(
		Update,
		(
			(
				(listen_for_moves, move_objects).chain(),
				cycle_turning_animation_system.run_if(on_event::<RotateSingleCycle>()),
			)
				.in_set(AppSet::UpdateVisuals),
			spin_animation_system,
			jump_turn_animation_system,
		),
	);
}

#[derive(Debug, Clone, Copy, Default, Reflect)]
pub enum RotationDirection {
	#[default]
	Clockwise,
	CounterClockwise,
}

impl From<CycleTurningDirection> for RotationDirection {
	fn from(value: CycleTurningDirection) -> Self {
		match value {
			CycleTurningDirection::Nominal => RotationDirection::Clockwise,
			CycleTurningDirection::Reverse => RotationDirection::CounterClockwise,
		}
	}
}

/// Component for enabling animation behaviour
#[derive(Component, Debug, Clone, Copy, Default, Reflect)]
pub struct AnimatedObject {
	/// Which way the slerp should go.
	pub rotation_direction: RotationDirection,
	/// <0.0-1.0> percentage of the animation progress
	pub progress: f32,
	/// Center of rotation
	pub cycle_center: Vec3,
	/// The direction we're starting from, if None, the animation skips to the end
	pub start_direction: Option<Dir2>,
	pub start_magnitude: f32,
	/// The direction we're ending on, if None, the animation cannot play
	pub final_direction: Option<Dir2>,
	pub final_magnitude: f32,
}

impl AnimatedObject {
	pub fn sample(&self) -> Option<Vec3> {
		let target = self.final_direction?;
		Some(if let Some(source) = self.start_direction {
			let t = animation_easing_function(self.progress).clamp(0.0, 1.0);
			let dir = {
				let mut angle = Vec2::angle_between(*source, *target);
				match self.rotation_direction {
					RotationDirection::Clockwise => {
						if angle > 0.0 {
							angle -= TAU
						}
					}
					RotationDirection::CounterClockwise => {
						if angle < 0.0 {
							angle += TAU
						}
					}
				}
				Rot2::radians(angle * t) * source
			};
			let magnitude = f32::lerp(self.start_magnitude, self.final_magnitude, t);
			self.cycle_center + (magnitude * dir).extend(0.0)
		} else {
			self.cycle_center + (self.final_magnitude * target).extend(0.0)
		})
	}
}

fn animation_easing_function(t: f32) -> f32 {
	// Quadratic ease-out.
	// Looks better that a flat rotation, but is easier
	// to chain like we do that a double-ended easing function
	if t > 0.5 {
		1.0 - (1.0 - t).powi(2) * 4.0 / 3.0
	} else {
		t * 4.0 / 3.0
	}
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

/// A component that lets an entity rotate quickly
#[derive(Component, Clone, Copy, Debug, Reflect)]
pub struct JumpTurnAnimation {
	pub current_phase: f32,
	pub jump_animation_progress: f32,
	pub jump_animation_time: f32,
	pub jump_animation_magitude: f32,
}

impl JumpTurnAnimation {
	pub fn progress(&mut self, delta_seconds: f32) {
		if self.jump_animation_progress < 1.0 {
			self.jump_animation_progress += delta_seconds / self.jump_animation_time;
		}
	}

	pub fn make_jump(&mut self, magnitude: f32, animation_time: f32) {
		self.current_phase = self.sample() + magnitude;
		self.jump_animation_time = animation_time;
		self.jump_animation_magitude = magnitude;
		self.jump_animation_progress = 0.0;
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
		}
	}
}

const ANIMATION_TIME: f32 = 0.5;

fn listen_for_moves(
	mut rotation_events: EventReader<RotateSingleCycle>,
	cycles: Query<(&CycleVertices, &GlobalTransform)>,
	vertices: Query<&GlobalTransform, With<Vertex>>,
	mut objects: Query<
		(&mut Transform, &VertexPosition, Option<&mut AnimatedObject>),
		With<Object>,
	>,
) {
	// Maps current vertices to previous vertices and the center of rotation as well as direction
	let mut permutation_map = HashMap::new();
	for event in rotation_events.read() {
		let Ok((vertices, transform)) = cycles.get(event.0.target_cycle) else {
			log::warn!("Target of RotateSingleCycle is not a cycle entity");
			continue;
		};
		let location = transform.translation();
		let mut vertex_loop = get_flipped_wrapped_iterator(&vertices.0, event.0.direction);
		let Some(mut last_id) = vertex_loop.next() else {
			continue;
		};
		for vertex in vertex_loop {
			permutation_map.insert(vertex, (last_id, location, event.0.direction));
			last_id = vertex;
		}
	}

	for (mut transform, vertex_position, animation) in objects.iter_mut() {
		let end_vertex = vertex_position.0;

		let Ok(end_vertex_transform) = vertices.get(end_vertex) else {
			log::warn!("VertexPosition of an object is not a vertex entity");
			continue;
		};

		let end_position = end_vertex_transform.translation();

		if let Some(mut animation) = animation {
			let Some(&(start_vertex, center_of_rotation, direction)) =
				permutation_map.get(&end_vertex)
			else {
				// No movement, we can skip
				continue;
			};

			let Ok(start_vertex_transform) = vertices.get(start_vertex) else {
				log::warn!("CycleVertices item is not a vertex entity");
				continue;
			};

			// If the animation hasn't finished playing, we give it a chance to catch up.
			let sample = animation.sample();
			#[allow(clippy::unnecessary_unwrap)]
			// Clippy is annoying and it cannot be fixed cleanly
			let start_vector = if sample.is_some() && animation.progress < 1.0 {
				sample.unwrap()
			} else {
				start_vertex_transform.translation()
			};

			animation.rotation_direction = direction.into();

			animation.progress = 0.0;
			animation.cycle_center = center_of_rotation;
			let (start_dir, start_magnitude) =
				Dir2::new_and_length((start_vector - center_of_rotation).xy())
					.unwrap_or((Dir2::X, 0.0));
			let (end_dir, end_magnitude) = Dir2::new_and_length(
				(end_vertex_transform.translation() - center_of_rotation).xy(),
			)
			.unwrap_or((Dir2::X, 0.0));
			animation.start_direction = Some(start_dir);
			animation.start_magnitude = start_magnitude;
			animation.final_direction = Some(end_dir);
			animation.final_magnitude = end_magnitude;
		} else {
			// Object is not animated, so we just set the translation to the desired place.
			transform.translation.x = end_position.x;
			transform.translation.y = end_position.y;
		}
	}
}

fn move_objects(mut objects: Query<(&mut Transform, &mut AnimatedObject)>, time: Res<Time<Real>>) {
	for (mut transform, mut animation) in objects.iter_mut() {
		if animation.progress >= 1.0 {
			continue;
		}
		animation.progress += (time.delta_seconds() / ANIMATION_TIME).min(1.0);
		if let Some(goal) = animation.sample() {
			transform.translation.x = goal.x;
			transform.translation.y = goal.y;
		}
	}
}

fn spin_animation_system(
	mut query: Query<(&mut SpinAnimation, &mut Transform)>,
	time: Res<Time<Real>>,
) {
	let delta_seconds = time.delta_seconds();
	for (mut animation, mut transform) in &mut query {
		animation.progress(delta_seconds);
		transform.rotation = Quat::from_axis_angle(Vec3::Z, animation.sample());
	}
}

fn jump_turn_animation_system(
	mut query: Query<(&mut JumpTurnAnimation, &mut Transform)>,
	time: Res<Time<Real>>,
) {
	let delta_seconds = time.delta_seconds();
	for (mut animation, mut transform) in &mut query {
		animation.progress(delta_seconds);
		transform.rotation = Quat::from_axis_angle(Vec3::Z, animation.sample());
	}
}

const CYCLE_CENTER_ANIMATION_ANGLE: f32 = std::f32::consts::PI / 2.0;

fn cycle_turning_animation_system(
	cycles_q: Query<&CycleVisualEntities>,
	mut jump_q: Query<&mut JumpTurnAnimation>,
	mut events: EventReader<RotateSingleCycle>,
) {
	for event in events.read() {
		let Ok(visuals) = cycles_q.get(event.0.target_cycle) else {
			log::warn!("RotateSingleCycle event does not target a cycle entity");
			continue;
		};
		let Ok(mut animation) = jump_q.get_mut(visuals.center) else {
			log::warn!("Cycle center sprite does not have JumpTurnAnimation component");
			continue;
		};
		let direction_multiplier = match event.0.direction.into() {
			RotationDirection::Clockwise => -1.0,
			RotationDirection::CounterClockwise => 1.0,
		};
		animation.make_jump(
			direction_multiplier * CYCLE_CENTER_ANIMATION_ANGLE,
			ANIMATION_TIME,
		);
	}
}

fn init_cycle_animation(
	mut commands: Commands,
	query: Query<&CycleVisualEntities, Added<CycleVisualEntities>>,
) {
	for visuals in &query {
		commands
			.entity(visuals.center)
			.insert(JumpTurnAnimation::default());
		commands.entity(visuals.arrow).insert(SpinAnimation {
			current_phase: rand::thread_rng().gen_range(0.0..std::f32::consts::TAU),
			..default()
		});
	}
}

fn init_thing_animation(mut commands: Commands, query: Query<Entity, Added<ThingData>>) {
	for id in &query {
		commands.entity(id).insert(AnimatedObject::default());
	}
}
