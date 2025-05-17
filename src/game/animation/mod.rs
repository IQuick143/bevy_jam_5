mod paths;

use super::{
	components::*, drawing::CycleCenterVisualEntities, level::ThingData, logic::*, prelude::*,
};
use crate::AppSet;
use bevy::platform::collections::HashMap;
use paths::*;
use rand::Rng as _;
use std::f32::consts::TAU;

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
				cycle_turning_animation_system.run_if(on_event::<RotateSingleCycle>),
			)
				.in_set(AppSet::UpdateVisuals),
			spin_animation_system,
			jump_turn_animation_system,
		),
	);
}

fn animation_easing_function(t: f32) -> f32 {
	// Quadratic ease-out.
	// Looks better than a flat rotation, but is easier
	// to chain like we do than a double-ended easing function
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
	mut objects: Query<(&mut Transform, &VertexPosition, Option<&mut PathAnimation>), With<Object>>,
) {
	// Maps current vertices to previous vertices and the center of rotation as well as direction
	let mut permutation_map = HashMap::new();
	for event in rotation_events.read() {
		let Ok((vertices, transform)) = cycles.get(event.0.target_cycle) else {
			log::warn!("Target of RotateSingleCycle is not a cycle entity");
			continue;
		};
		let location = transform.translation().xy();
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

		let end_position = end_vertex_transform.translation().xy();

		if let Some(mut animation) = animation {
			let Some(&(start_vertex, center_point, direction)) = permutation_map.get(&end_vertex)
			else {
				// No movement, we can skip
				continue;
			};

			let Ok(start_vertex_transform) = vertices.get(start_vertex) else {
				log::warn!("CycleVertices item is not a vertex entity");
				continue;
			};

			let start_position = start_vertex_transform.translation().xy();
			let initial_angle = (start_position - center_point).to_angle();
			let final_angle = (end_position - center_point).to_angle();
			let radius = end_position.distance(center_point);
			let adjusted_final_angle = CircleArcPathSegment::final_angle_from_single_rotation(
				initial_angle,
				final_angle,
				direction.into(),
			);

			animation.add_segment(AnimationPathSegment::CircleArc(CircleArcPathSegment {
				center_point,
				radius,
				initial_angle,
				final_angle: adjusted_final_angle,
			}));
		} else {
			// Object is not animated, so we just set the translation to the desired place.
			transform.translation.x = end_position.x;
			transform.translation.y = end_position.y;
		}
	}
}

fn move_objects(mut objects: Query<(&mut Transform, &mut PathAnimation)>, time: Res<Time<Real>>) {
	for (mut transform, mut animation) in objects.iter_mut() {
		if !animation.is_in_progress() {
			continue;
		}
		animation.add_progress(time.delta_secs() / ANIMATION_TIME);
		let new_position = animation.sample();
		transform.translation.x = new_position.x;
		transform.translation.y = new_position.y;
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

const CYCLE_CENTER_ANIMATION_ANGLE: f32 = std::f32::consts::PI / 2.0;

fn cycle_turning_animation_system(
	cycles_q: Query<&CycleCenterVisualEntities>,
	mut jump_q: Query<&mut JumpTurnAnimation>,
	mut events: EventReader<RotateSingleCycle>,
) {
	for event in events.read() {
		let Ok(visuals) = cycles_q.get(event.0.target_cycle) else {
			log::warn!("RotateSingleCycle event does not target a cycle entity");
			continue;
		};
		let Ok(mut animation) = jump_q.get_mut(visuals.sprite) else {
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
	query: Query<&CycleCenterVisualEntities, Added<CycleCenterVisualEntities>>,
) {
	for visuals in &query {
		commands
			.entity(visuals.sprite)
			.insert(JumpTurnAnimation::default());
		commands.entity(visuals.arrow).insert(SpinAnimation {
			current_phase: rand::thread_rng().gen_range(0.0..std::f32::consts::TAU),
			..default()
		});
	}
}

fn init_thing_animation(mut commands: Commands, query: Query<Entity, Added<ThingData>>) {
	for id in &query {
		// We do not need to initialize its static position
		// since the animation does not get queried until
		// something actually animates
		commands.entity(id).insert(PathAnimation::default());
	}
}
