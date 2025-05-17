use std::f32::consts::TAU;

use super::{
	components::*, drawing::CycleCenterVisualEntities, level::ThingData, logic::*, prelude::*,
};
use crate::AppSet;
use bevy::platform::collections::HashMap;
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
				cycle_turning_animation_system.run_if(on_event::<RotateSingleCycle>),
			)
				.in_set(AppSet::UpdateVisuals),
			spin_animation_system,
			jump_turn_animation_system,
		),
	);
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Reflect)]
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

/// Component for enabling path animations
#[derive(Component, Debug, Clone, Default, Reflect)]
pub struct AnimatedObject {
	/// The position where the object should be when no animation plays
	static_position: Vec2,
	/// The segments of the path that the object should take
	segments: std::collections::VecDeque<AnimationPathSegment>,
	/// Combined length of all path segments
	/// since the path was last modified
	total_length: f32,
	/// Combined length of all expired path segments
	/// since the path was last modified
	traversed_length: f32,
	/// Progress on the full animation in [0, 1]
	progress: f32,
	/// Spatial progress through the first segment [0, 1]
	first_segment_progress: f32,
}

impl AnimatedObject {
	pub fn is_in_progress(&self) -> bool {
		!self.segments.is_empty()
	}

	/// Advances the animation by a given fraction of its length
	pub fn add_progress(&mut self, delta_progress: f32) {
		self.progress += delta_progress;
		// Progress corrected by the easing function
		// Also relative distance [0, 1] covered so far
		let distance_progress = animation_easing_function(self.progress);
		// How far (by distance) we are into the first segment
		let mut delta_distance = distance_progress * self.total_length - self.traversed_length;

		while let Some(first_segment) = self.segments.front() {
			log::info!("{self:?}");
			let first_segment_length = first_segment.length();
			// Correct for rounding errors by explicitly overriding at 1
			let first_segment_completed =
				first_segment_length <= delta_distance || self.progress >= 1.0;
			if first_segment_completed {
				// The whole segment has been animated, so remove it
				// and move on to the next
				self.traversed_length += first_segment_length;
				delta_distance -= first_segment_length;
				self.segments.pop_front();
			} else {
				// Stop in the middle of the segment
				self.first_segment_progress = delta_distance / first_segment_length;
				return;
			}
			log::info!("Dropping section");
		}

		// No segments are left to animate, so use static position
		self.progress = 1.0;
		self.total_length = 0.0;
		self.traversed_length = 0.0;
	}

	/// Gets the current position on the path
	pub fn sample(&self) -> Vec2 {
		self.segments
			.front()
			.map(|x| x.sample(self.first_segment_progress))
			.unwrap_or(self.static_position)
	}

	/// Adds another segment to the animation
	///
	/// This resets animation progress to zero.
	/// If possible, the new segment is spliced into the last
	/// existing segment.
	pub fn add_segment(&mut self, segment: AnimationPathSegment) {
		self.static_position = segment.end_position();
		self.reset_progress();
		self.append_segment(segment);
	}

	/// Restarts the remaining path as a new animation sequence
	fn reset_progress(&mut self) {
		if let Some(first_segment) = self.segments.front_mut() {
			let shortened_first_segment = first_segment.cut(self.first_segment_progress);
			let first_segment_length = first_segment.length();
			let first_segment_remaining_length = shortened_first_segment.length();
			self.total_length += first_segment_remaining_length - first_segment_length;
			*first_segment = shortened_first_segment;
		}
		self.progress = 0.0;
		self.first_segment_progress = 0.0;
		self.total_length -= self.traversed_length;
		self.traversed_length = 0.0;
	}

	/// Adds a path segment to current animation sequence
	fn append_segment(&mut self, segment: AnimationPathSegment) {
		if let Some(last_segment) = self.segments.pop_back() {
			if let Some(new_last_segment) = segment.splice(&last_segment) {
				let last_segment_length = last_segment.length();
				let new_last_segment_length = new_last_segment.length();
				self.total_length += new_last_segment_length - last_segment_length;
				if new_last_segment_length >= Self::SKIP_SEGMENT_LENGTH_THRESHOLD {
					self.segments.push_back(new_last_segment);
				}
			} else {
				self.segments.push_back(last_segment);
				let added_length = segment.length();
				self.total_length += added_length;
				self.segments.push_back(segment);
			}
		} else {
			let added_length = segment.length();
			self.total_length += added_length;
			self.segments.push_back(segment);
		}
	}

	/// How long a path segment has to be in order to not be skipped
	const SKIP_SEGMENT_LENGTH_THRESHOLD: f32 = 0.0001;
}

/// A simple segment of a path animation
#[derive(Clone, Copy, Debug, Reflect)]
pub enum AnimationPathSegment {
	/// Linear path defined by its start and end points
	Line(Vec2, Vec2),
	/// Arc segment
	CircleArc(CircleArcPathSegment),
}

impl AnimationPathSegment {
	fn end_position(&self) -> Vec2 {
		match self {
			Self::Line(_, x) => *x,
			Self::CircleArc(x) => x.center_point + Vec2::from_angle(x.final_angle) * x.radius,
		}
	}

	fn length(&self) -> f32 {
		match self {
			Self::Line(x, y) => x.distance(*y),
			Self::CircleArc(x) => (x.initial_angle - x.final_angle).abs() * x.radius,
		}
	}

	fn sample(&self, progress: f32) -> Vec2 {
		match self {
			Self::Line(x, y) => x.lerp(*y, progress),
			Self::CircleArc(x) => {
				let current_angle = x.initial_angle.lerp(x.final_angle, progress);
				x.center_point + Vec2::from_angle(current_angle) * x.radius
			}
		}
	}

	/// Constructs a new path segment that is what will remain
	/// of this path segment after a given progression is elapsed
	fn cut(&self, progress: f32) -> Self {
		match self {
			Self::Line(x, y) => Self::Line(x.lerp(*y, progress), *y),
			Self::CircleArc(x) => {
				let mut result = *x;
				result.initial_angle = x.initial_angle.lerp(x.final_angle, progress);
				Self::CircleArc(result)
			}
		}
	}

	/// How different can two path segments be to be still considered splicable
	const SPLICE_DISTANCE_THRESHOLD: f32 = 0.0001;

	/// Attempts to splice a segment into the previous one
	/// ## Parameters
	/// - `other` - the segment that precedes this one
	/// ## Returns
	/// Path segment equivalent to both arguments in a sequence,
	/// sans backtracking, or [`None`] if they cannot be spliced
	fn splice(&self, other: &Self) -> Option<Self> {
		match (self, other) {
			(&Self::Line(c, d), &Self::Line(a, b)) => {
				if b.distance_squared(c) > Self::SPLICE_DISTANCE_THRESHOLD.powi(2) {
					return None;
				}
				let normalized_dot = (b - a).normalize_or_zero().dot((c - d).normalize_or_zero());
				if normalized_dot > Self::SPLICE_DISTANCE_THRESHOLD.powi(2) {
					return None;
				}
				Some(Self::Line(a, d))
			}
			(Self::CircleArc(second), Self::CircleArc(first)) => {
				if first.center_point.distance_squared(second.center_point)
					> Self::SPLICE_DISTANCE_THRESHOLD.powi(2)
				{
					return None;
				}
				if (first.radius - second.radius).abs() > Self::SPLICE_DISTANCE_THRESHOLD {
					return None;
				}
				if (first.final_angle - second.initial_angle).rem_euclid(TAU)
					> Self::SPLICE_DISTANCE_THRESHOLD
				{
					return None;
				}
				let mut result = *first;
				result.final_angle += second.final_angle - second.initial_angle;
				Some(Self::CircleArc(result))
			}
			_ => None,
		}
	}
}

#[derive(Clone, Copy, Debug, Reflect)]
pub struct CircleArcPathSegment {
	/// Center of the arc
	pub center_point: Vec2,
	/// Radius of the arc
	pub radius: f32,
	/// Position angle of the start point
	pub initial_angle: f32,
	/// Position angle of the end point
	///
	/// This is always compared to the initial angle,
	/// and the direction of animation is determined
	/// by which angle is larger. This means it is possible for the path
	/// to span more than a full circle. by putting in angles that differ
	/// by more than 2 pi
	pub final_angle: f32,
}

impl CircleArcPathSegment {
	/// Calculates the value that should be put into [`CircleArcPathSegment::final_angle`]
	/// in order to make a path that is up to a full circle in a specified direction
	pub fn final_angle_from_single_rotation(
		start: f32,
		end: f32,
		direction: RotationDirection,
	) -> f32 {
		if start >= end && direction == RotationDirection::CounterClockwise {
			end + TAU
		} else if start <= end && direction == RotationDirection::Clockwise {
			end - TAU
		} else {
			end
		}
	}
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
	mut objects: Query<
		(&mut Transform, &VertexPosition, Option<&mut AnimatedObject>),
		With<Object>,
	>,
) {
	// Maps current vertices to previous vertices and the center of rotation as well as direction
	let mut permutation_map: HashMap<_, _> = HashMap::default();
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

fn move_objects(mut objects: Query<(&mut Transform, &mut AnimatedObject)>, time: Res<Time<Real>>) {
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
		commands.entity(id).insert(AnimatedObject::default());
	}
}
