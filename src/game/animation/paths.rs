//! Animation of movement of objects along cycles

use super::{animation_easing_function, TurnAnimationLength};
use crate::{
	game::{
		components::*,
		level::{CyclePlacement, LevelData, ThingData},
		prelude::*,
	},
	screen::Screen,
	AppSet,
};

pub(super) fn plugin(app: &mut App) {
	app.add_systems(
		LevelInitialization,
		init_thing_animation.after(LevelInitializationSet::SpawnPrimaryEntities),
	)
	.add_systems(
		Update,
		(listen_for_moves, move_objects)
			.chain()
			.in_set(AppSet::UpdateVisuals)
			.run_if(in_state(Screen::Playing)),
	);
}

/// Component for enabling path animations
#[derive(Component, Debug, Clone, Default, Reflect)]
struct PathAnimation {
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

impl PathAnimation {
	fn is_in_progress(&self) -> bool {
		!self.segments.is_empty()
	}

	/// Advances the animation by a given fraction of its length
	fn add_progress(&mut self, delta_progress: f32) {
		self.progress += delta_progress;
		// Progress corrected by the easing function
		// Also relative distance [0, 1] covered so far
		let distance_progress = animation_easing_function(self.progress);
		// How far (by distance) we are into the first segment
		let mut delta_distance = distance_progress * self.total_length - self.traversed_length;

		while let Some(first_segment) = self.segments.front() {
			let first_segment_length = first_segment.length;
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
		}

		// No segments are left to animate, so use static position
		self.progress = 1.0;
		self.total_length = 0.0;
		self.traversed_length = 0.0;
	}

	/// Gets the current position on the path
	fn sample(&self, level_data: &LevelData) -> Vec2 {
		self.segments
			.front()
			.map(|x| x.sample(self.first_segment_progress, level_data))
			.unwrap_or(self.static_position)
	}

	/// Adds another segment to the animation
	///
	/// This resets animation progress to zero.
	/// If possible, the new segment is spliced into the last
	/// existing segment.
	fn add_segment(&mut self, segment: AnimationPathSegment, new_static_pos: Vec2) {
		if segment.length < Self::SKIP_SEGMENT_LENGTH_THRESHOLD {
			return;
		}
		self.static_position = new_static_pos;
		self.reset_progress();
		self.append_segment(segment);
	}

	/// Restarts the remaining path as a new animation sequence
	fn reset_progress(&mut self) {
		if let Some(first_segment) = self.segments.front_mut() {
			let shortened_first_segment = first_segment.cut(self.first_segment_progress);
			let first_segment_length = first_segment.length;
			let first_segment_remaining_length = shortened_first_segment.length;
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
				let last_segment_length = last_segment.length;
				let new_last_segment_length = new_last_segment.length;
				self.total_length += new_last_segment_length - last_segment_length;
				if new_last_segment_length >= Self::SKIP_SEGMENT_LENGTH_THRESHOLD {
					self.segments.push_back(new_last_segment);
				}
			} else {
				self.segments.push_back(last_segment);
				let added_length = segment.length;
				self.total_length += added_length;
				self.segments.push_back(segment);
			}
		} else {
			let added_length = segment.length;
			self.total_length += added_length;
			self.segments.push_back(segment);
		}
	}

	/// How long a path segment has to be in order to not be skipped
	const SKIP_SEGMENT_LENGTH_THRESHOLD: f32 = 0.0001;
}

/// A simple segment of a path animation
#[derive(Clone, Copy, Debug, Reflect, Deref)]
struct AnimationPathSegment {
	/// Index of the cycle along which the object moves
	owner_cycle: usize,
	/// Position data of the segment
	#[deref]
	measurements: AnimationPathSegmentMeasurements,
}

impl AnimationPathSegment {
	fn end_position(&self, level_data: &LevelData) -> Vec2 {
		self.sample(1.0, level_data)
	}

	fn sample(&self, progress: f32, level_data: &LevelData) -> Vec2 {
		let cycle_placement = &level_data.cycles[self.owner_cycle].placement;
		self.measurements.sample(cycle_placement, progress)
	}

	/// Constructs a new path segment that is what will remain
	/// of this path segment after a given progression is elapsed
	fn cut(&self, progress: f32) -> Self {
		Self {
			owner_cycle: self.owner_cycle,
			measurements: self.measurements.cut(progress),
		}
	}

	/// Attempts to splice a segment into the previous one
	/// ## Parameters
	/// - `other` - the segment that precedes this one
	/// ## Returns
	/// Path segment equivalent to both arguments in a sequence,
	/// sans backtracking, or [`None`] if they cannot be spliced
	fn splice(&self, other: &Self) -> Option<Self> {
		// Only paths that lie on the same cycle can be spliced
		if self.owner_cycle != other.owner_cycle {
			return None;
		}
		Some(Self {
			owner_cycle: self.owner_cycle,
			measurements: self.measurements.splice(&other.measurements),
		})
	}
}

#[derive(Clone, Copy, Debug, Reflect)]
struct AnimationPathSegmentMeasurements {
	/// Starting position, relative to zero point of the owner cycle,
	/// of the animation
	///
	/// The value should be in range [0, 1), though this is not necessary
	start_pos: f32,
	/// Final position, relative to the zero point of the owner cycle,
	/// of the animation
	///
	/// `end_pos - start_pos > 1.0` indicates that the object should
	/// take multiple full turns around the cycle
	///
	/// `end_pos < start_pos` indicates turning counterclockwise
	end_pos: f32,
	/// Cached precomputed length of the path segment, in world units
	length: f32,
}

impl AnimationPathSegmentMeasurements {
	fn splice(&self, other: &Self) -> Self {
		Self {
			start_pos: other.start_pos,
			end_pos: other.end_pos + self.end_pos - self.start_pos,
			length: self.spliced_length(other),
		}
	}

	fn spliced_length(&self, other: &Self) -> f32 {
		if (self.start_pos > self.end_pos) == (other.start_pos > other.end_pos) {
			self.length + other.length
		} else {
			(self.length - other.length).abs()
		}
	}

	fn cut(&self, progress: f32) -> Self {
		Self {
			start_pos: self.sample_t(progress),
			end_pos: self.end_pos,
			length: self.length * (1.0 - progress),
		}
	}

	fn sample(&self, placement: &CyclePlacement, progress: f32) -> Vec2 {
		placement.sample(self.sample_t(progress))
	}

	fn sample_t(&self, progress: f32) -> f32 {
		self.start_pos.lerp(self.end_pos, progress)
	}
}

fn listen_for_moves(
	mut rotation_events: EventReader<TurnCycleResult>,
	mut objects: Query<(&mut Transform, Option<&mut PathAnimation>), With<Object>>,
	active_level: PlayingLevelData,
	entity_index: Res<GameStateEcsIndex>,
) {
	let level_data = match active_level.get() {
		Ok(d) => d,
		Err(e) => {
			error!("Active level data could not be loaded: {e}");
			return;
		}
	};
	for event in rotation_events.read() {
		// Maps vertices that have been affected to the path segments taken by the objects on them
		let vertex_paths = event.get_vertex_paths(level_data);

		let moved_objects = entity_index
			.objects
			.iter()
			.zip(vertex_paths)
			.filter_map(|(i, p)| i.and_then(|i| p.map(|p| (i, p))));
		for (object_id, path) in moved_objects {
			let Ok((mut transform, animation)) = objects.get_mut(object_id) else {
				warn!("Object referenced by entity index not found in ECS");
				continue;
			};
			let end_position = path.end_position(level_data);
			if let Some(mut animation) = animation {
				animation.add_segment(path, end_position);
			} else {
				// Object is not animated, so we just set the translation to the desired place.
				transform.translation.x = end_position.x;
				transform.translation.y = end_position.y;
			}
		}
	}
}

fn move_objects(
	mut objects: Query<(&mut Transform, &mut PathAnimation)>,
	time: Res<Time<Real>>,
	animation_time: Res<TurnAnimationLength>,
	active_level: PlayingLevelData,
) {
	let level_data = match active_level.get() {
		Ok(d) => d,
		Err(e) => {
			error!("Active level data could not be loaded: {e}");
			return;
		}
	};

	for (mut transform, mut animation) in objects.iter_mut() {
		if !animation.is_in_progress() {
			continue;
		}
		animation.add_progress(time.delta_secs() / **animation_time);
		let new_position = animation.sample(level_data);
		transform.translation.x = new_position.x;
		transform.translation.y = new_position.y;
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

impl TurnCycleResult {
	/// Calculates the paths taken by objects that lie on each individual vertex
	fn get_vertex_paths(&self, level_data: &LevelData) -> Vec<Option<AnimationPathSegment>> {
		let mut vertex_paths = vec![None; level_data.vertices.len()];
		for (cycle_id, rotate_by) in self.cycles_turned_by(level_data) {
			if rotate_by == 0 {
				// No rotation, nothing to see here
				continue;
			}
			let Some(cycle_data) = level_data.cycles.get(cycle_id) else {
				continue;
			};
			if cycle_data.vertex_indices.is_empty() {
				// Cycle has no vertices, no need to observe it further
				continue;
			}
			let vertex_count = cycle_data.vertex_indices.len();
			let rotate_amount = rotate_by.unsigned_abs() as usize;
			let full_rotations = rotate_amount / vertex_count;
			let absolute_movement_offset = rotate_amount % vertex_count;
			let movement_offset = if rotate_by > 0 {
				vertex_count - absolute_movement_offset
			} else {
				absolute_movement_offset
			};
			for (end_index, &vertex_id) in cycle_data.vertex_indices.iter().enumerate() {
				let start_index = (end_index + movement_offset) % vertex_count;
				let start_position = cycle_data.vertex_positions[start_index];
				let end_position = cycle_data.vertex_positions[end_index];
				let adjusted_end_position = {
					let is_clockwise = rotate_by > 0;
					let mut full_rotations = full_rotations;
					let is_integer_multiple_of_full_rotation = start_index == end_index;
					let start_and_end_are_swapped = (start_position < end_position) == is_clockwise;
					if !is_integer_multiple_of_full_rotation && start_and_end_are_swapped {
						full_rotations += 1;
					}
					let bias = full_rotations as f32;
					end_position - bias * rotate_by.signum() as f32
				};
				let cycle_length = cycle_data.placement.shape.length();
				let path_length = (start_position - adjusted_end_position).abs() * cycle_length;
				vertex_paths[vertex_id] = Some(AnimationPathSegment {
					owner_cycle: cycle_id,
					measurements: AnimationPathSegmentMeasurements {
						start_pos: start_position,
						end_pos: adjusted_end_position,
						length: path_length,
					},
				});
			}
		}
		vertex_paths
	}
}
