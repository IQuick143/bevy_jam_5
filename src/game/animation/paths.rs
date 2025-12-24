//! Animation of movement of objects along cycles

use smallvec::SmallVec;

use super::{animation_easing_function, TurnAnimationLength};
use crate::{
	game::{
		components::*,
		level::{CyclePlacement, LevelData, ThingData},
		logic_relay::RotateCycleGroupWithResult,
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
	fn add_segment(
		&mut self,
		segment: AnimationPathSegment,
		new_static_pos: Vec2,
		allow_splice: bool,
	) {
		if segment.length < Self::SKIP_SEGMENT_LENGTH_THRESHOLD {
			return;
		}
		self.static_position = new_static_pos;
		self.reset_progress();
		self.append_segment(segment, allow_splice);
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
	fn append_segment(&mut self, mut segment: AnimationPathSegment, allow_splice: bool) {
		if allow_splice {
			// Try to splice the new segment into the head of the path first
			// Splice multiple segments at once if possible,
			// because some may have been inserted into the path with splicing disabled
			while let Some(last_segment) = self.segments.pop_back() {
				if let Some(new_last_segment) = segment.splice(&last_segment) {
					// Splice into the last segment if possible
					let last_segment_length = last_segment.length;
					self.total_length -= last_segment_length;
					// Put the combined segment aside and try again
					segment = new_last_segment;
				} else {
					// Put the segment back and insert the new one normally
					self.segments.push_back(last_segment);
					break;
				}
			}
		}
		// Insert the new segment as stand-alone
		let added_length = segment.length;
		if added_length >= Self::SKIP_SEGMENT_LENGTH_THRESHOLD {
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
	fn start_position(&self, level_data: &LevelData) -> Vec2 {
		self.sample(0.0, level_data)
	}

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

	/// Constructs a new path segment that is this segment played backwards
	fn reverse(&self) -> Self {
		Self {
			owner_cycle: self.owner_cycle,
			measurements: self.measurements.reverse(),
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

	fn reverse(&self) -> Self {
		Self {
			start_pos: self.end_pos,
			end_pos: self.start_pos,
			length: self.length,
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
	mut rotation_events: MessageReader<RotateCycleGroupWithResult>,
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
		let vertex_paths = event.result.get_vertex_paths(level_data);

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
				animation.add_segment(path, end_position, true);
				// If the move got blocked, play the animation back without splicing
				if event.result.blocked() {
					let start_position = path.start_position(level_data);
					animation.add_segment(path.reverse(), start_position, false);
				}
			} else if !event.result.blocked() {
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
	///
	/// The paths apply to the object that ends up on a particular vertex.
	/// This is the starting vertex of the path if the turn is blocked, the ending
	/// vertex if it is not.
	fn get_vertex_paths(&self, level_data: &LevelData) -> Vec<Option<AnimationPathSegment>> {
		let is_vertex_blocked = self.jammed_vertex_mask(level_data);
		let is_cycle_jammed = self.jammed_cycle_mask(level_data);
		let wall_hits_by_cycle = self.wall_hits_by_cycle(level_data);
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
			let is_clockwise = rotate_by > 0;
			let rotate_amount = if is_cycle_jammed[cycle_id] {
				1 // If there is a jam, do not move anything more than one space
			} else {
				rotate_by.unsigned_abs() as usize
			};
			let full_rotations = rotate_amount / vertex_count;
			let absolute_movement_offset = rotate_amount % vertex_count;
			let movement_offset = if is_clockwise {
				vertex_count - absolute_movement_offset
			} else {
				absolute_movement_offset
			};
			for (terminal_index, &vertex_id) in cycle_data.vertex_indices.iter().enumerate() {
				// Vertices where a clash occurred do not get any path ever
				if is_vertex_blocked[vertex_id] {
					continue;
				}
				// Index [0,n) of the vertices where the animation starts and ends
				let start_index;
				let end_index;
				if self.blocked() {
					// The turn is blocked, so the object will return to its starting vertex
					start_index = terminal_index;
					// end_index = (start_index - movement_offset) % vertex_count
					// but be careful not to underflow
					let mut proposed_end_index =
						vertex_count
							- 1 - (vertex_count - start_index + movement_offset - 1) % vertex_count;
					// Cut the path earlier if there is a wall to hit
					for &wall_position in &wall_hits_by_cycle[cycle_id] {
						let start_and_end_swapped =
							(start_index > proposed_end_index) == is_clockwise;
						let wall_after_start = (wall_position >= start_index) == is_clockwise;
						let wall_before_end = (wall_position < proposed_end_index) == is_clockwise;
						let wall_hit = full_rotations > 0 // The wall would be hit on a full rotation
							|| wall_after_start && wall_before_end // Or if the wall is between start and end
							|| start_and_end_swapped && (wall_after_start || wall_before_end); // If the indices wrapped, also count it as "between"
						if wall_hit {
							// Cut the rotation at the wall
							proposed_end_index = if is_clockwise {
								// One tile after the wall
								(wall_position + 1) % vertex_count
							} else {
								// One tile before the wall
								wall_position
							};
						}
					}
					end_index = proposed_end_index;
				} else {
					// The object will end up at its end vertex
					end_index = terminal_index;
					start_index = (end_index + movement_offset) % vertex_count;
				}
				// Start and end positions along the cycle, [0, 1]
				let start_position = cycle_data.vertex_positions[start_index];
				let end_position = if self.blocked() {
					// For a blocked turn, set the end position
					// half a step before the end vertex
					let pre_end_index = (end_index + vertex_count - (is_clockwise as usize)
						+ (!is_clockwise as usize))
						% vertex_count;
					let mut pre_end_position = cycle_data.vertex_positions[pre_end_index];
					let past_end_position = cycle_data.vertex_positions[end_index];
					let pre_and_post_are_swapped =
						(pre_end_position < past_end_position) == is_clockwise;
					if pre_and_post_are_swapped || vertex_count == 1 {
						pre_end_position += if is_clockwise { 1.0 } else { -1.0 };
					};
					(pre_end_position + past_end_position) / 2.0
				} else {
					cycle_data.vertex_positions[end_index]
				};
				// Adjust the end position so that it accurately reflects
				// the direction and number of full turns relative to starting position
				let adjusted_end_position = {
					let mut full_rotations = full_rotations;
					if self.blocked() {
						// Never make a full rotation when the turn is blocked
						// (the end index is past-the-end, so ending there would count
						// as a full rotation)
						full_rotations = (start_index == end_index) as usize;
					}
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
				let measurements = AnimationPathSegmentMeasurements {
					start_pos: start_position,
					end_pos: adjusted_end_position,
					length: path_length,
				};
				vertex_paths[vertex_id] = Some(AnimationPathSegment {
					owner_cycle: cycle_id,
					measurements,
				});
			}
		}
		vertex_paths
	}

	/// Calculates for each vertex whether objects on it should remain static
	/// because it participates in a clash
	fn jammed_vertex_mask(&self, level_data: &LevelData) -> Vec<bool> {
		let mut is_vertex_blocked = vec![false; level_data.vertices.len()];
		for &clash_index in &self.clashes {
			let (_, _, vertices) = &level_data.forbidden_group_pairs[clash_index];
			for &vertex_index in vertices {
				is_vertex_blocked[vertex_index] = true;
			}
		}
		is_vertex_blocked
	}

	fn jammed_cycle_mask(&self, level_data: &LevelData) -> Vec<bool> {
		let mut is_cycle_blocked = vec![false; level_data.cycles.len()];
		for &clash_index in &self.clashes {
			let &(a, b, _) = &level_data.forbidden_group_pairs[clash_index];
			for group_id in [a, b] {
				for &(cycle_id, _) in &level_data.groups[group_id].cycles {
					is_cycle_blocked[cycle_id] = true;
				}
			}
		}
		is_cycle_blocked
	}

	fn wall_hits_by_cycle(&self, level_data: &LevelData) -> Vec<SmallVec<[usize; 1]>> {
		let mut hits_by_cycle = vec![SmallVec::new(); level_data.cycles.len()];
		for &(cycle_id, offset) in &self.wall_hits {
			hits_by_cycle[cycle_id].push(offset);
		}
		hits_by_cycle
	}
}
