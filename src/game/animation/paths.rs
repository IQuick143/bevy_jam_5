use super::*;

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
pub struct PathAnimation {
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
	pub fn end_position(&self) -> Vec2 {
		match self {
			Self::Line(_, x) => *x,
			Self::CircleArc(x) => x.center_point + Vec2::from_angle(x.final_angle) * x.radius,
		}
	}

	pub fn length(&self) -> f32 {
		match self {
			Self::Line(x, y) => x.distance(*y),
			Self::CircleArc(x) => (x.initial_angle - x.final_angle).abs() * x.radius,
		}
	}

	pub fn sample(&self, progress: f32) -> Vec2 {
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
	/// in order to make a path that is `full_rotations` plus up to one full circle
	/// in a specified direction
	pub fn final_angle_from_expected_rotation(
		start: f32,
		end: f32,
		mut full_rotations: usize,
		direction: RotationDirection,
	) -> f32 {
		if start != end && (start < end) == (direction == RotationDirection::Clockwise) {
			full_rotations += 1;
		}
		let angle_bias = TAU * full_rotations as f32;
		match direction {
			RotationDirection::Clockwise => end - angle_bias,
			RotationDirection::CounterClockwise => end + angle_bias,
		}
	}
}
