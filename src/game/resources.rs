use super::prelude::*;

/// Contains an overview of conditions that are needed to complete the level
#[derive(Resource, Debug, Clone, Copy, Reflect, Default)]
pub struct LevelCompletionConditions {
	pub buttons_present: u32,
	pub buttons_triggered: u32,
	pub flags_present: u32,
	pub flags_occupied: u32,
}

impl LevelCompletionConditions {
	/// Whether the level has been completed
	pub fn is_level_completed(&self) -> bool {
		self.is_goal_unlocked() && self.flags_occupied == self.flags_present
	}

	/// Whether all secondary completion criteria have been met,
	/// and the level will be completed as soon as all players travel to a goal
	pub fn is_goal_unlocked(&self) -> bool {
		self.buttons_present == self.buttons_triggered
	}
}

/// Contains an overview of conditions that are needed to complete the level
#[derive(Resource, Deref, DerefMut, Debug, Clone, Reflect)]
pub struct RingMaterial(pub Handle<ColorMaterial>);

impl FromWorld for RingMaterial {
	fn from_world(world: &mut World) -> Self {
		let mut materials = world
			.get_resource_mut::<Assets<ColorMaterial>>()
			.expect("I'd expect materials to exist pretty please.");

		RingMaterial(materials.add(ColorMaterial {
			color: Color::WHITE,
			..default()
		}))
	}
}
