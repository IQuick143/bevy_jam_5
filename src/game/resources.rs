use bevy::color::palettes;

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

/// Contains a handle to the material used for rendering the cycle rings
#[derive(Resource, Deref, DerefMut, Debug, Clone, Reflect)]
pub struct RingMaterial(pub Handle<ColorMaterial>);

impl FromWorld for RingMaterial {
	fn from_world(world: &mut World) -> Self {
		let mut materials = world
			.get_resource_mut::<Assets<ColorMaterial>>()
			.expect("I'd expect materials to exist pretty please.");

		RingMaterial(materials.add(ColorMaterial {
			color: palettes::tailwind::GRAY_400.into(),
			..default()
		}))
	}
}

/// Contains colors used for rendering objects and glyphs
#[derive(Resource, Debug, Clone, Reflect)]
pub struct ThingPalette {
	pub box_base: Color,
	pub box_trigger: Color,
	pub button_base: Color,
	pub button_trigger: Color,
	pub player: Color,
	pub goal_closed: Color,
	pub goal_open: Color,
	pub cycle_disabled: Color,
	pub cycle_ready: Color,
}

impl Default for ThingPalette {
	fn default() -> Self {
		use palettes::tailwind as p;
		Self {
			box_base: p::ROSE_100.into(),
			box_trigger: p::ROSE_500.into(),
			button_base: p::ROSE_100.into(),
			button_trigger: p::ROSE_500.into(),
			player: p::SKY_100.into(),
			goal_closed: p::SKY_100.into(),
			goal_open: p::GREEN_500.into(),
			cycle_disabled: p::GRAY_200.into(),
			cycle_ready: p::GRAY_300.into(),
		}
	}
}
