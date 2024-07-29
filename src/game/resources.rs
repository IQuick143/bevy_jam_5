use bevy::color::palettes;

use super::{level::LOGICAL_COLORS, prelude::*};

pub(super) fn plugin(app: &mut App) {
	app.init_resource::<LevelCompletionConditions>()
		.init_resource::<IsLevelCompleted>()
		.init_resource::<IsGoalUnlocked>()
		.init_resource::<RingMaterial>()
		.init_resource::<LinkMaterial>()
		.init_resource::<ThingPalette>();
}

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

/// Contains an information whether the level being played has been completed
/// in this session (making moves after completion does not matter)
#[derive(Resource, Clone, Copy, PartialEq, Eq, Debug, Default)]
pub struct IsLevelCompleted(pub bool);

/// Reflects the value of [`LevelCompletionConditions::is_goal_unlocked`].
/// Once the level is completed, locks to true.
#[derive(Resource, Clone, Copy, PartialEq, Eq, Debug, Default)]
pub struct IsGoalUnlocked(pub bool);

/// Contains an overview of conditions that are needed to complete the level
#[derive(Resource, Debug, Clone, Copy, Reflect, Default)]
pub struct HintText {
	pub hover_text: Option<&'static str>,
	pub hint_text: Option<&'static str>,
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
			color: palettes::tailwind::SLATE_400.into(),
			..default()
		}))
	}
}

/// Contains a handle to the material used for rendering cycle links
#[derive(Resource, Deref, DerefMut, Debug, Clone, Reflect)]
pub struct LinkMaterial(pub Handle<ColorMaterial>);

impl FromWorld for LinkMaterial {
	fn from_world(world: &mut World) -> Self {
		let mut materials = world
			.get_resource_mut::<Assets<ColorMaterial>>()
			.expect("I'd expect materials to exist pretty please.");

		LinkMaterial(materials.add(ColorMaterial {
			color: palettes::tailwind::SLATE_300.into(),
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
	pub cycle_trigger: Color,
	pub colored_base: [Color; LOGICAL_COLORS],
	pub colored_trigger: [Color; LOGICAL_COLORS],
}

impl Default for ThingPalette {
	fn default() -> Self {
		use palettes::tailwind as p;
		Self {
			box_base: p::ORANGE_200.into(),
			box_trigger: p::GREEN_500.into(),
			button_base: p::ORANGE_200.into(),
			button_trigger: p::GREEN_500.into(),
			player: p::SLATE_200.into(),
			goal_closed: p::SLATE_100.into(),
			goal_open: p::GREEN_500.into(),
			cycle_disabled: p::SLATE_200.into(),
			cycle_ready: p::SLATE_300.into(),
			cycle_trigger: p::SLATE_400.into(),
			colored_base: [
				p::ROSE_700.into(),
				p::YELLOW_700.into(),
				p::INDIGO_700.into(),
				p::FUCHSIA_700.into(),
				p::SLATE_500.into(),
				p::GREEN_700.into(),
			],
			colored_trigger: [
				p::ROSE_500.into(),
				p::YELLOW_500.into(),
				p::INDIGO_500.into(),
				p::FUCHSIA_500.into(),
				p::SLATE_100.into(),
				p::GREEN_500.into(),
			],
		}
	}
}
