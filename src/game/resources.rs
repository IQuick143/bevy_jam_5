use bevy::{color::palettes, utils::hashbrown::HashMap};

use super::{level::ThingType, prelude::*};

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

/// Contains handles to the materials used for rendering objects and glyphs
#[doc(alias = "ObjectMaterial")]
#[doc(alias = "GlyphMaterial")]
#[derive(Resource, Deref, DerefMut, Debug, Clone, Reflect)]
pub struct ThingColor(HashMap<ThingType, Color>);

impl ThingColor {
	pub fn get(&self, object: ThingType) -> &Color {
		self.0
			.get(&object)
			.expect("All ThingTypes should have a material registered")
	}

	#[allow(dead_code)]
	pub fn get_mut(&mut self, object: ThingType) -> &mut Color {
		self.0
			.get_mut(&object)
			.expect("All ThingTypes should have a material registered")
	}
}

impl Default for ThingColor {
	fn default() -> Self {
		use super::level::GlyphType::*;
		use super::level::ObjectType::*;
		use palettes::tailwind as p;

		let map = [
			(ThingType::Glyph(Button), p::RED_800.into()),
			(ThingType::Glyph(Flag), p::AMBER_400.into()),
			(ThingType::Object(Player), p::BLUE_700.into()),
			(ThingType::Object(Box), p::YELLOW_800.into()),
		]
		.into();
		ThingColor(map)
	}
}
