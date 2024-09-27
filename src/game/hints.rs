//! Handles level-global hints and hover hints for game entities

use super::{level::*, prelude::*, spawn::SpawnLevel};
use crate::{
	graphics::SPRITE_LENGTH,
	ui::hover::{self, *},
};
use bevy::math::bounding::{Aabb2d, BoundingCircle};

pub(super) fn plugin(app: &mut App) {
	app.add_systems(
		LevelInitialization,
		(
			apply_level_hint_text,
			(init_cycle_hover_hints, init_thing_hover_hints)
				.after(LevelInitializationSet::SpawnPrimaryEntities),
		),
	);
}

fn apply_level_hint_text(
	mut events: EventReader<SpawnLevel>,
	mut hint: ResMut<HintText>,
	levels: Res<Assets<LevelData>>,
) {
	if let Some(SpawnLevel(level_handle, _)) = events.read().last() {
		let Some(level) = levels.get(level_handle) else {
			log::warn!("Got an invalid level handle");
			return;
		};
		hint.hint_text.clone_from(&level.hint);
	}
}

fn init_cycle_hover_hints(
	mut commands: Commands,
	query: Query<(Entity, &CycleTurnability), Added<CycleTurnability>>,
) {
	for (id, turnability) in &query {
		commands.entity(id).insert(Hoverable {
			hover_text: match turnability {
				CycleTurnability::Always => hover::CYCLE_AUTOMATIC,
				CycleTurnability::WithPlayer => hover::CYCLE_MANUAL,
				CycleTurnability::Never => hover::CYCLE_STILL,
			},
			hover_bounding_circle: Some(BoundingCircle::new(Vec2::ZERO, SPRITE_LENGTH / 2.0)),
			hover_bounding_box: None,
		});
	}
}

fn init_thing_hover_hints(
	mut commands: Commands,
	query: Query<(Entity, &ThingData), Added<ThingData>>,
) {
	for (id, thing) in &query {
		let (hover_text, bounding_box) = match thing {
			ThingData::Object(ObjectData::Player) => (
				hover::PLAYER,
				Aabb2d::new(
					SPRITE_LENGTH * Vec2::new(0.0, 0.25),
					SPRITE_LENGTH * Vec2::new(0.25, 0.4),
				),
			),
			ThingData::Object(ObjectData::Box(_)) => (
				hover::BOX,
				Aabb2d::new(Vec2::ZERO, Vec2::splat(SPRITE_LENGTH / 4.0)),
			),
			ThingData::Glyph(GlyphData::Flag) => (
				hover::FLAG,
				Aabb2d::new(
					SPRITE_LENGTH * Vec2::new(0.0, 0.08),
					SPRITE_LENGTH * Vec2::new(0.25, 0.30),
				),
			),
			ThingData::Glyph(GlyphData::Button(_)) => (
				hover::BUTTON,
				Aabb2d::new(Vec2::ZERO, Vec2::splat(SPRITE_LENGTH / 3.0)),
			),
		};
		commands.entity(id).insert(Hoverable {
			hover_text,
			hover_bounding_box: Some(bounding_box),
			hover_bounding_circle: None,
		});
	}
}
