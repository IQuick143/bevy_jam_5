//! Handles level-global hints and hover hints for game entities

use super::{level::*, prelude::*, spawn::SpawnLevel};
use crate::{
	game::components::{Detector, Wall},
	graphics::SPRITE_LENGTH,
	ui::hover::{self, *},
};
use bevy::math::bounding::Aabb2d;

pub(super) fn plugin(app: &mut App) {
	app.add_systems(
		LevelInitialization,
		(
			apply_level_hint_text,
			(
				init_cycle_hover_hints,
				init_thing_hover_hints,
				init_wall_and_detector_hint,
			)
				.after(LevelInitializationSet::SpawnPrimaryEntities),
		),
	);
}

fn apply_level_hint_text(
	mut events: MessageReader<SpawnLevel>,
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
		commands.entity(id).insert((
			HoverHint(match turnability {
				CycleTurnability::Always => hover::CYCLE_AUTOMATIC,
				CycleTurnability::WithPlayer => hover::CYCLE_MANUAL,
				CycleTurnability::Never => hover::CYCLE_STILL,
			}),
			HoverPriority(hover::prio::CYCLE),
			// Hover hint inputs for cycles are handled together
			// with their game inputs
		));
	}
}

fn init_thing_hover_hints(
	mut commands: Commands,
	query: Query<(Entity, &ThingData), Added<ThingData>>,
) {
	for (id, thing) in &query {
		let (hover_text, bounding_box, priority) = match thing {
			ThingData::Object(ObjectData::Player) => (
				hover::PLAYER,
				Aabb2d::new(
					SPRITE_LENGTH * Vec2::new(0.0, 0.25),
					SPRITE_LENGTH * Vec2::new(0.22, 0.375),
				),
				hover::prio::OBJECT,
			),
			ThingData::Object(ObjectData::Box(_)) => (
				hover::BOX,
				Aabb2d::new(Vec2::ZERO, Vec2::splat(SPRITE_LENGTH / 4.0)),
				hover::prio::OBJECT,
			),
			ThingData::Glyph(GlyphData::Flag) => (
				hover::FLAG,
				Aabb2d::new(
					SPRITE_LENGTH * Vec2::new(0.0, 0.08),
					SPRITE_LENGTH * Vec2::new(0.25, 0.30),
				),
				hover::prio::GLYPH,
			),
			ThingData::Glyph(GlyphData::Button(_)) => (
				hover::BUTTON,
				Aabb2d::new(Vec2::ZERO, Vec2::splat(SPRITE_LENGTH / 3.0)),
				hover::prio::GLYPH,
			),
		};
		commands.entity(id).insert((
			HoverHint(hover_text),
			HoverHintBoundingRect(bounding_box),
			HoverPriority(priority),
		));
	}
}

fn init_wall_and_detector_hint(
	mut commands: Commands,
	w_query: Query<Entity, With<Wall>>,
	d_query: Query<Entity, With<Detector>>,
) {
	let bounding_box = Aabb2d::new(Vec2::ZERO, SPRITE_LENGTH * Vec2::new(0.120, 0.4));
	for wall in w_query.iter() {
		commands.entity(wall).insert((
			HoverHint(hover::WALL),
			HoverHintBoundingRect(bounding_box),
			HoverPriority(hover::prio::DETECTOR),
		));
	}
	for detector in d_query.iter() {
		commands.entity(detector).insert((
			HoverHint(hover::DETECTOR),
			HoverHintBoundingRect(bounding_box),
			HoverPriority(hover::prio::DETECTOR),
		));
	}
}
