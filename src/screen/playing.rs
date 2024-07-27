//! The screen state for the main game loop.

use bevy::{input::common_conditions::input_just_pressed, prelude::*};

use crate::game::{
	assets::{HandleMap, LevelID, PlainText},
	events::SpawnLevel,
	level::{self, layout::LevelLayout, ValidLevelData},
};

use super::{QueueScreenTransition, Screen};

pub(super) fn plugin(app: &mut App) {
	app.add_systems(
		Update,
		(
			return_to_level_select_screen
				.run_if(is_on_level_screen.and_then(input_just_pressed(KeyCode::Escape))),
			load_level.run_if(on_event::<StateTransitionEvent<Screen>>()),
		),
	);
}

fn is_on_level_screen(s: Option<Res<State<Screen>>>) -> bool {
	s.is_some_and(|s| matches!(s.get(), Screen::Level(_)))
}

fn return_to_level_select_screen(mut next_screen: EventWriter<QueueScreenTransition>) {
	next_screen.send(QueueScreenTransition::fade(Screen::LevelSelect));
}

fn load_level(
	mut commands: Commands,
	level_handles: Res<HandleMap<LevelID>>,
	level_assets: Res<Assets<PlainText>>,
	mut event: EventReader<StateTransitionEvent<Screen>>,
	mut next_screen: EventWriter<QueueScreenTransition>,
) {
	let Some(StateTransitionEvent {
		exited: _,
		entered: Some(Screen::Level(level_id)),
	}) = event.read().last()
	else {
		return;
	};

	let level_handle = level_handles
		.get(level_id)
		.expect("All level IDs should be assigned a level");
	let level_data = level_assets
		.get(level_handle)
		.expect("Handle should have been created by insertion into this");

	fn prepare_level(level_data: &str) -> Result<(ValidLevelData, LevelLayout), ()> {
		let level_file = level::parser::parse(level_data)
			.map_err(|err| log::error!("Failed to parse level file: {err}"))?;
		let level = level_file
			.data
			.try_into()
			.map_err(|err| log::error!("Level file validation failed: {err}"))?;
		let mut layout_builder = level::layout::LevelLayoutBuilder::new(&level);
		for placement in level_file.layout {
			layout_builder
				.add_placement(placement)
				.map_err(|err| log::error!("Level layout could not be applied: {err}"))?;
		}
		let level_layout = layout_builder
			.build()
			.map_err(|err| log::error!("Level layout could not be applied: {err}"))?;
		Ok((level, level_layout))
	}

	if let Ok((level, level_layout)) = prepare_level(level_data) {
		// Spawn the level
		commands.trigger(SpawnLevel(level, level_layout));
	} else {
		// If the level could not be loaded, go back to level select screen
		// (errors have already been reported)
		next_screen.send(QueueScreenTransition::fade(Screen::LevelSelect));
	}
}
