//! The screen state for the main game loop.

use bevy::{input::common_conditions::input_just_pressed, prelude::*};

use crate::game::{
	assets::{HandleMap, PlainText},
	events::SpawnLevel,
	level::{self, layout::LevelLayout, ValidLevelData},
	LevelID,
};

use super::{process_enqueued_transitions, PendingTransition, QueueScreenTransition, Screen};

pub(super) fn plugin(app: &mut App) {
	app.init_state::<PlayingLevel>()
		.init_resource::<PendingTransition<PlayingLevel>>()
		.add_event::<QueueScreenTransition<PlayingLevel>>()
		.enable_state_scoped_entities::<PlayingLevel>()
		.add_systems(OnEnter(Screen::Playing), load_level)
		.add_systems(OnExit(Screen::Playing), clear_playing_level_state)
		.add_systems(
			Update,
			(
				process_enqueued_transitions::<PlayingLevel>,
				return_to_level_select_screen.run_if(input_just_pressed(KeyCode::Escape)),
				load_level.run_if(on_event::<StateTransitionEvent<PlayingLevel>>()),
			)
				.run_if(in_state(Screen::Playing)),
		);
}

/// Complementary state variable for [`Screen::Playing`]
#[derive(States, Clone, Copy, PartialEq, Eq, Debug, Hash, Default)]
pub struct PlayingLevel(pub Option<LevelID>);

fn return_to_level_select_screen(mut next_screen: EventWriter<QueueScreenTransition<Screen>>) {
	next_screen.send(QueueScreenTransition::fade(Screen::LevelSelect));
}

fn clear_playing_level_state(mut next_state: ResMut<NextState<PlayingLevel>>) {
	next_state.set(PlayingLevel(None));
}

fn load_level(
	mut commands: Commands,
	level_handles: Res<HandleMap<LevelID>>,
	level_assets: Res<Assets<PlainText>>,
	playing_level: Res<State<PlayingLevel>>,
	mut next_screen: EventWriter<QueueScreenTransition<Screen>>,
) {
	let level_id = playing_level
		.get()
		.0
		.expect("Systems that transition into Screen::Playing must also set PlayingLevel state");

	let level_handle = level_handles
		.get(&level_id)
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
		commands.trigger(SpawnLevel(level_id, level, level_layout));
	} else {
		// If the level could not be loaded, go back to level select screen
		// (errors have already been reported)
		next_screen.send(QueueScreenTransition::fade(Screen::LevelSelect));
	}
}
