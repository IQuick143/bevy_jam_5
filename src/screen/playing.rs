//! The screen state for the main game loop.

use bevy::{input::common_conditions::input_just_pressed, prelude::*};

use crate::{
	game::{
		assets::{GlobalFont, HandleMap, PlainText},
		events::SpawnLevel,
		level::{self, layout::LevelLayout, ValidLevelData},
		prelude::*,
	},
	ui::prelude::*,
};

use super::*;

pub(super) fn plugin(app: &mut App) {
	app.init_state::<PlayingLevel>()
		.init_resource::<PendingTransition<PlayingLevel>>()
		.add_event::<QueueScreenTransition<PlayingLevel>>()
		.enable_state_scoped_entities::<PlayingLevel>()
		.add_systems(OnEnter(Screen::Playing), (load_level, spawn_game_ui))
		.add_systems(OnExit(Screen::Playing), clear_playing_level_state)
		.add_systems(
			Update,
			(
				process_enqueued_transitions::<PlayingLevel>,
				(
					reload_level.run_if(input_just_pressed(KeyCode::KeyR)),
					game_ui_input_system,
				)
					.run_if(ui_not_frozen),
				load_level.run_if(on_event::<StateTransitionEvent<PlayingLevel>>()),
				update_next_level_button_display.run_if(resource_changed::<IsLevelCompleted>),
			)
				.run_if(in_state(Screen::Playing)),
		)
		.add_systems(
			Update,
			despawn_level_state_scoped.run_if(on_event::<StateTransitionEvent<PlayingLevel>>()),
		);
}

/// Complementary state variable for [`Screen::Playing`]
#[derive(States, Clone, Copy, PartialEq, Eq, Debug, Hash, Default)]
pub struct PlayingLevel(pub Option<LevelID>);

/// Marker component for the next level button
#[derive(Component, Clone, Copy, Debug, Default)]
struct NextLevelButton;

#[derive(Component, Clone, Copy, PartialEq, Eq, Debug)]
enum GameUiAction {
	Back,
	Reset,
	NextLevel,
}

fn reload_level(
	state: Res<State<PlayingLevel>>,
	mut transition: EventWriter<QueueScreenTransition<PlayingLevel>>,
) {
	transition.send(QueueScreenTransition::fade(*state.get()));
}

fn clear_playing_level_state(mut next_state: ResMut<NextState<PlayingLevel>>) {
	next_state.set(PlayingLevel(None));
}

fn spawn_game_ui(mut commands: Commands, font: Res<GlobalFont>) {
	commands
		.ui_root()
		.insert((
			StateScoped(Screen::Playing),
			Style {
				width: Val::Percent(100.0),
				height: Val::Percent(100.0),
				justify_content: JustifyContent::Start,
				align_items: AlignItems::Start,
				flex_direction: FlexDirection::Row,
				column_gap: Val::Px(10.0),
				margin: UiRect::all(Val::Px(10.0)),
				..default()
			},
		))
		.with_children(|parent| {
			parent
				.button("Back", font.0.clone_weak())
				.insert(GameUiAction::Back);
			parent
				.button("Reset", font.0.clone_weak())
				.insert(GameUiAction::Reset);
			parent.button("Next Level", font.0.clone_weak()).insert((
				GameUiAction::NextLevel,
				NextLevelButton,
				InteractionPalette {
					none: ui_palette::NEXT_LEVEL_BUTTON_BACKGROUND,
					hovered: ui_palette::NEXT_LEVEL_BUTTON_HOVER,
					pressed: ui_palette::NEXT_LEVEL_BUTTON_PRESS,
				},
			));
		});
}

fn game_ui_input_system(
	query: InteractionQuery<&GameUiAction>,
	playing_level: Res<State<PlayingLevel>>,
	mut next_screen: EventWriter<QueueScreenTransition<Screen>>,
	mut next_level: EventWriter<QueueScreenTransition<PlayingLevel>>,
) {
	for (interaction, action) in &query {
		if *interaction != Interaction::Pressed {
			continue;
		}
		match action {
			GameUiAction::Back => {
				next_screen.send(QueueScreenTransition::fade(Screen::LevelSelect));
			}
			GameUiAction::Reset => {
				next_level.send(QueueScreenTransition::fade(*playing_level.get()));
			}
			GameUiAction::NextLevel => {
				let playing_level = playing_level
					.get()
					.0
					.expect("When in Screen::Playing state, PlayingLevel must also be set");
				if let Some(next) = playing_level.next_level() {
					next_level.send(QueueScreenTransition::fade(PlayingLevel(Some(next))));
				} else {
					log::warn!("NextLevel action received on the last level");
				}
			}
		}
	}
}

fn update_next_level_button_display(
	is_level_completed: Res<IsLevelCompleted>,
	playing_level: Res<State<PlayingLevel>>,
	mut query: Query<&mut Style, With<NextLevelButton>>,
) {
	let is_last_level = playing_level
		.get()
		.0
		.expect("When in Screen::Playing state, PlayingLevel must also be set")
		.next_level()
		.is_none();
	let display = if is_level_completed.0 && !is_last_level {
		Display::DEFAULT
	} else {
		Display::None
	};
	for mut style in &mut query {
		style.display = display;
	}
}

/// Custom handler for despawning level-scoped entities
/// Unlike the built-in despawner, this despawns everything
/// in all transitions, including identity transitions
fn despawn_level_state_scoped(
	mut commands: Commands,
	query: Query<Entity, With<StateScoped<PlayingLevel>>>,
) {
	for e in &query {
		commands.entity(e).despawn_recursive();
	}
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
