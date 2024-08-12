//! The screen state for the main game loop.

use bevy::{input::common_conditions::input_just_pressed, prelude::*};

use crate::{
	assets::{GlobalFont, LoadedLevelList},
	game::{events::SpawnLevel, level::LevelAsset, prelude::*},
	ui::prelude::*,
};

use super::*;

pub(super) fn plugin(app: &mut App) {
	app.init_state::<PlayingLevel>()
		.init_resource::<PendingTransition<PlayingLevel>>()
		.add_event::<QueueScreenTransition<PlayingLevel>>()
		.enable_state_scoped_entities::<PlayingLevel>()
		.add_systems(
			OnEnter(Screen::Playing),
			(spawn_game_ui, load_level).chain(),
		)
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
/// Stores the index of the currently-played level in the level list
#[derive(States, Clone, Copy, PartialEq, Eq, Debug, Hash, Default)]
pub struct PlayingLevel(pub Option<usize>);

/// Marker component for the next level button
#[derive(Component, Clone, Copy, Debug, Default)]
struct NextLevelButton;

/// Marker component for the level name label
#[derive(Component, Clone, Copy, Debug, Default)]
struct LevelNameBox;

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
	transition.send(QueueScreenTransition::fade(state.get().clone()));
}

fn clear_playing_level_state(mut next_state: ResMut<NextState<PlayingLevel>>) {
	next_state.set(PlayingLevel(None));
}

fn spawn_game_ui(mut commands: Commands, font: Res<GlobalFont>) {
	commands
		.ui_root_justified(JustifyContent::Start)
		.insert(StateScoped(Screen::Playing))
		.with_children(|parent| {
			parent
				.spawn(NodeBundle {
					style: Style {
						width: Val::Percent(100.0),
						flex_direction: FlexDirection::Row,
						column_gap: Val::Px(10.0),
						padding: UiRect::all(Val::Px(10.0)),
						align_items: AlignItems::Start,
						justify_content: JustifyContent::Start,
						..default()
					},
					..default()
				})
				.with_children(|parent| {
					parent
						.tool_button("Back", font.0.clone_weak())
						.insert(GameUiAction::Back);
					parent
						.tool_button("Reset", font.0.clone_weak())
						.insert(GameUiAction::Reset);
				});
			parent
				.spawn(NodeBundle {
					style: Style {
						width: Val::Percent(100.0),
						flex_direction: FlexDirection::Row,
						column_gap: Val::Px(10.0),
						padding: UiRect::all(Val::Px(10.0)),
						align_items: AlignItems::Start,
						justify_content: JustifyContent::End,
						position_type: PositionType::Absolute,
						..default()
					},
					..default()
				})
				.with_children(|parent| {
					parent
						.tool_button("Next Level", font.0.clone_weak())
						.insert((
							GameUiAction::NextLevel,
							NextLevelButton,
							InteractionPalette {
								none: ui_palette::NEXT_LEVEL_BUTTON_BACKGROUND,
								hovered: ui_palette::NEXT_LEVEL_BUTTON_HOVER,
								pressed: ui_palette::NEXT_LEVEL_BUTTON_PRESS,
							},
						));
				});
			parent
				.spawn(NodeBundle {
					style: Style {
						width: Val::Percent(100.0),
						height: Val::Px(50.0),
						margin: UiRect::all(Val::Px(10.0)),
						position_type: PositionType::Absolute,
						justify_content: JustifyContent::Center,
						align_items: AlignItems::Center,
						..default()
					},
					..default()
				})
				.with_children(|parent| {
					parent.spawn((
						LevelNameBox,
						TextBundle {
							text: Text::from_section(
								"", /* Will be set by load_level */
								TextStyle {
									font: font.0.clone_weak(),
									font_size: 35.0,
									color: ui_palette::LABEL_TEXT,
								},
							),
							..default()
						},
					));
				});
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
				next_level.send(QueueScreenTransition::fade(PlayingLevel(Some(
					playing_level + 1,
				))));
			}
		}
	}
}

fn update_next_level_button_display(
	is_level_completed: Res<IsLevelCompleted>,
	playing_level: Res<State<PlayingLevel>>,
	level_list: Res<LoadedLevelList>,
	mut query: Query<&mut Style, With<NextLevelButton>>,
) {
	let level_index = playing_level
		.get()
		.0
		.expect("When in Screen::Playing state, PlayingLevel must also be set");
	let is_last_level = level_index + 1 == level_list.levels.len();
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
fn despawn_level_state_scoped(mut commands: Commands, query: Query<Entity, With<LevelScoped>>) {
	for e in &query {
		commands.entity(e).despawn_recursive();
	}
}

fn load_level(
	mut commands: Commands,
	level_list: Res<LoadedLevelList>,
	level_assets: Res<Assets<LevelAsset>>,
	playing_level: Res<State<PlayingLevel>>,
	mut level_name_q: Query<&mut Text, With<LevelNameBox>>,
) {
	let level_index = playing_level
		.get()
		.0
		.expect("Systems that transition into Screen::Playing must also set PlayingLevel state");
	let level_handle = level_list
		.levels
		.get(level_index)
		.expect("PlayingLevel is out of range");
	let level_data = level_assets
		.get(level_handle)
		.expect("All level handles should be valid");

	// Spawn the level
	level_name_q.single_mut().sections[0]
		.value
		.clone_from(&level_data.name);
	commands.trigger(SpawnLevel(level_handle.clone_weak()));
}
