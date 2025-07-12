//! The screen state for the main game loop.

use bevy::{ecs::system::SystemParam, prelude::*};

use crate::{
	assets::{GlobalFont, HandleMap, ImageKey, LoadedLevelList},
	game::{
		drawing::ThingPalette,
		level::list::{LevelInfo, LevelList},
		prelude::*,
	},
	send_event,
	ui::{hover::HoverText, prelude::*},
	AppSet,
};

use super::*;

pub(super) fn plugin(app: &mut App) {
	app.init_state::<PlayingLevel>()
		.add_event::<GameUiAction>()
		.add_fade_event::<LoadLevel>()
		.add_systems(
			OnEnter(Screen::Playing),
			(spawn_game_ui, send_event(LoadLevel)),
		)
		.add_systems(OnExit(Screen::Playing), send_event(EnterLevel(None)))
		.add_systems(
			Update,
			(
				(
					send_event(GameUiAction::Reset).run_if(char_input_pressed('r')),
					send_event(GameUiAction::NextLevel).run_if(
						char_input_pressed('n')
							.and(resource_equals(IsLevelPersistentlyCompleted(true))),
					),
					send_event(GameUiAction::Undo).run_if(
						char_input_pressed('z')
							.and(|history: Res<MoveHistory>| !history.is_empty()),
					),
					game_ui_input_recording_system,
				)
					.run_if(ui_not_frozen)
					.in_set(AppSet::RecordInput),
				game_ui_input_processing_system.in_set(AppSet::ExecuteInput),
				(load_level, update_level_name_display)
					.chain()
					.run_if(on_event::<LoadLevel>),
				(update_next_level_button_display, update_checkmark_display)
					.run_if(resource_changed::<IsLevelPersistentlyCompleted>),
				update_undo_button_display.run_if(resource_changed::<MoveHistory>),
			)
				.run_if(in_state(Screen::Playing)),
		);
}

/// Complementary state variable for [`Screen::Playing`]
/// Stores the index of the currently-played level in the level list
#[derive(States, Clone, Copy, PartialEq, Eq, Debug, Hash, Default)]
pub struct PlayingLevel(pub Option<usize>);

/// Marker component for the next level button
#[derive(Component, Clone, Copy, Debug, Default)]
struct NextLevelButton;

/// Marker component for the undo button
#[derive(Component, Clone, Copy, Debug, Default)]
struct UndoButton;

/// Marker component for the level name label
#[derive(Component, Clone, Copy, Debug, Default)]
struct LevelNameBox;

/// Marker component for the slot where the checkmark will go to indicate completion
#[derive(Component, Clone, Copy, Debug, Default)]
struct LevelCompletionCheckmarkBox;

#[derive(Event, Component, Clone, Copy, PartialEq, Eq, Debug)]
enum GameUiAction {
	Back,
	Reset,
	NextLevel,
	Undo,
}

/// Event that is sent to signal that the currently selected level should be (re)loaded
#[derive(Event, Component, Clone, Copy, Debug, Default)]
pub struct LoadLevel;

/// [`SystemParam`] that provides a reference to the entry
/// of the level list that describes the level being played
#[derive(SystemParam)]
pub struct PlayingLevelListEntry<'w> {
	level_list: Res<'w, LoadedLevelList>,
	level_list_asset: Res<'w, Assets<LevelList>>,
	playing_level: Res<'w, State<PlayingLevel>>,
}

impl PlayingLevelListEntry<'_> {
	/// Gets the level info entry of the current level
	pub fn get(&self) -> Result<&LevelInfo, BevyError> {
		let level_index = self.playing_level.get().0.ok_or_else(|| {
			"Systems that transition into Screen::Playing must also set PlayingLevel state"
				.to_owned()
		})?;
		let level_info = self
			.level_list_asset
			.get(&self.level_list.0)
			.ok_or_else(|| "The LevelList asset should be valid".to_owned())?
			.levels
			.get(level_index)
			.ok_or_else(|| "PlayingLevel is out of range".to_owned())?;
		Ok(level_info)
	}
}

/// Size of the level title label on the playing screen
const LEVEL_TITLE_SIZE: f32 = 35.0;

fn spawn_game_ui(
	mut commands: Commands,
	font: Res<GlobalFont>,
	images: Res<HandleMap<ImageKey>>,
	colors: Res<ThingPalette>,
) {
	commands
		.ui_root_justified(JustifyContent::Start)
		.insert(StateScoped(Screen::Playing))
		.with_children(|parent| {
			parent
				.spawn(Node {
					width: Val::Vw(100.0),
					flex_direction: FlexDirection::Row,
					column_gap: Val::Px(10.0),
					padding: UiRect::all(Val::Px(10.0)),
					align_items: AlignItems::Start,
					justify_content: JustifyContent::Start,
					..default()
				})
				.with_children(|parent| {
					parent
						.tool_button("Back", font.0.clone_weak())
						.insert(GameUiAction::Back);
					parent
						.tool_button("Reset", font.0.clone_weak())
						.insert(GameUiAction::Reset);
					parent
						.tool_button("Undo", font.0.clone_weak())
						.insert((GameUiAction::Undo, UndoButton));
				});
			parent
				.spawn(Node {
					width: Val::Vw(100.0),
					flex_direction: FlexDirection::Row,
					column_gap: Val::Px(10.0),
					padding: UiRect::all(Val::Px(10.0)),
					align_items: AlignItems::Start,
					justify_content: JustifyContent::End,
					position_type: PositionType::Absolute,
					..default()
				})
				.with_children(|parent| {
					parent
						.tool_button("Next Level", font.0.clone_weak())
						.insert((
							GameUiAction::NextLevel,
							NextLevelButton,
							BackgroundColor(ui_palette::NEXT_LEVEL_BUTTON_BACKGROUND),
							InteractionPalette {
								none: ui_palette::NEXT_LEVEL_BUTTON_BACKGROUND,
								hovered: ui_palette::NEXT_LEVEL_BUTTON_HOVER,
								pressed: ui_palette::NEXT_LEVEL_BUTTON_PRESS,
							},
						));
				});
			parent
				.spawn(Node {
					width: Val::Vw(100.0),
					height: Val::Vh(10.0),
					margin: UiRect::all(Val::Px(10.0)),
					position_type: PositionType::Absolute,
					justify_content: JustifyContent::Center,
					align_items: AlignItems::Center,
					column_gap: Val::Px(15.0),
					..default()
				})
				.with_children(|parent| {
					parent.spawn((
						LevelNameBox,
						Text::default(),
						TextFont {
							font: font.0.clone_weak(),
							font_size: LEVEL_TITLE_SIZE,
							..default()
						},
						TextColor(ui_palette::LABEL_TEXT),
					));
					parent.spawn((
						LevelCompletionCheckmarkBox,
						Node {
							width: Val::Px(LEVEL_TITLE_SIZE),
							height: Val::Px(LEVEL_TITLE_SIZE),
							..default()
						},
						ImageNode {
							image: images[&ImageKey::Checkmark].clone_weak(),
							color: colors.checkmark,
							image_mode: NodeImageMode::Stretch,
							..default()
						},
					));
				});
		});

	commands
		.ui_root_justified(JustifyContent::End)
		.insert(StateScoped(Screen::Playing))
		.with_children(|parent| {
			parent
				.spawn(Node {
					width: Val::Vh(100.0),
					min_width: Val::Vw(50.0),
					max_width: Val::Vw(100.0),
					padding: UiRect::all(Val::Px(10.0)),
					justify_content: JustifyContent::Center,
					..default()
				})
				.with_child((
					HoverText,
					Text::default(),
					TextFont {
						font: font.0.clone_weak(),
						font_size: 20.0,
						..default()
					},
					TextLayout::new_with_justify(JustifyText::Center),
					TextColor(ui_palette::LABEL_TEXT),
				));
		});
}

fn game_ui_input_recording_system(
	query: InteractionQuery<&GameUiAction>,
	mut events: EventWriter<GameUiAction>,
) {
	for (interaction, action) in &query {
		if *interaction == Interaction::Pressed {
			events.write(*action);
		}
	}
}

fn game_ui_input_processing_system(
	mut events: EventReader<GameUiAction>,
	playing_level: PlayingLevelListEntry,
	mut commands: Commands,
	mut next_level: ResMut<NextState<PlayingLevel>>,
	mut undo_commands: EventWriter<UndoMove>,
) {
	if let Some(action) = events.read().last() {
		match action {
			GameUiAction::Back => {
				commands.spawn((
					FadeAnimationBundle::default(),
					DoScreenTransition(Screen::LevelSelect),
				));
			}
			GameUiAction::Reset => {
				commands.spawn((FadeAnimationBundle::default(), LoadLevel));
			}
			GameUiAction::NextLevel => {
				let next_level_id = playing_level.get().ok().and_then(|l| l.next_level);
				if next_level_id.is_none() {
					log::warn!("Received a Next level action on a level without a successor");
				} else {
					next_level.set(PlayingLevel(next_level_id));
					commands.spawn((FadeAnimationBundle::default(), LoadLevel));
				}
			}
			GameUiAction::Undo => {
				undo_commands.write(UndoMove);
			}
		}
	}
}

fn update_next_level_button_display(
	is_level_completed: Res<IsLevelPersistentlyCompleted>,
	playing_level: PlayingLevelListEntry,
	mut query: Query<&mut Node, With<NextLevelButton>>,
) {
	let next_level = playing_level.get().ok().and_then(|l| l.next_level);
	let display = if is_level_completed.0 && next_level.is_some() {
		Display::DEFAULT
	} else {
		Display::None
	};
	for mut node in &mut query {
		node.display = display;
	}
}

fn update_checkmark_display(
	is_level_completed: Res<IsLevelPersistentlyCompleted>,
	mut query: Query<&mut Node, With<LevelCompletionCheckmarkBox>>,
) {
	let display = if is_level_completed.0 {
		Display::DEFAULT
	} else {
		Display::None
	};
	for mut node in &mut query {
		node.display = display;
	}
}

fn update_undo_button_display(
	history: Res<MoveHistory>,
	mut query: Query<&mut Node, With<UndoButton>>,
) {
	let display = if history.is_empty() {
		Display::None
	} else {
		Display::DEFAULT
	};
	for mut node in &mut query {
		node.display = display;
	}
}

fn update_level_name_display(
	mut events: EventReader<EnterLevel>,
	mut level_name_q: Query<&mut Text, With<LevelNameBox>>,
	level_assets: Res<Assets<LevelData>>,
) {
	if let Some(level_handle) = events.read().last().and_then(|e| e.0.as_ref()) {
		let level_data = level_assets
			.get(level_handle)
			.expect("Got an invalid level handle");
		level_name_q
			.single_mut()
			.unwrap()
			.0
			.clone_from(&level_data.name);
	}
}

fn load_level(playing_level: PlayingLevelListEntry, mut events: EventWriter<EnterLevel>) {
	let level_handle = playing_level
		.get()
		.expect("load_level called but current level could not be loaded")
		.data_handle
		.clone_weak();
	events.write(EnterLevel(Some(level_handle)));
}
