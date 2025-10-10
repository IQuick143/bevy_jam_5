//! The screen state for the main game loop.

use bevy::{ecs::system::SystemParam, prelude::*};

use crate::{
	assets::{GlobalFont, HandleMap, ImageKey, LoadedLevelList, UiButtonAtlas},
	game::{
		drawing::ThingPalette,
		level::list::{LevelInfo, LevelList},
		prelude::*,
	},
	send_message,
	ui::{consts::*, hover::HoverText, interaction::InteractionEnabled, palette::*, prelude::*},
	AppSet,
};

use super::*;

pub(super) fn plugin(app: &mut App) {
	app.init_state::<PlayingLevel>()
		.add_message::<GameUiAction>()
		.add_fade_message::<LoadLevel>()
		.add_systems(
			OnEnter(Screen::Playing),
			(spawn_game_ui, send_message(LoadLevel)),
		)
		.add_systems(OnExit(Screen::Playing), send_message(EnterLevel(None)))
		.add_systems(
			Update,
			(
				(
					send_message(GameUiAction::Reset).run_if(char_input_pressed('r')),
					send_message(GameUiAction::NextLevel).run_if(
						char_input_pressed('n')
							.and(resource_equals(IsLevelPersistentlyCompleted(true))),
					),
					send_message(GameUiAction::Undo).run_if(
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
					.run_if(on_message::<LoadLevel>),
				(
					update_next_level_button_display,
					update_checkmark_display.before(update_checkmark_margin),
				)
					.run_if(resource_changed::<IsLevelPersistentlyCompleted>),
				update_checkmark_margin,
				start_completion_cue_animation.run_if(
					resource_changed::<IsLevelCompleted>
						.and(resource_equals(IsLevelCompleted(true))),
				),
				tick_completion_cue_animation,
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
#[require(InteractionEnabled(false))]
struct UndoButton;

/// Marker component for the level name label
#[derive(Component, Clone, Copy, Debug, Default)]
struct LevelNameBox;

/// Marker component for the slot where the checkmark will go to indicate completion
#[derive(Component, Clone, Copy, Debug, Default)]
struct LevelCompletionCheckmarkBox {
	/// Progress of the animation where the checkmark appears, [0, 1]
	appear_progress: f32,
}

/// Marker component for the glow effect on the checkmark that indicates level completion
#[derive(Component, Clone, Copy, Debug, Default)]
struct LevelCompletionCheckmarkGlow {
	/// Progress of the level completion cue animation [0, 1]
	progress: f32,
}

#[derive(Message, Component, Clone, Copy, PartialEq, Eq, Debug)]
enum GameUiAction {
	Back,
	Reset,
	NextLevel,
	Undo,
}

/// Message that is sent to signal that the currently selected level should be (re)loaded
#[derive(Message, Component, Clone, Copy, Debug, Default)]
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

fn spawn_game_ui(
	mut commands: Commands,
	font: Res<GlobalFont>,
	images: Res<HandleMap<ImageKey>>,
	button_sprites: Res<UiButtonAtlas>,
	colors: Res<ThingPalette>,
) {
	commands
		.spawn((
			widgets::ui_root_justified(JustifyContent::Start),
			DespawnOnExit(Screen::Playing),
		))
		.with_children(|parent| {
			parent.spawn((
				Node {
					width: Val::Percent(100.0),
					flex_direction: FlexDirection::Row,
					column_gap: COMMON_GAP,
					padding: UiRect::all(COMMON_GAP),
					align_items: AlignItems::Start,
					justify_content: JustifyContent::Start,
					..default()
				},
				children![
					(
						widgets::sprite_button(&button_sprites, UiButtonAtlas::EXIT),
						GameUiAction::Back,
					),
					(
						widgets::sprite_button(&button_sprites, UiButtonAtlas::RESTART),
						GameUiAction::Reset,
					),
					(
						widgets::sprite_button(&button_sprites, UiButtonAtlas::UNDO),
						GameUiAction::Undo,
						UndoButton,
					),
				],
			));
			parent
				.spawn(Node {
					width: Val::Percent(100.0),
					flex_direction: FlexDirection::Row,
					column_gap: COMMON_GAP,
					padding: UiRect::all(COMMON_GAP),
					align_items: AlignItems::Start,
					justify_content: JustifyContent::End,
					position_type: PositionType::Absolute,
					..default()
				})
				.with_children(|parent| {
					parent
						.spawn(widgets::sprite_button(
							&button_sprites,
							UiButtonAtlas::PROCEED,
						))
						.insert((
							GameUiAction::NextLevel,
							NextLevelButton,
							InteractionPalette {
								none: NEXT_LEVEL_BUTTON_BACKGROUND,
								hovered: NEXT_LEVEL_BUTTON_HOVERED_BACKGROUND,
								pressed: NEXT_LEVEL_BUTTON_PRESSED_BACKGROUND,
								disabled: BUTTON_DISABLED_BACKGROUND,
							},
						));
				});
			parent.spawn((
				Node {
					width: Val::Percent(100.0),
					height: LEVEL_TITLE_HEIGHT,
					margin: UiRect::all(COMMON_GAP),
					position_type: PositionType::Absolute,
					justify_content: JustifyContent::Center,
					align_items: AlignItems::Center,
					column_gap: Val::Px(LEVEL_TITLE_CHECK_GAP),
					..default()
				},
				children![
					(
						LevelNameBox,
						Text::default(),
						TextFont {
							font: font.0.clone(),
							font_size: LEVEL_TITLE_SIZE,
							..default()
						},
						TextColor(ui_palette::LABEL_TEXT),
					),
					(
						LevelCompletionCheckmarkBox::default(),
						Node {
							width: Val::Px(LEVEL_TITLE_SIZE),
							height: Val::Px(LEVEL_TITLE_SIZE),
							..default()
						},
						ImageNode {
							image: images[&ImageKey::Checkmark].clone(),
							color: colors.checkmark,
							image_mode: NodeImageMode::Stretch,
							..default()
						},
					),
				],
			));
		});

	commands.spawn((
		widgets::ui_root_justified(JustifyContent::End),
		DespawnOnExit(Screen::Playing),
		children![(
			Node {
				width: Val::Vh(100.0),
				min_width: Val::Vw(50.0),
				max_width: Val::Vw(100.0),
				padding: UiRect::all(COMMON_GAP),
				justify_content: JustifyContent::Center,
				..default()
			},
			children![(
				HoverText,
				Text::default(),
				TextFont {
					font: font.0.clone(),
					font_size: HOVER_HINT_TEXT_SIZE,
					..default()
				},
				TextLayout::new_with_justify(Justify::Center),
				TextColor(ui_palette::LABEL_TEXT),
			)],
		)],
	));
}

fn game_ui_input_recording_system(
	query: InteractionQuery<&GameUiAction>,
	mut events: MessageWriter<GameUiAction>,
) {
	for (interaction, enabled, action) in &query {
		if enabled.is_none_or(|e| **e) && *interaction == Interaction::Pressed {
			events.write(*action);
		}
	}
}

fn game_ui_input_processing_system(
	mut events: MessageReader<GameUiAction>,
	playing_level: PlayingLevelListEntry,
	mut commands: Commands,
	mut next_level: ResMut<NextState<PlayingLevel>>,
	mut undo_commands: MessageWriter<UndoMove>,
) {
	if let Some(action) = events.read().last() {
		match action {
			GameUiAction::Back => {
				commands.do_screen_transition(Screen::LevelSelect);
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
	is_completed_this_session: Res<IsLevelCompleted>,
	mut query: Query<(&mut Node, &mut ImageNode, &mut LevelCompletionCheckmarkBox)>,
) {
	for (mut node, mut image, mut checkmark) in &mut query {
		if **is_level_completed {
			// The full animation should only be played if the level was just completed
			// (entering a level that had been completed in a previous session does not count)
			// and the checkmark was not displayed before
			if **is_completed_this_session && node.display == Display::None {
				checkmark.appear_progress = 0.0;
			} else {
				checkmark.appear_progress = 1.0;
				// The system that animates the margins will not run this way,
				// so set the mrgin here instead
				node.margin.left = Val::Px(0.0);
				node.margin.right = Val::Px(0.0);
				image.color.set_alpha(1.0);
			}
			// Always show the checkmark on completed levels
			node.display = Display::DEFAULT;
		} else {
			node.display = Display::None;
			checkmark.appear_progress = 1.0;
		}
	}
}

/// How long it takes for a checkmark to fully appear
const CHECKMARK_APPEAR_TIME: f32 = 0.2;

fn update_checkmark_margin(
	mut query: Query<(&mut Node, &mut ImageNode, &mut LevelCompletionCheckmarkBox)>,
	time: Res<Time>,
) {
	for (mut node, mut image, mut checkmark) in &mut query {
		// Do nothing to checkboxes that are already fully animated
		if checkmark.appear_progress >= 1.0 {
			continue;
		}
		let delta = time.delta_secs() / CHECKMARK_APPEAR_TIME;
		checkmark.appear_progress = (checkmark.appear_progress + delta).min(1.0);
		let animation_progress = checkmark_margin_animation_curve(checkmark.appear_progress);
		let margin = (LEVEL_TITLE_SIZE + LEVEL_TITLE_CHECK_GAP) * (1.0 - animation_progress);
		// Split the margin between the sides so that the checkmark stays in place
		// while the level title moves away
		node.margin.left = Val::Px(-margin / 2.0);
		node.margin.right = Val::Px(-margin / 2.0);
		// Fade in the checkmark
		image.color.set_alpha(animation_progress);
	}
}

fn checkmark_margin_animation_curve(x: f32) -> f32 {
	1.0 - (1.0 - x).powi(2)
}

fn start_completion_cue_animation(
	query: Query<Entity, With<LevelCompletionCheckmarkBox>>,
	images: Res<HandleMap<ImageKey>>,
	colors: Res<ThingPalette>,
	mut commands: Commands,
) {
	for node_id in &query {
		commands.entity(node_id).with_child((
			LevelCompletionCheckmarkGlow::default(),
			Node {
				position_type: PositionType::Absolute,
				width: Val::Percent(100.0),
				height: Val::Percent(100.0),
				..default()
			},
			ImageNode {
				image: images[&ImageKey::CheckmarkSolid].clone(),
				color: colors.checkmark,
				image_mode: NodeImageMode::Stretch,
				..default()
			},
		));
	}
}

/// How long the animation of level completion cue takes, in seconds
const CHECKMARK_GLOW_TIME: f32 = 0.5;

fn tick_completion_cue_animation(
	mut query: Query<(
		Entity,
		&mut Node,
		&mut ImageNode,
		&mut LevelCompletionCheckmarkGlow,
	)>,
	mut commands: Commands,
	time: Res<Time>,
) {
	for (node_id, mut node, mut image, mut data) in &mut query {
		data.progress += time.delta_secs() / CHECKMARK_GLOW_TIME;
		if data.progress >= 1.0 {
			// Despawn the animation entity once the animation is complete
			commands.entity(node_id).despawn();
		} else {
			// Update the entity to reflect the animation progress
			image.color.set_alpha(1.0 - data.progress);
			let glow_size = 150.0 * checkmark_margin_animation_curve(data.progress);
			node.width = Val::Percent(100.0 + glow_size);
			node.height = Val::Percent(100.0 + glow_size);
			node.left = Val::Percent(-glow_size / 2.0);
			node.top = Val::Percent(-glow_size / 2.0);
		}
	}
}

fn update_undo_button_display(
	history: Res<MoveHistory>,
	mut query: Query<&mut InteractionEnabled, With<UndoButton>>,
) {
	for mut enabled in &mut query {
		enabled.set_if_neq(InteractionEnabled(!history.is_empty()));
	}
}

fn update_level_name_display(
	mut events: MessageReader<EnterLevel>,
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

fn load_level(playing_level: PlayingLevelListEntry, mut events: MessageWriter<EnterLevel>) {
	let level_handle = playing_level
		.get()
		.expect("load_level called but current level could not be loaded")
		.data_handle
		.clone();
	events.write(EnterLevel(Some(level_handle)));
}
