use bevy::prelude::*;

use super::*;
use crate::{
	assets::{GlobalFont, HandleMap, ImageKey, LoadedLevelList, UiButtonAtlas},
	drawing::{ColorKey, NodeColorKey},
	game::level::{
		completion::{CompletionStatus, LevelHubCompletion},
		list::LevelList,
		LevelData,
	},
	save::SaveGame,
	ui::{consts::*, prelude::*, scrollbox::Scrollbox},
};

pub(super) fn plugin(app: &mut App) {
	app.add_systems(OnEnter(Screen::LevelSelect), spawn_screen)
		.add_systems(
			Update,
			handle_level_select_screen_action
				.run_if(in_state(Screen::LevelSelect).and(ui_not_frozen)),
		);
}

#[derive(Component, Clone, PartialEq, Eq, Debug)]
enum LevelSelectAction {
	Back,
	PlayLevel(usize),
}

fn spawn_screen(
	mut commands: Commands,
	levels: Res<LoadedLevelList>,
	font: Res<GlobalFont>,
	level_assets: Res<Assets<LevelData>>,
	level_list_asset: Res<Assets<LevelList>>,
	save: Res<SaveGame>,
	images: Res<HandleMap<ImageKey>>,
	button_sprites: Res<UiButtonAtlas>,
) {
	let levels = level_list_asset
		.get(&levels.0)
		.expect("The LevelList asset should be valid");
	let root_id = commands
		.spawn((
			widgets::ui_root_justified(JustifyContent::Center),
			DespawnOnExit(Screen::LevelSelect),
		))
		.id();
	let main_id = commands
		.spawn((
			Scrollbox {
				step: COMMON_GAP_PX + GRID_BUTTON_HEIGHT_PX,
			},
			Node {
				flex_direction: FlexDirection::Column,
				justify_content: JustifyContent::Start,
				align_content: AlignContent::Center,
				width: Val::Percent(90.0),
				padding: UiRect::vertical(Val::Percent(5.0)),
				row_gap: COMMON_GAP,
				overflow: Overflow::scroll_y(),
				..default()
			},
			ChildOf(root_id),
		))
		.id();
	commands.spawn((
		Node {
			margin: TOOLBAR_MARGIN,
			..default()
		},
		DespawnOnExit(Screen::LevelSelect),
		children![(
			widgets::sprite_button(&button_sprites, UiButtonAtlas::EXIT),
			LevelSelectAction::Back,
		)],
	));

	let hub_completion = LevelHubCompletion::from_save(levels, &save);
	for (hub_id, hub) in levels.hubs.iter().enumerate() {
		if hub.levels.is_empty() || !hub_completion.is_hub_unlocked(levels, hub_id) {
			continue;
		}

		let hub_title_wrapper = commands
			.spawn((
				Node {
					padding: UiRect::top(COMMON_GAP),
					justify_content: JustifyContent::Center,
					..default()
				},
				ChildOf(main_id),
			))
			.id();
		let mut hub_title = commands.spawn((
			Node {
				justify_content: JustifyContent::Center,
				padding: UiRect::horizontal(px(
					LEVEL_COMPLETED_MARKER_SIZE_PX + LEVEL_COMPLETED_MARKER_MARGIN_PX
				)),
				..default()
			},
			ChildOf(hub_title_wrapper),
			children![(
				Name::new("Label Text"),
				Text::new(hub.hub_name.clone()),
				TextFont {
					font_size: COMMON_TEXT_SIZE,
					font: font.0.clone(),
					..default()
				},
				TextColor(ui_palette::LABEL_TEXT),
			)],
		));
		let completion_status = hub_completion.hub_completion_status(hub_id);
		if completion_status >= CompletionStatus::Completed {
			let image_key = match completion_status {
				CompletionStatus::Completed => ImageKey::Checkmark,
				CompletionStatus::Cleared => ImageKey::Star,
				CompletionStatus::Started => unreachable!(),
			};
			hub_title.with_child((
				Name::new("Hub Completed Marker"),
				Node {
					width: LEVEL_COMPLETED_MARKER_SIZE,
					height: LEVEL_COMPLETED_MARKER_SIZE,
					position_type: PositionType::Absolute,
					bottom: px(0),
					right: px(0),
					..default()
				},
				ImageNode {
					image: images[&image_key].clone(),
					image_mode: NodeImageMode::Stretch,
					..default()
				},
				NodeColorKey(ColorKey::Checkmark),
			));
		}

		commands
			.spawn((
				Node {
					display: Display::Flex,
					flex_wrap: FlexWrap::Wrap,
					column_gap: COMMON_GAP,
					row_gap: COMMON_GAP,
					width: Val::Percent(100.0),
					justify_content: JustifyContent::Center,
					align_content: AlignContent::Center,
					..default()
				},
				ChildOf(main_id),
			))
			.with_children(|parent| {
				for &level_id in &hub.levels {
					if !hub_completion.is_level_unlocked(levels, level_id) {
						continue;
					}

					let level_meta = &levels.levels[level_id];
					if let Some(level) = level_assets.get(&level_meta.data_handle) {
						let mut button = parent.spawn((
							widgets::grid_button(level.name.clone(), font.0.clone()),
							LevelSelectAction::PlayLevel(level_id),
						));
						if save.is_level_completed(&level_meta.identifier) {
							button.with_child((
								Name::new("Level Completed Marker"),
								Node {
									width: LEVEL_COMPLETED_MARKER_SIZE,
									height: LEVEL_COMPLETED_MARKER_SIZE,
									position_type: PositionType::Absolute,
									bottom: LEVEL_COMPLETED_MARKER_MARGIN,
									right: LEVEL_COMPLETED_MARKER_MARGIN,
									..default()
								},
								ImageNode {
									image: images[&ImageKey::Checkmark].clone(),
									image_mode: NodeImageMode::Stretch,
									..default()
								},
								NodeColorKey(ColorKey::Checkmark),
							));
						} else {
							// Make the button a different color to indicate it's new
							button.insert(InteractionPalette::NEW_LEVEL_BUTTON);
						}
					} else {
						log::warn!("Invalid level asset handle");
					}
				}
			});
	}
}

fn handle_level_select_screen_action(
	mut commands: Commands,
	mut next_level: ResMut<NextState<PlayingLevel>>,
	query: InteractionQuery<&LevelSelectAction>,
) {
	for (interaction, enabled, action) in &query {
		if enabled.is_none_or(|e| **e) && *interaction != Interaction::Pressed {
			continue;
		}
		match action {
			LevelSelectAction::Back => {
				commands.do_screen_transition(Screen::Title);
			}
			LevelSelectAction::PlayLevel(id) => {
				next_level.set(PlayingLevel(Some(*id)));
				commands.do_screen_transition(Screen::Playing);
			}
		}
	}
}
