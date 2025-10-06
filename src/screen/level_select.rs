use bevy::prelude::*;

use super::*;
use crate::{
	assets::{GlobalFont, HandleMap, ImageKey, LoadedLevelList},
	game::{
		drawing::ThingPalette,
		level::{list::LevelList, LevelData},
	},
	save::SaveGame,
	ui::{consts::*, prelude::*},
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
	colors: Res<ThingPalette>,
) {
	let levels = level_list_asset
		.get(&levels.0)
		.expect("The LevelList asset should be valid");
	let root_id = commands
		.spawn((widgets::ui_root(), DespawnOnExit(Screen::LevelSelect)))
		.id();
	commands.spawn((
		widgets::header("Level Select", font.0.clone()),
		ChildOf(root_id),
	));
	let main_id = commands
		.spawn((
			Node {
				flex_direction: FlexDirection::Column,
				justify_content: JustifyContent::Start,
				align_content: AlignContent::Center,
				height: Val::Percent(60.0),
				row_gap: Val::Px(10.0),
				overflow: Overflow::scroll_y(),
				..default()
			},
			ChildOf(root_id),
		))
		.id();
	commands.spawn((
		widgets::menu_button("Back", font.0.clone()),
		LevelSelectAction::Back,
		ChildOf(root_id),
	));

	for hub in &levels.hubs {
		commands
			.spawn((
				Node {
					display: Display::Grid,
					column_gap: COMMON_GAP,
					row_gap: COMMON_GAP,
					justify_content: JustifyContent::Center,
					align_content: AlignContent::Center,
					grid_template_columns: vec![RepeatedGridTrack::auto(3)],
					..default()
				},
				ChildOf(main_id),
			))
			.with_children(|parent| {
				for &level_id in &hub.levels {
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
									color: colors.checkmark,
									image_mode: NodeImageMode::Stretch,
									..default()
								},
							));
						} else {
							log::warn!("Invalid level asset handle");
						}
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
	for (interaction, action) in &query {
		if *interaction != Interaction::Pressed {
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
