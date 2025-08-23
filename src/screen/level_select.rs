use bevy::prelude::*;

use super::*;
use crate::{
	assets::{GlobalFont, HandleMap, ImageKey, LoadedLevelList},
	game::{
		drawing::ThingPalette,
		level::{list::LevelList, LevelData},
	},
	save::SaveGame,
	ui::prelude::*,
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
	commands
		.ui_root()
		.insert(StateScoped(Screen::LevelSelect))
		.with_children(|parent| {
			parent.header("Level Select", font.0.clone_weak());
			parent
				.spawn(Node {
					display: Display::Grid,
					column_gap: Val::Px(10.0),
					row_gap: Val::Px(10.0),
					justify_content: JustifyContent::Center,
					align_content: AlignContent::Center,
					grid_template_columns: vec![RepeatedGridTrack::auto(3)],
					..default()
				})
				.with_children(|parent| {
					for (level_id, level_meta) in levels.levels.iter().enumerate() {
						if let Some(level) = level_assets.get(&level_meta.data_handle) {
							let mut button =
								parent.small_button(level.name.clone(), font.0.clone_weak());
							button.insert(LevelSelectAction::PlayLevel(level_id));
							if save.is_level_completed(&level_meta.identifier) {
								button.with_child((
									Name::new("Level Completed Marker"),
									Node {
										width: Val::Px(30.0),
										height: Val::Px(30.0),
										position_type: PositionType::Absolute,
										bottom: Val::Px(7.5),
										right: Val::Px(7.5),
										..default()
									},
									ImageNode {
										image: images[&ImageKey::Checkmark].clone_weak(),
										color: colors.checkmark,
										image_mode: NodeImageMode::Stretch,
										..default()
									},
								));
							}
						} else {
							log::warn!("Invalid level asset handle");
						}
					}
				});
			parent
				.button("Back", font.0.clone_weak())
				.insert(LevelSelectAction::Back);
		});
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
