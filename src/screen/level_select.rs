use bevy::prelude::*;

use super::*;
use crate::{
	assets::{GlobalFont, LoadedLevelList},
	game::level::LevelData,
	ui::prelude::*,
};

pub(super) fn plugin(app: &mut App) {
	app.add_systems(OnEnter(Screen::LevelSelect), spawn_screen)
		.add_systems(
			Update,
			handle_level_select_screen_action
				.run_if(in_state(Screen::LevelSelect).and_then(ui_not_frozen)),
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
) {
	commands
		.ui_root()
		.insert(StateScoped(Screen::LevelSelect))
		.with_children(|parent| {
			parent.header("Level Select", font.0.clone_weak());
			for section in &levels.list.sections {
				let mut section_levels = levels.levels[section.level_indices.clone()]
					.iter()
					.enumerate()
					// Offset indices to account for section start
					.map(|(i, x)| (i + section.level_indices.start, x))
					.peekable();
				if section_levels.peek().is_none() {
					// Exclude any sections that have no displayable levels
					continue;
				}
				parent.label(
					section
						.name
						.clone()
						.unwrap_or_else(|| "NAME_MISSING".to_owned()),
					font.0.clone_weak(),
				);
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
						for (level_id, level_handle) in section_levels {
							if let Some(level) = level_assets.get(level_handle) {
								parent
									.small_button(level.name.clone(), font.0.clone_weak())
									.insert(LevelSelectAction::PlayLevel(level_id));
							}
						}
					});
			}
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
				commands.spawn((
					FadeAnimationBundle::default(),
					DoScreenTransition(Screen::Title),
				));
			}
			LevelSelectAction::PlayLevel(id) => {
				next_level.set(PlayingLevel(Some(*id)));
				commands.spawn((
					FadeAnimationBundle::default(),
					DoScreenTransition(Screen::Playing),
				));
			}
		}
	}
}
