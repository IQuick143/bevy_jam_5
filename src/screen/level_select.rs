use bevy::prelude::*;

use super::*;
use crate::{
	assets::{GlobalFont, LevelList},
	game::level::LevelAsset,
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
	PlayLevel(Handle<LevelAsset>),
}

fn spawn_screen(
	mut commands: Commands,
	levels: Res<LevelList>,
	font: Res<GlobalFont>,
	level_assets: Res<Assets<LevelAsset>>,
) {
	commands
		.ui_root()
		.insert(StateScoped(Screen::LevelSelect))
		.with_children(|parent| {
			parent.header("Level Select", font.0.clone_weak());
			parent
				.spawn(NodeBundle {
					style: Style {
						display: Display::Grid,
						column_gap: Val::Px(10.0),
						row_gap: Val::Px(10.0),
						justify_content: JustifyContent::Center,
						align_content: AlignContent::Center,
						grid_template_columns: vec![RepeatedGridTrack::auto(3)],
						..default()
					},
					..default()
				})
				.with_children(|parent| {
					for level_id in levels.iter() {
						if let Some(level) = level_assets.get(level_id) {
							parent
								.small_button(level.name.clone(), font.0.clone_weak())
								.insert(LevelSelectAction::PlayLevel(level_id.clone_weak()));
						}
					}
				});
			parent
				.button("Back", font.0.clone_weak())
				.insert(LevelSelectAction::Back);
		});
}

fn handle_level_select_screen_action(
	mut next_screen: EventWriter<QueueScreenTransition<Screen>>,
	mut next_level: ResMut<NextState<PlayingLevel>>,
	query: InteractionQuery<&LevelSelectAction>,
) {
	for (interaction, action) in &query {
		if *interaction != Interaction::Pressed {
			continue;
		}
		match action {
			LevelSelectAction::Back => {
				next_screen.send(QueueScreenTransition::fade(Screen::Title));
			}
			LevelSelectAction::PlayLevel(id) => {
				next_level.set(PlayingLevel(Some(id.clone_weak())));
				next_screen.send(QueueScreenTransition::fade(Screen::Playing));
			}
		}
	}
}
