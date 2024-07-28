use bevy::{input::common_conditions::input_just_pressed, prelude::*};

use super::*;
use crate::{
	game::{assets::GlobalFont, LevelID},
	ui::prelude::*,
};

pub(super) fn plugin(app: &mut App) {
	app.add_systems(OnEnter(Screen::LevelSelect), spawn_screen)
		.add_systems(
			Update,
			(
				handle_level_select_screen_action,
				return_to_title_screen.run_if(input_just_pressed(KeyCode::Escape)),
			)
				.run_if(in_state(Screen::LevelSelect).and_then(ui_not_frozen)),
		);
}

#[derive(Component, Clone, Copy, PartialEq, Eq, Debug)]
enum LevelSelectAction {
	Back,
	PlayLevel(LevelID),
}

fn spawn_screen(mut commands: Commands, font: Res<GlobalFont>) {
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
					for level_id in &LevelID::LEVEL_ORDER {
						parent
							.small_button(level_id.level_name(), font.0.clone_weak())
							.insert(LevelSelectAction::PlayLevel(*level_id));
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
				next_level.set(PlayingLevel(Some(*id)));
				next_screen.send(QueueScreenTransition::fade(Screen::Playing));
			}
		}
	}
}

fn return_to_title_screen(mut next_screen: EventWriter<QueueScreenTransition<Screen>>) {
	next_screen.send(QueueScreenTransition::fade(Screen::Title));
}
