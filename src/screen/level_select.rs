use bevy::{input::common_conditions::input_just_pressed, prelude::*};

use super::*;
use crate::ui::prelude::*;

pub(super) fn plugin(app: &mut App) {
	app.add_systems(OnEnter(Screen::LevelSelect), spawn_screen)
		.add_systems(
			Update,
			(
				handle_level_select_screen_action,
				return_to_title_screen.run_if(input_just_pressed(KeyCode::Escape)),
			)
				.run_if(in_state(Screen::LevelSelect)),
		);
}

#[derive(Component, Clone, Copy, PartialEq, Eq, Debug)]
enum LevelSelectAction {
	Back,
	PlayLevel(LevelID),
}

fn spawn_screen(mut commands: Commands) {
	commands
		.ui_root()
		.insert(StateScoped(Screen::LevelSelect))
		.with_children(|parent| {
			parent.header("Level Select");
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
					parent
						.small_button("Intro")
						.insert(LevelSelectAction::PlayLevel(LevelID::Intro));
					parent
						.small_button("Boxes")
						.insert(LevelSelectAction::PlayLevel(LevelID::Boxes));
					parent
						.small_button("Manual")
						.insert(LevelSelectAction::PlayLevel(LevelID::Manual));
					parent
						.small_button("Cycle")
						.insert(LevelSelectAction::PlayLevel(LevelID::Cycle));
					parent
						.small_button("Swap")
						.insert(LevelSelectAction::PlayLevel(LevelID::Swap));
					parent
						.small_button("Bicycle")
						.insert(LevelSelectAction::PlayLevel(LevelID::Bicycle));
					parent
						.small_button("Tricycle")
						.insert(LevelSelectAction::PlayLevel(LevelID::Tricycle));
					parent
						.small_button("CargoTricycle")
						.insert(LevelSelectAction::PlayLevel(LevelID::CargoTricycle));
					parent
						.small_button("CargoSingle")
						.insert(LevelSelectAction::PlayLevel(LevelID::CargoSinglePlayer));
					parent
						.small_button("SquareCycle")
						.insert(LevelSelectAction::PlayLevel(LevelID::SquareCycle));
					parent
						.small_button("DiamondCycle")
						.insert(LevelSelectAction::PlayLevel(LevelID::DiamondCycle));
					parent
						.small_button("Lotus")
						.insert(LevelSelectAction::PlayLevel(LevelID::Lotus));
					parent
						.small_button("ThreeInARow")
						.insert(LevelSelectAction::PlayLevel(LevelID::ThreeInARow));
					parent
						.small_button("TripleRing")
						.insert(LevelSelectAction::PlayLevel(LevelID::TripleRing));
					parent
						.small_button("Car")
						.insert(LevelSelectAction::PlayLevel(LevelID::Car));
					parent
						.small_button("Olympic")
						.insert(LevelSelectAction::PlayLevel(LevelID::Olympic));
					parent
						.small_button("Pedalo")
						.insert(LevelSelectAction::PlayLevel(LevelID::Pedalo));
					parent
						.small_button("Disrupt")
						.insert(LevelSelectAction::PlayLevel(LevelID::Disrupt));
					parent
						.small_button("Pyramid")
						.insert(LevelSelectAction::PlayLevel(LevelID::Pyramid));
				});
			parent.button("Back").insert(LevelSelectAction::Back);
		});
}

fn handle_level_select_screen_action(
	mut next_screen: EventWriter<QueueScreenTransition>,
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
				next_screen.send(QueueScreenTransition::fade(Screen::Level(*id)));
			}
		}
	}
}

fn return_to_title_screen(mut next_screen: EventWriter<QueueScreenTransition>) {
	next_screen.send(QueueScreenTransition::fade(Screen::Title));
}
