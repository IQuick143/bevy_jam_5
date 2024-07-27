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
					let mut button = |text: &str, level: LevelID| {
						parent
							.small_button(text)
							.insert(LevelSelectAction::PlayLevel(level));
					};
					use LevelID::*;
					button("Intro", Intro);
					button("Boxes", Boxes);
					button("Cycle", Cycle);
					button("Transfer", Transfer);
					button("Manual", Manual);
					button("Swap", Swap);
					button("Bicycle", Bicycle);
					button("Tricycle", Tricycle);
					button("CargoTricycle", CargoTricycle);
					button("CargoSingle", CargoSinglePlayer);
					button("SquareCycle", SquareCycle);
					button("DiamondCycle", DiamondCycle);
					button("Lotus", Lotus);
					button("ThreeInARow", ThreeInARow);
					button("TripleRing", TripleRing);
					button("Car", Car);
					button("Olympic", Olympic);
					button("Pedalo", Pedalo);
					button("Disrupt", Disrupt);
					button("Pyramid", Pyramid);
					button("Teamwork", Teamwork);
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
