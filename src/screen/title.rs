//! The title screen that appears when the game starts.

use bevy::prelude::*;

use super::{QueueScreenTransition, Screen};
use crate::{game::assets::GlobalFont, ui::prelude::*};

pub(super) fn plugin(app: &mut App) {
	app.add_systems(OnEnter(Screen::Title), enter_title);

	app.register_type::<TitleAction>();
	app.add_systems(Update, handle_title_action.run_if(in_state(Screen::Title)));
}

#[derive(Component, Debug, Clone, Copy, PartialEq, Eq, Reflect)]
#[reflect(Component)]
enum TitleAction {
	Play,
	Credits,
	/// Exit doesn't work well with embedded applications.
	#[cfg(not(target_family = "wasm"))]
	Exit,
}

fn enter_title(mut commands: Commands, font: Res<GlobalFont>) {
	commands
		.ui_root()
		.insert(StateScoped(Screen::Title))
		.with_children(|children| {
			children.header("Ptolemy's Epicycles", font.0.clone_weak());
			children
				.button("Play", font.0.clone_weak())
				.insert(TitleAction::Play);
			children
				.button("Credits", font.0.clone_weak())
				.insert(TitleAction::Credits);

			#[cfg(not(target_family = "wasm"))]
			children
				.button("Exit", font.0.clone_weak())
				.insert(TitleAction::Exit);
		});
}

fn handle_title_action(
	mut next_screen: EventWriter<QueueScreenTransition<Screen>>,
	mut button_query: InteractionQuery<&TitleAction>,
	#[cfg(not(target_family = "wasm"))] mut app_exit: EventWriter<AppExit>,
) {
	for (interaction, action) in &mut button_query {
		if matches!(interaction, Interaction::Pressed) {
			match action {
				TitleAction::Play => {
					next_screen.send(QueueScreenTransition::fade(Screen::LevelSelect));
				}
				TitleAction::Credits => {
					next_screen.send(QueueScreenTransition::fade(Screen::Credits));
				}
				#[cfg(not(target_family = "wasm"))]
				TitleAction::Exit => {
					app_exit.send(AppExit::Success);
				}
			}
		}
	}
}
