//! The Settings screen

use super::*;
use crate::{assets::GlobalFont, ui::prelude::*};

pub(super) fn plugin(app: &mut App) {
	app.add_systems(OnEnter(Screen::Settings), enter_settings)
		.add_systems(
			Update,
			handle_settings_action.run_if(in_state(Screen::Settings).and(ui_not_frozen)),
		);
}

#[derive(Component, Clone, Copy, PartialEq, Eq, Debug, Reflect)]
#[reflect(Component)]
enum SettingsAction {
	Back,
}

fn enter_settings(mut commands: Commands, font: Res<GlobalFont>) {
	commands
		.ui_root()
		.insert(StateScoped(Screen::Settings))
		.with_children(|children| {
			children.header("Settings", font.0.clone_weak());
			children.text("Hello World", JustifyContent::Center, font.0.clone_weak());
			children
				.button("Back", font.0.clone_weak())
				.insert(SettingsAction::Back);
		});
}

fn handle_settings_action(mut commands: Commands, query: InteractionQuery<&SettingsAction>) {
	for (interaction, action) in &query {
		if *interaction == Interaction::Pressed {
			match action {
				SettingsAction::Back => {
					commands.spawn((
						FadeAnimationBundle::default(),
						DoScreenTransition(Screen::Title),
					));
				}
			}
		}
	}
}
