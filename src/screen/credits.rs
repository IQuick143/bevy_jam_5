//! A credits screen that can be accessed from the title screen.

use bevy::prelude::*;

use super::*;
use crate::{game::assets::GlobalFont, ui::prelude::*};

pub(super) fn plugin(app: &mut App) {
	app.add_systems(OnEnter(Screen::Credits), enter_credits);
	app.add_systems(OnExit(Screen::Credits), exit_credits);

	app.add_systems(
		Update,
		handle_credits_action.run_if(in_state(Screen::Credits).and_then(ui_not_frozen)),
	);
	app.register_type::<CreditsAction>();
}

#[derive(Component, Debug, Clone, Copy, PartialEq, Eq, Reflect)]
#[reflect(Component)]
enum CreditsAction {
	Back,
}

fn enter_credits(mut commands: Commands, font: Res<GlobalFont>) {
	commands
		.ui_root()
		.insert(StateScoped(Screen::Credits))
		.with_children(|children| {
			children.header("Made by", font.0.clone_weak());
			children.label("IQuick 143 - Game design, Programming, Visual direction, Level design", font.0.clone_weak());
			children.label("IWonderWhatThisAPIDoes - Programming, Art, Level Design", font.0.clone_weak());
			children.label("SoysCodingCafe - Level Design", font.0.clone_weak());

			children.header("Assets", font.0.clone_weak());
			children.label("Bevy logo - All rights reserved by the Bevy Foundation. Permission granted for splash screen use when unmodified.", font.0.clone_weak());
			children.label("Comfortaa font - By Johan Aakerlund, licensed under Open Font License", font.0.clone_weak());

			children.button("Back", font.0.clone_weak()).insert(CreditsAction::Back);
		});
}

fn exit_credits(mut _commands: Commands) {
	//commands.trigger(PlaySoundtrack::Disable);
}

fn handle_credits_action(
	mut next_screen: EventWriter<QueueScreenTransition<Screen>>,
	mut button_query: InteractionQuery<&CreditsAction>,
) {
	for (interaction, action) in &mut button_query {
		if matches!(interaction, Interaction::Pressed) {
			match action {
				CreditsAction::Back => {
					next_screen.send(QueueScreenTransition::fade(Screen::Title));
				}
			}
		}
	}
}
