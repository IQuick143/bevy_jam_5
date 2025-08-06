//! A credits screen that can be accessed from the title screen.

use bevy::prelude::*;

use super::*;
use crate::{assets::GlobalFont, ui::prelude::*};

pub(super) fn plugin(app: &mut App) {
	app.add_systems(OnEnter(Screen::Credits), enter_credits);
	app.add_systems(OnExit(Screen::Credits), exit_credits);

	app.add_systems(
		Update,
		handle_credits_action.run_if(in_state(Screen::Credits).and(ui_not_frozen)),
	);
	app.register_type::<CreditsAction>();
}

#[derive(Component, Debug, Clone, Copy, PartialEq, Eq, Reflect)]
#[reflect(Component)]
enum CreditsAction {
	Back,
}

fn enter_credits(mut commands: Commands, font: Res<GlobalFont>) {
	let mut table_node = Node {
		display: Display::Grid,
		width: Val::Percent(100.0),
		max_width: Val::Px(800.0),
		column_gap: Val::Px(20.0),
		row_gap: Val::Px(10.0),
		grid_template_columns: vec![RepeatedGridTrack::px(1, 290.0), RepeatedGridTrack::auto(1)],
		..default()
	};
	commands
		.ui_root()
		.insert(StateScoped(Screen::Credits))
		.with_children(|children| {
			children.header("Made by", font.0.clone_weak());
			children.spawn(table_node.clone())
				.with_children(|children| {
					children.text("IQuick 143", JustifyContent::End, font.0.clone_weak());
					children.text("Game design, Programming, Visual direction, Level design", JustifyContent::Start, font.0.clone_weak());
					children.text("IWonderWhatThisAPIDoes", JustifyContent::End, font.0.clone_weak());
					children.text("Programming, Art, Level Design", JustifyContent::Start, font.0.clone_weak());
					children.text("SoysCodingCafe", JustifyContent::End, font.0.clone_weak());
					children.text("Level Design", JustifyContent::Start, font.0.clone_weak());
					children.text("spilledcereals", JustifyContent::End, font.0.clone_weak());
					children.text("Music, SFX", JustifyContent::Start, font.0.clone_weak());
				});

			children.header("Assets", font.0.clone_weak());
			table_node.grid_template_columns[0] = RepeatedGridTrack::px(1, 175.0);
			children.spawn(table_node)
				.with_children(|children| {
					children.text("Bevy logo", JustifyContent::End, font.0.clone_weak());
					children.text("All rights reserved by the Bevy Foundation. Permission granted for splash screen use when unmodified.", JustifyContent::Start, font.0.clone_weak());
					children.text("Comfortaa font", JustifyContent::End, font.0.clone_weak());
					children.text("By Johan Aakerlund, licensed under Open Font License.", JustifyContent::Start, font.0.clone_weak());
				});

			children.button("Back", font.0.clone_weak()).insert(CreditsAction::Back);
		});
}

fn exit_credits(mut _commands: Commands) {
	//commands.trigger(PlaySoundtrack::Disable);
}

fn handle_credits_action(
	mut commands: Commands,
	mut button_query: InteractionQuery<&CreditsAction>,
) {
	for (interaction, action) in &mut button_query {
		if matches!(interaction, Interaction::Pressed) {
			match action {
				CreditsAction::Back => {
					commands.do_screen_transition(Screen::Title);
				}
			}
		}
	}
}
