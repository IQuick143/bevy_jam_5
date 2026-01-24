//! Global feedback form screen

use crate::{
	assets::{GlobalFont, UiButtonAtlas},
	screen::{DoScreenTransitionCommands as _, Screen},
	ui::{consts::*, prelude::*, widgets},
	AppSet,
};
use bevy::prelude::*;

pub(super) fn plugin(app: &mut App) {
	app.add_message::<FeedbackFormAction>()
		.add_systems(OnEnter(Screen::Playtest), spawn_feedback_form_screen)
		.add_systems(
			Update,
			(
				read_feedback_screen_input
					.run_if(ui_not_frozen)
					.in_set(AppSet::RecordInput),
				handle_feedback_screen_input.in_set(AppSet::ExecuteInput),
			),
		);
}

#[derive(Component, Message, Clone, Copy, PartialEq, Eq, Debug)]
enum FeedbackFormAction {
	Back,
	Submit,
}

fn spawn_feedback_form_screen(
	mut commands: Commands,
	button_sprites: Res<UiButtonAtlas>,
	font: Res<GlobalFont>,
) {
	commands.spawn((
		widgets::ui_root(),
		DespawnOnExit(Screen::Playtest),
		children![
			widgets::header("Playtest feedback form", font.0.clone()),
			(
				widgets::menu_button("Submit", font.0.clone()),
				FeedbackFormAction::Submit,
			),
		],
	));
	commands.spawn((
		Node {
			margin: TOOLBAR_MARGIN,
			..default()
		},
		DespawnOnExit(Screen::Playtest),
		children![(
			widgets::sprite_button(&button_sprites, UiButtonAtlas::EXIT),
			FeedbackFormAction::Back,
		)],
	));
}

fn read_feedback_screen_input(
	query: InteractionQuery<&FeedbackFormAction>,
	mut messages: MessageWriter<FeedbackFormAction>,
) {
	for (interaction, enabled, action) in &query {
		if *interaction == Interaction::Pressed && enabled.is_none_or(|e| **e) {
			messages.write(*action);
		}
	}
}

fn handle_feedback_screen_input(
	mut messages: MessageReader<FeedbackFormAction>,
	mut commands: Commands,
) {
	for message in messages.read() {
		match message {
			FeedbackFormAction::Back => commands.do_screen_transition(Screen::Title),
			FeedbackFormAction::Submit => todo!(),
		}
	}
}
