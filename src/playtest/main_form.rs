//! Global feedback form screen

use super::{consts::*, log::PlaytestLog};
use crate::{
	assets::{GlobalFont, UiButtonAtlas},
	screen::{DoScreenTransitionCommands as _, Screen},
	ui::{consts::*, prelude::*, scrollbox::Scrollbox, widgets},
	AppSet,
};
use bevy::prelude::*;
use bevy_ui_text_input::TextInputContents;

pub(super) fn plugin(app: &mut App) {
	app.add_message::<FeedbackFormAction>()
		.add_systems(OnEnter(Screen::Playtest), spawn_feedback_form_screen)
		.add_systems(
			Update,
			(
				(
					read_feedback_screen_input.run_if(ui_not_frozen),
					synchronize_text_feedback,
				)
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

/// Marker for a text input linked to a text question identified by a key
#[derive(Component, Clone, Copy, Debug)]
struct FeedbackTextQuestion(&'static str);

const FEEDBACK_QUESTIONS: [(&str, &str); 3] = [
	(
		"favorites",
		"Which parts of the game did you enjoy the most?",
	),
	("confusing", "Which parts of the game were confusing?"),
	("price", "How much do you think the game should cost?"),
];

fn spawn_feedback_form_screen(
	mut commands: Commands,
	button_sprites: Res<UiButtonAtlas>,
	font: Res<GlobalFont>,
	playtest: Res<PlaytestLog>,
) {
	let id = commands
		.spawn((
			widgets::ui_root(),
			DespawnOnExit(Screen::Playtest),
			Scrollbox {
				step: COMMON_TEXT_SIZE,
			},
		))
		.id();
	commands.spawn((
		widgets::header("Playtest feedback form", font.0.clone()),
		ChildOf(id),
	));

	for (key, question) in FEEDBACK_QUESTIONS {
		let current_answer = playtest
			.global_feedback
			.get(key)
			.map(String::as_str)
			.unwrap_or_default();
		commands.spawn((
			widgets::text(question, JustifyContent::Center, font.0.clone()),
			ChildOf(id),
		));
		commands.spawn((
			Node {
				max_width: Val::Percent(100.0),
				padding: UiRect::horizontal(COMMON_GAP),
				width: GLOBAL_FEEDBACK_FORM_MAX_WIDTH,
				..default()
			},
			ChildOf(id),
			children![super::widgets::text_input(
				current_answer,
				font.0.clone(),
				FeedbackTextQuestion(key),
			)],
		));
	}

	commands.spawn((
		widgets::menu_button("Submit", font.0.clone()),
		FeedbackFormAction::Submit,
		ChildOf(id),
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

fn synchronize_text_feedback(
	query: Query<(&TextInputContents, &FeedbackTextQuestion), Changed<TextInputContents>>,
	mut playtest: ResMut<PlaytestLog>,
) {
	for (value, FeedbackTextQuestion(key)) in &query {
		playtest
			.global_feedback
			.insert(key.to_string(), value.get().to_string());
	}
}
