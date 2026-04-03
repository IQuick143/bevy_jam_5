//! Global feedback form screen

use super::{
	consts::*,
	log::{LogSerializationScope, PlaytestLog},
	submit::*,
};
use crate::{
	AppSet,
	assets::{GlobalFont, HandleMap, ImageKey, UiButtonAtlas},
	drawing::{ColorKey, NodeColorKey, TextColorKey},
	game::spawn::EnterLevelStage,
	screen::{DoScreenTransition, DoScreenTransitionCommands as _, LoadLevel, Screen},
	ui::{consts::*, prelude::*, scrollbox::Scrollbox, widgets},
};
use bevy::{input::common_conditions::input_just_pressed, prelude::*};
use bevy_ui_text_input::TextInputContents;

pub(super) fn plugin(app: &mut App) {
	app.add_message::<FeedbackFormAction>()
		.init_resource::<MainFormReturnScreen>()
		.add_systems(OnEnter(Screen::Playtest), spawn_feedback_form_screen)
		.add_systems(
			Update,
			(
				(
					(
						exit_feedback_screen.run_if(input_just_pressed(KeyCode::Escape)),
						read_feedback_screen_input,
					)
						.run_if(ui_not_frozen),
					synchronize_text_feedback,
				)
					.in_set(AppSet::RecordInput),
				handle_feedback_screen_input.in_set(AppSet::ExecuteInput),
				display_submission_status.in_set(SubmissionSystems),
			)
				.run_if(in_state(Screen::Playtest)),
		);
}

/// Denotes the screen to which the submission form should return on exit
#[derive(Resource, Clone, Copy, Debug, Default)]
pub enum MainFormReturnScreen {
	#[default]
	Title,
	Playing,
}

#[derive(Component, Message, Clone, Copy, PartialEq, Eq, Debug)]
enum FeedbackFormAction {
	Back,
	Submit(LogSerializationScope),
}

/// Marker for a text input linked to a text question identified by a key
#[derive(Component, Clone, Copy, Debug)]
struct FeedbackTextQuestion(&'static str);

/// Marker component for the node that displays the status of a submission
#[derive(Component, Clone, Copy, Debug, Default)]
struct SubmitResultNode;

const FEEDBACK_QUESTIONS: [(&str, &str, bool); 7] = [
	(
		"technical",
		"Did you encounterd any bugs/technical issues?",
		false,
	),
	(
		"favorites",
		"What was your favorite moment in the game?",
		false,
	),
	(
		"frustrating",
		"What part of the game frustrated you the most? (Please be harsh, it helps.)",
		false,
	),
	(
		"confusing",
		"Which parts of the game were confusing?",
		false,
	),
	(
		"art",
		"Did you enjoy the artstyle, was everything legible? Why/why not?",
		false,
	),
	("comment", "Any other comments?", false),
	("price", "How much do you think the game should cost?", true),
];

fn spawn_feedback_form_screen(
	mut commands: Commands,
	button_sprites: Res<UiButtonAtlas>,
	font: Res<GlobalFont>,
	playtest: Res<PlaytestLog>,
) {
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

	let id = commands
		.spawn((widgets::ui_root(), DespawnOnExit(Screen::Playtest)))
		.id();
	let main = commands
		.spawn((
			Scrollbox {
				step: COMMON_TEXT_SIZE,
			},
			Node {
				flex_direction: FlexDirection::Column,
				justify_content: JustifyContent::Start,
				align_items: AlignItems::Center,
				padding: UiRect::vertical(Val::Percent(5.0)),
				row_gap: COMMON_GAP,
				overflow: Overflow::scroll_y(),
				..default()
			},
			ChildOf(id),
		))
		.id();

	commands.spawn((
		widgets::header("Feedback and submission", font.0.clone()),
		ChildOf(main),
	));

	commands.spawn((
		widgets::text_with_size(
			format!("Tester ID: {}", playtest.tester_id()),
			JustifyContent::Center,
			SMALL_TEXT_SIZE,
			font.0.clone(),
		),
		ChildOf(main),
	));

	for (key, question, is_single_line) in FEEDBACK_QUESTIONS {
		let current_answer = playtest
			.global_feedback
			.get(key)
			.map(String::as_str)
			.unwrap_or_default();
		commands.spawn((
			widgets::text(question, JustifyContent::Center, font.0.clone()),
			ChildOf(main),
		));
		commands.spawn((
			Node {
				max_width: Val::Percent(100.0),
				padding: UiRect::horizontal(COMMON_GAP),
				width: GLOBAL_FEEDBACK_FORM_MAX_WIDTH,
				..default()
			},
			ChildOf(main),
			children![super::widgets::text_input(
				current_answer,
				font.0.clone(),
				is_single_line,
				FeedbackTextQuestion(key),
			)],
		));
	}

	commands.spawn((
		Node {
			flex_direction: FlexDirection::Column,
			max_width: GLOBAL_FEEDBACK_FORM_MAX_WIDTH,
			row_gap: COMMON_GAP,
			align_items: AlignItems::Center,
			..default()
		},
		ChildOf(main),
		children![
			(
				Node {
					padding: UiRect::vertical(WIDE_GAP),
					column_gap: COMMON_GAP,
					..default()
				},
				children![
					(
						ImageNode::default(),
						Node {
							width: Val::Px(COMMON_TEXT_SIZE),
							height: Val::Px(COMMON_TEXT_SIZE),
							..default()
						},
						NodeColorKey(default()),
						SubmitResultNode,
					),
					(
						Text::default(),
						TextFont {
							font_size: COMMON_TEXT_SIZE,
							font: font.0.clone(),
							..default()
						},
						TextColorKey(ColorKey::UiLabelText),
						SubmitResultNode,
					),
				],
			),
			(
				widgets::grid_button("Submit all playtest feedback", font.0.clone()),
				FeedbackFormAction::Submit(LogSerializationScope::Full),
			),
			(
				Node {
					padding: UiRect::bottom(WIDE_GAP),
					..default()
				},
				Text::new("Send us all your feedback including a log of the moves you took in-game. (Please do! it helps us understand how you approached the puzzles and what you struggled with).\nYou can always submit again to update your response."),
				TextLayout::new_with_justify(Justify::Center),
				TextFont {
					font_size: SMALL_TEXT_SIZE,
					font: font.0.clone(),
					..default()
				},
				TextColorKey(ColorKey::UiLabelText),
			),
			(
				widgets::grid_button("Submit feedback without move logs", font.0.clone()),
				FeedbackFormAction::Submit(LogSerializationScope::FeedbackOnly),
			),
			(
				Node {
					padding: UiRect::bottom(WIDE_GAP),
					..default()
				},
				Text::new("Send us your feedback but remove gameplay data."),
				TextLayout::new_with_justify(Justify::Center),
				TextFont {
					font_size: SMALL_TEXT_SIZE,
					font: font.0.clone(),
					..default()
				},
				TextColorKey(ColorKey::UiLabelText),
			),
			(
				widgets::grid_button("Delete submission", font.0.clone()),
				FeedbackFormAction::Submit(LogSerializationScope::Clear),
			),
			(
				Node {
					padding: UiRect::bottom(WIDE_GAP),
					..default()
				},
				Text::new("Delete your playtest data from our server. All data stays on your machine, so you can resubmit if you change your mind."),
				TextLayout::new_with_justify(Justify::Center),
				TextFont {
					font_size: SMALL_TEXT_SIZE,
					font: font.0.clone(),
					..default()
				},
				TextColorKey(ColorKey::UiLabelText),
			),
		],
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
	return_screen: Res<MainFormReturnScreen>,
) {
	for message in messages.read() {
		match message {
			FeedbackFormAction::Back => {
				exit_feedback_screen(commands, return_screen);
				break;
			}
			FeedbackFormAction::Submit(scope) => {
				commands.spawn((SubmissionTask::new(*scope), FreezeUi));
			}
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

fn display_submission_status(
	mut text_node: Single<&mut Text, With<SubmitResultNode>>,
	image_node: Single<(&mut ImageNode, &mut NodeColorKey), With<SubmitResultNode>>,
	query: Query<&SubmissionTask, Changed<SubmissionTask>>,
	images: Res<HandleMap<ImageKey>>,
) {
	let (mut image_node, mut image_color) = image_node.into_inner();
	if let Some(task) = query.iter().last() {
		let status_message = match task.get_result() {
			None => "Submitting...",
			Some(Ok(())) => match task.scope() {
				LogSerializationScope::Clear => "Response deleted!",
				_ => "Response accepted!",
			},
			Some(Err(ureq::Error::Timeout(_))) => "Request timed out",
			Some(Err(ureq::Error::StatusCode(413))) => {
				"Sorry, your response is too large. Consider submitting without game log or email the raw file to us"
			}
			Some(Err(ureq::Error::StatusCode(429))) => {
				"You have submitted too many times recently, try again later"
			}
			Some(Err(ureq::Error::StatusCode(500..))) => {
				"Internal server error, try again later or contact us."
			}
			Some(Err(_)) => {
				"Could not process response from server, try again later or contact us."
			}
		};
		let (icon_key, icon_color) = match task.get_result() {
			None => (None, ColorKey::Blank),
			Some(Ok(())) => (Some(ImageKey::Checkmark), ColorKey::Checkmark),
			Some(Err(_)) => (Some(ImageKey::ErrorX), ColorKey::WarningSign),
		};
		***text_node = status_message.to_owned();
		image_node.image = icon_key.map(|k| images[&k].clone()).unwrap_or_default();
		**image_color = icon_color;
	}
}

fn exit_feedback_screen(mut commands: Commands, return_screen: Res<MainFormReturnScreen>) {
	match *return_screen {
		MainFormReturnScreen::Title => commands.do_screen_transition(Screen::Title),
		MainFormReturnScreen::Playing => {
			commands.spawn((
				FadeAnimationBundle::default(),
				DoScreenTransition(Screen::Playing),
				LoadLevel(EnterLevelStage::Resume),
			));
		}
	}
}
