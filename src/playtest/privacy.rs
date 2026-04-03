//! Playtest privacy statement screen

use super::log::PlaytestLog;
use crate::{
	AppSet,
	assets::{GlobalFont, UiButtonAtlas, all_assets_loaded},
	drawing::{ColorKey, TextColorKey},
	screen::{DoScreenTransitionCommands as _, Screen},
	send_message,
	ui::{consts::*, interaction::InteractionQuery, scrollbox::Scrollbox, widgets},
};
use bevy::{input::common_conditions::input_just_pressed, prelude::*};

pub(super) fn plugin(app: &mut App) {
	app.add_message::<ExitInfoScreen>()
		.add_systems(OnEnter(Screen::PlaytestInformation), spawn_statement_screen)
		.add_systems(
			Update,
			(
				proceed_to_entry_screen.run_if(in_state(Screen::Loading).and(all_assets_loaded)),
				(
					(
						record_button_inputs,
						// Enable quitting via escape only after
						// the tester has properly clicked through once
						send_message(ExitInfoScreen).run_if(
							has_seen_privacy_statement.and(input_just_pressed(KeyCode::Escape)),
						),
					)
						.in_set(AppSet::RecordInput),
					exit_privacy_screen
						.run_if(on_message::<ExitInfoScreen>)
						.in_set(AppSet::ExecuteInput),
				)
					.run_if(in_state(Screen::PlaytestInformation)),
			),
		);
}

#[derive(Component, Message, Clone, Copy, Debug)]
struct ExitInfoScreen;

/// Replacement system for leaving the loading screen.
///
/// Switches to this screen, or the title screen
/// if the tester has already seen this one
fn proceed_to_entry_screen(mut next_screen: ResMut<NextState<Screen>>, playtest: Res<PlaytestLog>) {
	let target_screen = if !playtest.has_ackd_privacy_statement {
		Screen::PlaytestInformation
	} else {
		Screen::Title
	};

	next_screen.set(target_screen);
}

fn spawn_statement_screen(
	mut commands: Commands,
	font: Res<GlobalFont>,
	button_sprites: Res<UiButtonAtlas>,
	playtest: Res<PlaytestLog>,
) {
	// Give the user a conspicuous exit button only if they already clicked through
	if playtest.has_ackd_privacy_statement {
		commands.spawn((
			Node {
				margin: TOOLBAR_MARGIN,
				..default()
			},
			DespawnOnExit(Screen::PlaytestInformation),
			children![(
				widgets::sprite_button(&button_sprites, UiButtonAtlas::EXIT),
				ExitInfoScreen,
			)],
		));
	}

	fn paragraph(font: &GlobalFont, text: &str) -> impl Bundle {
		(
			Node {
				width: Val::Percent(80.0),
				justify_content: JustifyContent::Center,
				align_items: AlignItems::Start,
				..default()
			},
			children![(
				Text::new(text),
				TextFont {
					font_size: COMMON_TEXT_SIZE,
					font: font.0.clone(),
					..default()
				},
				TextColorKey(ColorKey::UiLabelText),
			)],
		)
	}

	commands.spawn((
		widgets::ui_root(),
		DespawnOnExit(Screen::PlaytestInformation),
		children![(
			Scrollbox {
				step: 2.*COMMON_TEXT_SIZE,
			},
			Node {
				flex_direction: FlexDirection::Column,
				justify_content: JustifyContent::Start,
				align_items: AlignItems::Center,
				margin: UiRect::vertical(Val::Percent(5.0)),
				width: Val::Percent(100.0),
				height: Val::Percent(100.0),
				row_gap: COMMON_GAP,
				overflow: Overflow::scroll_y(),
				..default()
			},
			children![
				widgets::label("Hello playtester!", font.0.clone()),
				paragraph(&font, "This is a special build of the game for playtesting purposes."),
				paragraph(&font, "\tAs such it is designed to collect playtesting feedback (data entered into feedback forms) and gameplay data (actions (\"moves\") you take within the game, but not raw inputs). We do not collect personally identifying information."),
				paragraph(&font, "\tFor the playtest to work we need you to fill out in-game forms as you play and at the end SUBMIT your feedback via the Playtest Panel."),
				paragraph(&font, "\tSending gameplay data is opt-in. If you change your mind, you can delete or update your data at any time via the Playtest Panel. You can also send us a support request to our email address."),
				paragraph(&font, "\tPLEASE don't forget to submit your feedback, else we won't receive any!!"),
				widgets::label(
					"Thank you for your help and have fun!",
					font.0.clone()
				),
				(
					widgets::grid_button("I understand and will not forget to send my feedback.", font.0.clone()),
					ExitInfoScreen,
				),
			],
		)],
	));
}

fn record_button_inputs(
	query: InteractionQuery<&ExitInfoScreen>,
	mut messages: MessageWriter<ExitInfoScreen>,
) {
	for (interaction, enabled, action) in &query {
		if enabled.is_none_or(|x| **x) && *interaction == Interaction::Pressed {
			messages.write(*action);
		}
	}
}

fn exit_privacy_screen(mut commands: Commands, mut playtest: ResMut<PlaytestLog>) {
	if !playtest.has_ackd_privacy_statement {
		// Mark it as seen, so it does not pop up the next time
		playtest.has_ackd_privacy_statement = true;
		// Proceed to the title
		commands.do_screen_transition(Screen::Title);
	} else {
		// Otherwise the user must have navigated here manually,
		// which can only be done from the playetest panel
		commands.do_screen_transition(Screen::Playtest);
	}
}

fn has_seen_privacy_statement(playtest: Res<PlaytestLog>) -> bool {
	playtest.has_ackd_privacy_statement
}
