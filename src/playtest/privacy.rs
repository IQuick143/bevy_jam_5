//! Playtest privacy statement screen

use super::log::PlaytestLog;
use crate::{
	assets::{all_assets_loaded, GlobalFont, UiButtonAtlas},
	screen::{DoScreenTransitionCommands as _, Screen},
	send_message,
	ui::{consts::*, interaction::InteractionQuery, scrollbox::Scrollbox, widgets},
	AppSet,
};
use bevy::{input::common_conditions::input_just_pressed, prelude::*};

pub(super) fn plugin(app: &mut App) {
	app.add_message::<ExitPrivacyScreen>()
		.add_systems(
			OnEnter(Screen::PlaytestPrivacyStatement),
			spawn_statement_screen,
		)
		.add_systems(
			Update,
			(
				proceed_to_entry_screen.run_if(in_state(Screen::Loading).and(all_assets_loaded)),
				(
					(
						record_button_inputs,
						// Enable quitting via escape only after
						// the tester has properly clicked through once
						send_message(ExitPrivacyScreen).run_if(
							has_seen_privacy_statement.and(input_just_pressed(KeyCode::Escape)),
						),
					)
						.in_set(AppSet::RecordInput),
					exit_privacy_screen
						.run_if(on_message::<ExitPrivacyScreen>)
						.in_set(AppSet::ExecuteInput),
				)
					.run_if(in_state(Screen::PlaytestPrivacyStatement)),
			),
		);
}

#[derive(Component, Message, Clone, Copy, Debug)]
struct ExitPrivacyScreen;

/// Replacement system for leaving the loading screen.
///
/// Switches to this screen, or the title screen
/// if the tester has already seen this one
fn proceed_to_entry_screen(mut next_screen: ResMut<NextState<Screen>>, playtest: Res<PlaytestLog>) {
	let target_screen = if !playtest.has_seen_privacy_statement {
		Screen::PlaytestPrivacyStatement
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
	if playtest.has_seen_privacy_statement {
		commands.spawn((
			Node {
				margin: TOOLBAR_MARGIN,
				..default()
			},
			DespawnOnExit(Screen::PlaytestPrivacyStatement),
			children![(
				widgets::sprite_button(&button_sprites, UiButtonAtlas::EXIT),
				ExitPrivacyScreen,
			)],
		));
	}

	commands.spawn((
		widgets::ui_root(),
		DespawnOnExit(Screen::PlaytestPrivacyStatement),
		children![(
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
			children![
				widgets::label("Hello playtester!", font.0.clone()),
				widgets::label(
					"We have to dump some legal bs on you. Please do not resist.",
					font.0.clone()
				),
				(
					widgets::grid_button("Understood!", font.0.clone()),
					ExitPrivacyScreen,
				),
			],
		)],
	));
}

fn record_button_inputs(
	query: InteractionQuery<&ExitPrivacyScreen>,
	mut messages: MessageWriter<ExitPrivacyScreen>,
) {
	for (interaction, enabled, action) in &query {
		if enabled.is_none_or(|x| **x) && *interaction == Interaction::Pressed {
			messages.write(*action);
		}
	}
}

fn exit_privacy_screen(mut commands: Commands, mut playtest: ResMut<PlaytestLog>) {
	// Mark it as seen, so it does not pop up the next time
	if !playtest.has_seen_privacy_statement {
		playtest.has_seen_privacy_statement = true;
	}

	commands.do_screen_transition(Screen::Title);
}

fn has_seen_privacy_statement(playtest: Res<PlaytestLog>) -> bool {
	playtest.has_seen_privacy_statement
}
