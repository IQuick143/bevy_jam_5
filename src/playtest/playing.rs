//! Extension to the playing screen in playtesting builds

use super::{
	log::PlaytestLog,
	stars::{StarRating, StarRatingValue},
};
use crate::{
	assets::{GlobalFont, HandleMap, ImageKey, UiButtonAtlas},
	drawing::ColorKey,
	graphics,
	screen::{
		DoScreenTransition, DoScreenTransitionCommands, GotoNextLevel, PlayingLevelListEntry,
		Screen,
	},
	ui::{
		consts::*,
		hover::{self, *},
		interaction::{InteractionEnabled, Unfreeze},
		prelude::*,
	},
	AppSet,
};
use bevy::{color::palettes::tailwind, prelude::*};

pub(super) fn plugin(app: &mut App) {
	app.add_message::<OpenFeedbackForm>()
		.add_message::<ExitFeedbackForm>()
		.add_systems(OnEnter(Screen::Playing), spawn_playtest_playing_ui)
		.add_systems(
			Update,
			(
				record_playing_screen_input.in_set(AppSet::RecordInput),
				(
					spawn_feedback_form.run_if(on_message::<OpenFeedbackForm>),
					(close_feedback_form, do_action_after_close_form)
						.run_if(on_message::<ExitFeedbackForm>),
				)
					.in_set(AppSet::ExecuteInput),
				intercept_exit_from_level
					.after(AppSet::ExecuteInput)
					.run_if(in_state(Screen::Playing)),
				synchronize_level_feedback,
				update_submit_enable_disable,
			),
		);
}

/// Marker component for the root of the feedback form
#[derive(Component, Clone, Copy, Debug, Default)]
struct FeedbackForm;

#[derive(Component, Message, Clone, Copy, Debug, Default)]
struct OpenFeedbackForm(AfterExitFeedbackForm);

#[derive(Component, Message, Clone, Copy, Debug, Default)]
struct ExitFeedbackForm(AfterExitFeedbackForm);

/// Action to take after the tester completes the feedback form
#[derive(Component, Clone, Copy, Debug, Default)]
enum AfterExitFeedbackForm {
	#[default]
	Stay,
	ChangeScreen(Screen),
	NextLevel,
}

impl InteractionPalette {
	const PLAYTEST_BUTTON: Self = Self {
		none: ColorKey::PlaytestMarker,
		..Self::SPRITE_BUTTON
	};

	const CONFIRM_BUTTON: Self = Self::NEXT_LEVEL_BUTTON;
}

const PLAYTEST_BUTTON_HOVER_HINT: &str = "Playtest feedback form";

fn spawn_playtest_playing_ui(mut commands: Commands, image_handles: Res<HandleMap<ImageKey>>) {
	commands.spawn((
		widgets::ui_root_justified(JustifyContent::Start),
		DespawnOnExit(Screen::Playing),
		children![(
			Node {
				width: Val::Percent(100.0),
				justify_content: JustifyContent::End,
				padding: UiRect::new(
					Val::Px(0.0),
					Val::Px(
						COMMON_GAP_PX
							+ TOOLBAR_MARGIN_X_PX + SPRITE_BUTTON_HEIGHT
							* UiButtonAtlas::TILE_SIZE.x as f32
							/ UiButtonAtlas::TILE_SIZE.y as f32
					),
					COMMON_GAP,
					Val::Px(0.0),
				),
				..default()
			},
			children![(
				widgets::common_sprite_button(
					image_handles[&ImageKey::PlaytestMarker].clone(),
					InteractionPalette::PLAYTEST_BUTTON,
					1.0,
					None,
				),
				OpenFeedbackForm(AfterExitFeedbackForm::Stay),
				HoverHint(PLAYTEST_BUTTON_HOVER_HINT),
				HoverPriority(hover::prio::STATIC_UI),
				UseHoverFromInteraction,
			)],
		)],
	));
}

const FEEDBACK_FORM_WIDTH: Val = Val::Px(600.0);
const FEEDBACK_FORM_BG_COLOR: Color = Color::WHITE;
const FEEDBACK_FORM_FRAME_COLOR: Color = Color::Srgba(tailwind::SLATE_500);
const FEEDBACK_FORM_FRAME_WIDTH: Val = Val::Px(graphics::RING_OUTLINE_WIDTH);
const FEEDBACK_FORM_PADDING: UiRect = UiRect::axes(COMMON_GAP, Val::Px(5.0));
const FEEDBACK_FORM_BODY_PADDING: UiRect = UiRect::axes(Val::Px(40.0), Val::Px(15.0));

fn spawn_feedback_form(
	mut messages: MessageReader<OpenFeedbackForm>,
	mut commands: Commands,
	playing_level: PlayingLevelListEntry,
	playtest_log: Res<PlaytestLog>,
	ui_button_atlas: Res<UiButtonAtlas>,
	image_handles: Res<HandleMap<ImageKey>>,
	font: Res<GlobalFont>,
	existing_form: Query<(), With<FeedbackForm>>,
) -> Result {
	if !existing_form.is_empty() {
		return Err("Attempting to open feedback form when one is already open".into());
	}

	let Some(&OpenFeedbackForm(after_close)) = messages.read().last() else {
		return Ok(());
	};

	let level_key = &playing_level.get()?.identifier;
	let current_rating = playtest_log
		.get_level(level_key)
		.map(|l| l.stars)
		.unwrap_or_default() as u32;

	commands.spawn((
		widgets::ui_root(),
		FeedbackForm,
		FreezeUi, // Modal dialog; block all other UI while active
		children![(
			Node {
				max_width: Val::Percent(100.0),
				width: FEEDBACK_FORM_WIDTH,
				border: UiRect::all(FEEDBACK_FORM_FRAME_WIDTH),
				padding: FEEDBACK_FORM_PADDING,
				flex_direction: FlexDirection::Column,
				..default()
			},
			BackgroundColor(FEEDBACK_FORM_BG_COLOR),
			BorderColor::all(FEEDBACK_FORM_FRAME_COLOR),
			children![
				(
					Node::default(),
					children![
						(
							widgets::sprite_button(&ui_button_atlas, UiButtonAtlas::EXIT),
							ExitFeedbackForm(AfterExitFeedbackForm::Stay),
							Unfreeze,
						),
						Node {
							flex_grow: 1.0,
							..default()
						},
						(
							widgets::common_sprite_button(
								image_handles[&ImageKey::Checkmark].clone(),
								InteractionPalette::CONFIRM_BUTTON,
								1.0,
								None,
							),
							ExitFeedbackForm(after_close),
							Unfreeze,
							// Allow the player to submit if a rating has been given
							InteractionEnabled(current_rating > 0),
						),
					],
				),
				(
					Node {
						padding: FEEDBACK_FORM_BODY_PADDING,
						flex_direction: FlexDirection::Column,
						align_items: AlignItems::Center,
						..default()
					},
					children![
						(widgets::label("How did you like this level?", font.0.clone())),
						(StarRating::new(5), StarRatingValue(current_rating)),
					],
				),
			],
		)],
	));

	Ok(())
}

fn close_feedback_form(mut commands: Commands, query: Query<Entity, With<FeedbackForm>>) {
	for id in &query {
		commands.entity(id).despawn();
	}
}

/// This system resubmits the transition thas was intercepted
/// by [`intercept_exit_from_level`] once the feedback form
/// is closed
fn do_action_after_close_form(
	mut messages: MessageReader<ExitFeedbackForm>,
	mut commands: Commands,
) {
	let Some(ExitFeedbackForm(action)) = messages.read().last() else {
		return;
	};
	match action {
		AfterExitFeedbackForm::Stay => {}
		AfterExitFeedbackForm::ChangeScreen(screen) => {
			commands.do_screen_transition(*screen);
		}
		AfterExitFeedbackForm::NextLevel => {
			commands.spawn((FadeAnimationBundle::default(), GotoNextLevel));
		}
	}
}

fn record_playing_screen_input(
	query: InteractionQuery<AnyOf<(&OpenFeedbackForm, &ExitFeedbackForm)>>,
	mut open_events: MessageWriter<OpenFeedbackForm>,
	mut close_events: MessageWriter<ExitFeedbackForm>,
	existing_form: Query<(), With<FeedbackForm>>,
) {
	for (interaction, enabled, (open, close)) in &query {
		if enabled.is_none_or(|e| **e) && *interaction == Interaction::Pressed {
			if let Some(e) = open {
				if existing_form.is_empty() {
					open_events.write(*e);
				} else {
					// If a form is already open, close it when the tester
					// clicks the button instead
					close_events.write(ExitFeedbackForm(AfterExitFeedbackForm::Stay));
				}
			}
			if let Some(e) = close {
				close_events.write(*e);
			}
		}
	}
}

/// This systsm intercepts the player if they attempt
/// to leave a level in any way,
/// and opens the feedback form instead
/// if it seems like it should be filled out
///
/// Once the tester fills in the feedback form,
/// the intercepted action is resubmited
/// by [`do_action_after_close_form`]
fn intercept_exit_from_level(
	query: Query<(
		Entity,
		&FadeAnimation,
		AnyOf<(&DoScreenTransition, &GotoNextLevel)>,
	)>,
	playing_level: PlayingLevelListEntry,
	playtest_log: Res<PlaytestLog>,
	mut commands: Commands,
	mut messgaes: MessageWriter<OpenFeedbackForm>,
) -> Result {
	// Skip the setup if we know there will be nothing to evaluate
	if query.is_empty() {
		return Ok(());
	}

	// Do not intercept if the user has already rated this level
	let level_key = &playing_level.get()?.identifier;
	if playtest_log.is_level_rated(level_key) {
		return Ok(());
	}

	for (id, animation, (next_screen, _)) in &query {
		// This means we are entering the level, so we should let it finish on its own
		if animation.is_past_peak() {
			continue;
		}
		// Cancel the transition by despawning it
		commands.entity(id).despawn();
		// Proceed to the player's selected action after closing the form
		let after_close = if let Some(DoScreenTransition(screen)) = next_screen {
			AfterExitFeedbackForm::ChangeScreen(*screen)
		} else {
			AfterExitFeedbackForm::NextLevel
		};
		// Open the form before letting the player proceed
		messgaes.write(OpenFeedbackForm(after_close));
	}

	Ok(())
}

fn synchronize_level_feedback(
	query: Query<Ref<StarRatingValue>, Changed<StarRatingValue>>,
	playing_level: PlayingLevelListEntry,
	mut playtest_log: ResMut<PlaytestLog>,
) -> Result {
	if query.is_empty() {
		return Ok(());
	}

	let level_key = &playing_level.get()?.identifier;

	for value in &query {
		// Do not respond to initialization, that is always a false signal
		// Do not respond when value is zero, it should not even be possible
		// to set it to zero
		if value.is_added() || **value == 0 {
			continue;
		}

		playtest_log.level_mut(level_key.clone()).stars = **value as u8;
	}
	Ok(())
}

fn update_submit_enable_disable(
	rating_q: Query<&StarRatingValue, Changed<StarRatingValue>>,
	mut submit_q: Query<&mut InteractionEnabled, With<ExitFeedbackForm>>,
) {
	let is_submitable = rating_q.iter().any(|&StarRatingValue(rating)| rating > 0);
	for mut enabled in &mut submit_q {
		if is_submitable {
			enabled.set_if_neq(InteractionEnabled(true));
		}
	}
}
