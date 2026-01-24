//! Extension to the playing screen in playtesting builds

use super::{
	consts::*,
	log::PlaytestLog,
	stars::{StarRating, StarRatingValue},
};
use crate::{
	assets::{GlobalFont, HandleMap, ImageKey, UiButtonAtlas},
	game::logic_relay::{RotateCycleGroup, RotationCause},
	save::SaveGame,
	screen::{
		DoScreenTransition, DoScreenTransitionCommands, GotoNextLevel, PlayingLevel,
		PlayingLevelListEntry, Screen,
	},
	send_message,
	ui::{
		consts::*,
		hover::{self, *},
		interaction::{InteractionEnabled, Unfreeze},
		prelude::*,
	},
	AppSet,
};
use bevy::{input::common_conditions::input_just_pressed, prelude::*};
use bevy_ui_text_input::TextInputContents;

pub(super) fn plugin(app: &mut App) {
	app.add_message::<OpenFeedbackForm>()
		.add_message::<ExitFeedbackForm>()
		.init_resource::<MoveCounter>()
		.add_systems(OnEnter(Screen::Playing), spawn_playtest_playing_ui)
		.add_systems(
			Update,
			(
				(
					record_playing_screen_input,
					send_message(ExitFeedbackForm(AfterExitFeedbackForm::Stay))
						.run_if(input_just_pressed(KeyCode::Escape).and(feedback_form_is_open)),
					emit_confirm_for_current_form.run_if(
						input_just_pressed(KeyCode::Enter)
							.and(feedback_form_is_open)
							.and(is_current_level_rated),
					),
				)
					.in_set(AppSet::RecordInput),
				(
					spawn_feedback_form.run_if(on_message::<OpenFeedbackForm>),
					(close_feedback_form, do_action_after_close_form)
						.run_if(on_message::<ExitFeedbackForm>),
				)
					.in_set(AppSet::ExecuteInput),
				intercept_exit_from_level
					.after(AppSet::ExecuteInput)
					.run_if(in_state(Screen::Playing).and(level_exit_should_be_intercepted)),
				synchronize_star_feedback,
				synchronize_text_feedback,
				update_submit_enable_disable,
				increment_move_counter.run_if(on_message::<RotateCycleGroup>),
				reset_move_counter.run_if(state_changed::<PlayingLevel>),
			),
		);
}

/// Marker component for the root of the feedback form
#[derive(Component, Clone, Copy, Debug, Default)]
struct FeedbackForm(AfterExitFeedbackForm);

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

/// Counts the number of forward moves made on a level
/// in a single session, but even through resets
#[derive(Resource, Clone, Copy, Debug, Default, Deref, DerefMut)]
struct MoveCounter(u32);

impl MoveCounter {
	/// When [`MoveCounter`] reaches this value, open the feedback form
	/// before letting the tester click away
	const MOVES_BEFORE_ASKING_FOR_FEEDBACK: u32 = 20;
}

/// Marker component for rating widgets for in-level feedback
#[derive(Component, Clone, Copy, PartialEq, Eq, Debug)]
enum LevelStarRating {
	/// General level quality rating
	General,
	/// Subjective difficulty rating
	Difficulty,
}

/// Marker component for text input widgets for in-level feedback
#[derive(Component, Clone, Copy, Debug)]
struct LevelTextInput;

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
	let level_data = playtest_log.get_level(level_key);
	let current_rating = level_data.map(|l| l.stars).unwrap_or_default() as u32;
	let current_difficulty = level_data.map(|l| l.difficulty).unwrap_or_default() as u32;
	let current_comment = level_data.map(|l| l.comment.as_str()).unwrap_or_default();

	commands.spawn((
		widgets::ui_root(),
		FeedbackForm(after_close),
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
						row_gap: FEEDBACK_FORM_BODY_GAP,
						flex_direction: FlexDirection::Column,
						align_items: AlignItems::Center,
						..default()
					},
					children![
						widgets::label("How did you like this level?", font.0.clone()),
						(
							StarRating::new(5),
							StarRatingValue(current_rating),
							LevelStarRating::General,
						),
						widgets::label("Subjective difficulty?", font.0.clone()),
						(
							StarRating::new(5),
							StarRatingValue(current_difficulty),
							LevelStarRating::Difficulty,
						),
						widgets::label("Any other comments?", font.0.clone()),
						super::widgets::text_input(current_comment, font.0.clone(), LevelTextInput),
					],
				),
			],
		)],
	));

	Ok(())
}

fn feedback_form_is_open(query: Query<(), With<FeedbackForm>>) -> bool {
	!query.is_empty()
}

fn is_current_level_rated(
	playing_level: PlayingLevelListEntry,
	playtest_log: Res<PlaytestLog>,
) -> Result<bool> {
	let level_key = &playing_level.get()?.identifier;
	let is_rated = playtest_log.is_level_rated(level_key);
	Ok(is_rated)
}

fn emit_confirm_for_current_form(
	form: Single<&FeedbackForm>,
	mut messages: MessageWriter<ExitFeedbackForm>,
) {
	let FeedbackForm(after_close) = *form;
	messages.write(ExitFeedbackForm(*after_close));
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
	mut commands: Commands,
	mut messgaes: MessageWriter<OpenFeedbackForm>,
) {
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
}

fn level_exit_should_be_intercepted(
	playing_level: PlayingLevelListEntry,
	playtest_log: Res<PlaytestLog>,
	save: Res<SaveGame>,
	move_count: Res<MoveCounter>,
) -> Result<bool> {
	// Do not intercept if the user has already rated this level
	let level_key = &playing_level.get()?.identifier;
	let is_rated = playtest_log.is_level_rated(level_key);
	if is_rated {
		return Ok(false);
	}

	// Do intercept if the level has been completed
	let is_completed = save.is_level_completed(level_key);
	if is_completed {
		return Ok(true);
	}

	// Intercept if the tester has tried and failed to solve
	// for a significant number of moves
	if **move_count >= MoveCounter::MOVES_BEFORE_ASKING_FOR_FEEDBACK {
		return Ok(true);
	}

	Ok(false)
}

fn synchronize_star_feedback(
	query: Query<(Ref<StarRatingValue>, &LevelStarRating), Changed<StarRatingValue>>,
	playing_level: PlayingLevelListEntry,
	mut playtest_log: ResMut<PlaytestLog>,
) -> Result {
	if query.is_empty() {
		return Ok(());
	}

	let level_key = &playing_level.get()?.identifier;

	for (value, rating) in &query {
		// Do not respond to initialization, that is always a false signal
		// Do not respond when value is zero, it should not even be possible
		// to set it to zero
		if value.is_added() || **value == 0 {
			continue;
		}

		let value = **value as u8;
		let level_data = playtest_log.level_mut(level_key.clone());
		match rating {
			LevelStarRating::General => level_data.stars = value,
			LevelStarRating::Difficulty => level_data.difficulty = value,
		}
	}
	Ok(())
}

fn synchronize_text_feedback(
	query: Query<Ref<TextInputContents>, (Changed<TextInputContents>, With<LevelTextInput>)>,
	playing_level: PlayingLevelListEntry,
	mut playtest_log: ResMut<PlaytestLog>,
) -> Result {
	if query.is_empty() {
		return Ok(());
	}

	let level_key = &playing_level.get()?.identifier;

	for value in &query {
		// Do not respond to initialization, that is always a false signal
		if value.is_added() {
			continue;
		}

		let level_data = playtest_log.level_mut(level_key.clone());
		level_data.comment = value.get().to_owned();
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

fn increment_move_counter(
	mut messages: MessageReader<RotateCycleGroup>,
	mut move_count: ResMut<MoveCounter>,
) {
	for message in messages.read() {
		if message.cause == RotationCause::Manual {
			**move_count += 1;
		}
	}
}

fn reset_move_counter(mut move_count: ResMut<MoveCounter>) {
	**move_count = 0;
}
