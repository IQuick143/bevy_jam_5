//! Extension to the playing screen in playtesting builds

use crate::{
	assets::{GlobalFont, HandleMap, ImageKey, UiButtonAtlas},
	drawing::ColorKey,
	graphics,
	screen::Screen,
	ui::{
		consts::*,
		hover::{self, *},
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
					close_feedback_form.run_if(on_message::<ExitFeedbackForm>),
				)
					.in_set(AppSet::ExecuteInput),
			),
		);
}

/// Marker component for the root of the feedback form
#[derive(Component, Clone, Copy, Debug, Default)]
struct FeedbackForm;

#[derive(Component, Message, Clone, Copy, Debug, Default)]
struct OpenFeedbackForm;

#[derive(Component, Message, Clone, Copy, Debug, Default)]
struct ExitFeedbackForm;

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
				OpenFeedbackForm,
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
	mut commands: Commands,
	ui_button_atlas: Res<UiButtonAtlas>,
	image_handles: Res<HandleMap<ImageKey>>,
	font: Res<GlobalFont>,
	existing_form: Query<(), With<FeedbackForm>>,
) {
	if !existing_form.is_empty() {
		warn!("Attempting to open feedback form when one is already open");
		return;
	}

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
							ExitFeedbackForm,
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
							ExitFeedbackForm,
						),
					],
				),
				(
					Node {
						padding: FEEDBACK_FORM_BODY_PADDING,
						..default()
					},
					children![(widgets::label("How did you like this level?", font.0.clone())),],
				),
			],
		)],
	));
}

fn close_feedback_form(mut commands: Commands, query: Query<Entity, With<FeedbackForm>>) {
	for id in &query {
		commands.entity(id).despawn();
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
					close_events.write(ExitFeedbackForm);
				}
			}
			if let Some(e) = close {
				close_events.write(*e);
			}
		}
	}
}
