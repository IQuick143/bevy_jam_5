//! UI-related constants for playtesting interface

use crate::{
	drawing::ColorKey,
	graphics,
	ui::{consts::*, interaction::InteractionPalette},
};
use bevy::{color::palettes::tailwind, prelude::*};

impl InteractionPalette {
	pub(super) const PLAYTEST_BUTTON: Self = Self {
		none: ColorKey::PlaytestMarker,
		..Self::SPRITE_BUTTON
	};

	pub(super) const CONFIRM_BUTTON: Self = Self::NEXT_LEVEL_BUTTON;
}

pub const FEEDBACK_FORM_WIDTH: Val = Val::Px(600.0);
pub const FEEDBACK_FORM_BG_COLOR: Color = Color::WHITE;
pub const FEEDBACK_FORM_FRAME_COLOR: Color = Color::Srgba(tailwind::SLATE_500);
pub const FEEDBACK_FORM_FRAME_WIDTH: Val = Val::Px(graphics::RING_OUTLINE_WIDTH);
pub const FEEDBACK_FORM_PADDING: UiRect = UiRect::axes(COMMON_GAP, Val::Px(5.0));
pub const FEEDBACK_FORM_BODY_PADDING: UiRect = UiRect::axes(Val::Px(40.0), Val::Px(15.0));
pub const FEEDBACK_FORM_BODY_GAP: Val = Val::Px(15.0);
pub const INPUT_TEXT_SIZE: f32 = 18.0;
pub const TEXT_INPUT_FRAME_WIDTH: Val = FEEDBACK_FORM_FRAME_WIDTH;
pub const TEXT_INPUT_PADDING: Val = Val::Px(10.0);
pub const TEXT_INPUT_MIN_HEIGHT: Val = Val::Px(INPUT_TEXT_SIZE * 4.0);
