use bevy::{color::palettes::tailwind::*, prelude::*};

pub const BUTTON_HOVERED_BACKGROUND: Color = Color::Srgba(SLATE_500);
pub const BUTTON_PRESSED_BACKGROUND: Color = Color::Srgba(SLATE_300);
pub const BUTTON_DISABLED_BACKGROUND: Color = Color::Srgba(SLATE_300);

pub const BUTTON_TEXT: Color = Color::Srgba(SLATE_50);
pub const LABEL_TEXT: Color = Color::Srgba(SLATE_800);
pub const HEADER_TEXT: Color = BUTTON_TEXT;

pub const NODE_BACKGROUND: Color = Color::Srgba(SLATE_400);

pub const SPRITE_BUTTON_FILL: Color = Color::Srgba(SLATE_200);
pub const SPRITE_BUTTON_HOVERED: Color = Color::Srgba(SLATE_400);
pub const SPRITE_BUTTON_PRESSED: Color = Color::Srgba(SLATE_500);
pub const SPRITE_BUTTON_DISABLED: Color = Color::Srgba(Srgba {
	alpha: 0.5,
	..SLATE_200
});

pub const NEXT_LEVEL_BUTTON_BACKGROUND: Color = Color::Srgba(GREEN_400);
pub const NEXT_LEVEL_BUTTON_HOVERED_BACKGROUND: Color = Color::Srgba(GREEN_500);
pub const NEXT_LEVEL_BUTTON_PRESSED_BACKGROUND: Color = Color::Srgba(GREEN_600);

pub const SLIDER_OUTLINE: Color = Color::Srgba(SLATE_700);
pub const SLIDER_FILL: Color = Color::Srgba(SLATE_200);
pub const SLIDER_HOVERED_FILL: Color = Color::Srgba(SLATE_400);
pub const SLIDER_PRESSED_FILL: Color = Color::Srgba(SLATE_100);
pub const SLIDER_DISABLED_FILL: Color = Color::Srgba(SLATE_100);
