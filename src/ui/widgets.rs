//! Helper traits for creating common widgets.

use super::{consts::*, interaction::InteractionPalette, palette::*};
use bevy::{prelude::*, ui::Val::*};

/// Largest type of buttons, use for menus
pub fn menu_button(text: impl Into<String>, font: Handle<Font>) -> impl Bundle {
	common_button(
		text.into(),
		font,
		MENU_BUTTON_TEXT_SIZE,
		default(),
		WIDE_BUTTON_WIDTH,
		MENU_BUTTON_HEIGHT,
	)
}

/// Large buttons for grids
pub fn grid_button(text: impl Into<String>, font: Handle<Font>) -> impl Bundle {
	common_button(
		text.into(),
		font,
		COMMON_BUTTON_TEXT_SIZE,
		default(),
		WIDE_BUTTON_WIDTH,
		GRID_BUTTON_HEIGHT,
	)
}

/// Small button for toolbars
pub fn toolbar_button(text: impl Into<String>, font: Handle<Font>) -> impl Bundle {
	common_button(
		text.into(),
		font,
		COMMON_BUTTON_TEXT_SIZE,
		TOOLBAR_BUTTON_PADDING,
		Auto,
		Auto,
	)
}

/// Small button whose size matches [`text`]
pub fn inline_button(text: impl Into<String>, font: Handle<Font>) -> impl Bundle {
	common_button(
		text.into(),
		font,
		COMMON_TEXT_SIZE,
		INLINE_BUTTON_PADDING,
		Auto,
		Auto,
	)
}

/// Base of all button bundles
pub fn common_button(
	text: String,
	font: Handle<Font>,
	font_size: f32,
	padding: UiRect,
	width: Val,
	height: Val,
) -> impl Bundle {
	(
		Name::new("Button"),
		Button,
		Node {
			padding,
			width,
			height,
			justify_content: JustifyContent::Center,
			align_items: AlignItems::Center,
			..default()
		},
		BackgroundColor(NODE_BACKGROUND),
		InteractionPalette {
			none: NODE_BACKGROUND,
			hovered: BUTTON_HOVERED_BACKGROUND,
			pressed: BUTTON_PRESSED_BACKGROUND,
		},
		children![(
			Name::new("Button Text"),
			Text::new(text),
			TextFont {
				font_size,
				font,
				..default()
			},
			TextColor(BUTTON_TEXT),
		)],
	)
}

/// Header label. Bigger than [`Widgets::label`]
pub fn header(text: impl Into<String>, font: Handle<Font>) -> impl Bundle {
	(
		Name::new("Header"),
		Node {
			width: LABEL_WIDTH,
			height: HEADING_HEIGHT,
			justify_content: JustifyContent::Center,
			align_items: AlignItems::Center,
			..default()
		},
		BackgroundColor(NODE_BACKGROUND),
		children![(
			Name::new("Header Text"),
			Text::new(text),
			TextFont {
				font_size: HEADING_TEXT_SIZE,
				font,
				..default()
			},
			TextColor(HEADER_TEXT),
		)],
	)
}

/// Simple text label
pub fn label(text: impl Into<String>, font: Handle<Font>) -> impl Bundle {
	(
		Name::new("Label"),
		Node {
			width: LABEL_WIDTH,
			justify_content: JustifyContent::Center,
			align_items: AlignItems::Center,
			..default()
		},
		children![(
			Name::new("Label Text"),
			Text::new(text),
			TextFont {
				font_size: COMMON_TEXT_SIZE,
				font,
				..default()
			},
			TextColor(LABEL_TEXT),
		)],
	)
}

/// Aligned text label with automatic width
pub fn text(text: impl Into<String>, align: JustifyContent, font: Handle<Font>) -> impl Bundle {
	(
		Name::new("Text"),
		Node {
			justify_content: align,
			align_items: AlignItems::Center,
			..default()
		},
		children![(
			Name::new("Text Content"),
			Text::new(text),
			TextFont {
				font_size: COMMON_TEXT_SIZE,
				font,
				..default()
			},
			TextColor(LABEL_TEXT),
		)],
	)
}

/// Root node that covers the full screen
/// and centers its content horizontally and vertically
pub fn ui_root() -> impl Bundle {
	ui_root_justified(JustifyContent::Center)
}

pub fn ui_root_justified(justify_content: JustifyContent) -> impl Bundle {
	(
		Name::new("UI Root"),
		Node {
			width: Percent(100.0),
			height: Percent(100.0),
			justify_content,
			align_items: AlignItems::Center,
			flex_direction: FlexDirection::Column,
			row_gap: COMMON_GAP,
			position_type: PositionType::Absolute,
			..default()
		},
	)
}
