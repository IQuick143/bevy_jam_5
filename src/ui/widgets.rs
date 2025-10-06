//! Helper traits for creating common widgets.

use super::{consts::*, interaction::InteractionPalette, palette::*};
use bevy::{prelude::*, ui::Val::*};

/// Largest type of buttons, use for menus
pub fn menu_button(text: impl Into<String>, font: Handle<Font>) -> impl Bundle {
	(
		Name::new("Button"),
		Button,
		Node {
			width: WIDE_BUTTON_WIDTH,
			height: MENU_BUTTON_HEIGHT,
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
				font_size: MENU_BUTTON_TEXT_SIZE,
				font,
				..default()
			},
			TextColor(BUTTON_TEXT),
		)],
	)
}

/// Large buttons for grids
pub fn grid_button(text: impl Into<String>, font: Handle<Font>) -> impl Bundle {
	(
		Name::new("Button"),
		Button,
		Node {
			width: WIDE_BUTTON_WIDTH,
			height: GRID_BUTTON_HEIGHT,
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
				font_size: COMMON_BUTTON_TEXT_SIZE,
				font,
				..default()
			},
			TextColor(BUTTON_TEXT),
		)],
	)
}

/// Small button for toolbars
pub fn toolbar_button(text: impl Into<String>, font: Handle<Font>) -> impl Bundle {
	(
		Name::new("Button"),
		Button,
		Node {
			padding: TOOLBAR_BUTTON_PADDING,
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
				font_size: COMMON_BUTTON_TEXT_SIZE,
				font,
				..default()
			},
			TextColor(BUTTON_TEXT),
		)],
	)
}

/// Small button whose size matches [`text`]
pub fn inline_button(text: impl Into<String>, font: Handle<Font>) -> impl Bundle {
	(
		Name::new("Button"),
		Button,
		Node {
			padding: INLINE_BUTTON_PADDING,
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
				font_size: COMMON_TEXT_SIZE,
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
