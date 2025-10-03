//! Helper traits for creating common widgets.

use super::{interaction::InteractionPalette, palette::*};
use bevy::{prelude::*, ui::Val::*};

/// Largest type of buttons, use for menus
pub fn menu_button(text: impl Into<String>, font: Handle<Font>) -> impl Bundle {
	(
		Name::new("Button"),
		Button,
		Node {
			width: Px(200.0),
			height: Px(65.0),
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
				font_size: 40.0,
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
			width: Px(200.0),
			height: Px(45.0),
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
				font_size: 30.0,
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
			padding: UiRect::all(Px(10.0)),
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
				font_size: 30.0,
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
			padding: UiRect::axes(Px(10.0), Px(5.0)),
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
				font_size: 24.0,
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
			width: Px(500.0),
			height: Px(65.0),
			justify_content: JustifyContent::Center,
			align_items: AlignItems::Center,
			..default()
		},
		BackgroundColor(NODE_BACKGROUND),
		children![(
			Name::new("Header Text"),
			Text::new(text),
			TextFont {
				font_size: 40.0,
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
			width: Px(500.0),
			justify_content: JustifyContent::Center,
			align_items: AlignItems::Center,
			..default()
		},
		children![(
			Name::new("Label Text"),
			Text::new(text),
			TextFont {
				font_size: 24.0,
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
				font_size: 24.0,
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
			row_gap: Px(10.0),
			position_type: PositionType::Absolute,
			..default()
		},
	)
}
