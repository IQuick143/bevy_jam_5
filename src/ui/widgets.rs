//! Helper traits for creating common widgets.

use super::{
	consts::*,
	interaction::{InteractionPalette, InteractionPaletteForChildSprites},
};
use crate::{
	assets::UiButtonAtlas,
	drawing::{ColorKey, NodeColorKey, TextColorKey},
};
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
		GRID_BUTTON_PADDING,
		Val::Auto,
		GRID_BUTTON_HEIGHT,
	)
}

/// Button rendered by sprite from the UI sprite atlas
pub fn sprite_button(sprites: &UiButtonAtlas, sprite_index: usize) -> impl Bundle {
	common_sprite_button(
		sprites.image.clone(),
		InteractionPalette::SPRITE_BUTTON,
		UiButtonAtlas::TILE_SIZE.x as f32 / UiButtonAtlas::TILE_SIZE.y as f32,
		Some(TextureAtlas {
			layout: sprites.layout.clone(),
			index: sprite_index,
		}),
	)
}

/// Button rendered by sprite
pub fn common_sprite_button(
	image: Handle<Image>,
	palette: InteractionPalette,
	aspect_ratio: f32,
	texture_atlas: Option<TextureAtlas>,
) -> impl Bundle {
	(
		Name::new("Button"),
		Button,
		Node {
			height: Val::Px(SPRITE_BUTTON_HEIGHT),
			width: Val::Px(SPRITE_BUTTON_HEIGHT * aspect_ratio),
			..default()
		},
		palette,
		InteractionPaletteForChildSprites,
		children![(
			Name::new("Button Image"),
			ImageNode {
				image,
				texture_atlas,
				..default()
			},
			NodeColorKey(palette.none),
		)],
	)
}

/// Small button whose size matches [`text`]
pub fn inline_button(text: impl Into<String>, font: Handle<Font>) -> impl Bundle {
	common_button(
		text.into(),
		font,
		COMMON_TEXT_SIZE,
		INLINE_BUTTON_PADDING,
		INLINE_BUTTON_WIDTH,
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
		NodeColorKey(ColorKey::NodeBackground),
		InteractionPalette::COMMON_BUTTON,
		children![(
			Name::new("Button Text"),
			Text::new(text),
			TextFont {
				font_size,
				font,
				..default()
			},
			TextColorKey(ColorKey::UiButtonText),
		)],
	)
}

/// Header label. Bigger than [`label`]
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
		NodeColorKey(ColorKey::NodeBackground),
		children![(
			Name::new("Header Text"),
			Text::new(text),
			TextFont {
				font_size: HEADING_TEXT_SIZE,
				font,
				..default()
			},
			TextColorKey(ColorKey::HeaderText),
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
			TextColorKey(ColorKey::UiLabelText),
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
			TextColorKey(ColorKey::UiLabelText),
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
