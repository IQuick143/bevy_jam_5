//! Common widgets for playtesting interface

use super::consts::*;
use crate::drawing::{ColorKey, TextColorKey};
use bevy::prelude::*;
use bevy_ui_text_input::{
	TextInputContents, TextInputNode, TextInputQueue,
	actions::{TextInputAction, TextInputEdit},
};

pub fn text_input(
	value: impl Into<String>,
	font: Handle<Font>,
	base_height: f32,
	extra: impl Bundle,
) -> impl Bundle {
	let mut queue = TextInputQueue::default();
	queue.add(TextInputAction::Edit(TextInputEdit::Paste(value.into())));

	(
		Node {
			width: Val::Percent(100.0),
			padding: UiRect::all(TEXT_INPUT_PADDING),
			border: UiRect::all(TEXT_INPUT_FRAME_WIDTH),
			..default()
		},
		BorderColor::all(FEEDBACK_FORM_FRAME_COLOR),
		BackgroundColor(TEXT_INPUT_BACKGROUND),
		children![(
			TextInputNode::default(),
			TextInputContents::default(),
			Node {
				width: Val::Percent(100.0),
				box_sizing: BoxSizing::ContentBox,
				min_height: Val::Px(INPUT_TEXT_SIZE * INPUT_LINE_HEIGHT * base_height),
				..default()
			},
			TextColorKey(ColorKey::UiLabelText),
			TextFont {
				font_size: INPUT_TEXT_SIZE,
				font,
				..default()
			},
			queue,
			extra,
		)],
	)
}
