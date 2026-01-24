//! Common widgets for playtesting interface

use super::consts::*;
use crate::drawing::{ColorKey, TextColorKey};
use bevy::prelude::*;
use bevy_ui_text_input::{
	actions::{TextInputAction, TextInputEdit},
	TextInputContents, TextInputNode, TextInputQueue,
};

pub fn text_input(value: impl Into<String>, font: Handle<Font>, extra: impl Bundle) -> impl Bundle {
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
		children![(
			TextInputNode::default(),
			TextInputContents::default(),
			Node {
				width: Val::Percent(100.0),
				min_height: TEXT_INPUT_MIN_HEIGHT,
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
