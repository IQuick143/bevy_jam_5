//! Checkbox-like UI elements

use crate::{ui::prelude::InteractionQuery, AppSet};
use bevy::prelude::*;

pub(super) fn plugin(app: &mut App) {
	app.add_systems(
		Update,
		(
			update_multistate_state.before(AppSet::RecordInput),
			update_multistate_labels,
		)
			.chain(),
	);
}

/// Marks a checkbox node and holds its current state
#[derive(Component, Clone, Copy, PartialEq, Eq, Debug, Default)]
#[require(Button)]
pub struct MultiStateButton {
	/// How many states the button has
	pub state_count: u32,
	/// Index of the current state
	pub current_state: u32,
}

impl MultiStateButton {
	pub fn new(state_count: u32, current_state: u32) -> Self {
		Self {
			state_count,
			current_state,
		}
	}
}

/// Support component for [`MultiStateButton`] that looks like a button
/// with a label that indicates its current state
#[derive(Component, Clone, Debug, Default, Deref, DerefMut)]
pub struct MultiStateButtonLabels(pub Vec<String>);

impl MultiStateButtonLabels {
	pub fn new(checked: impl IntoIterator<Item: Into<String>>) -> Self {
		Self(checked.into_iter().map(Into::into).collect())
	}
}

fn update_multistate_state(mut query: InteractionQuery<&mut MultiStateButton>) {
	for (interaction, enabled, mut button) in &mut query {
		if enabled.is_none_or(|e| **e) && *interaction == Interaction::Pressed {
			button.current_state += 1;
			if button.current_state >= button.state_count {
				button.current_state = 0;
			}
		}
	}
}

fn update_multistate_labels(
	checkbox_q: Query<
		(&MultiStateButton, &MultiStateButtonLabels, &Children),
		Changed<MultiStateButton>,
	>,
	mut text_q: Query<&mut Text>,
) {
	for (MultiStateButton { current_state, .. }, labels, children) in &checkbox_q {
		let maybe_child_text = children.first().and_then(|id| text_q.get_mut(*id).ok());
		let Some(mut child_text) = maybe_child_text else {
			warn!("Checkbox with CheckboxLabels does not have a text child");
			continue;
		};
		**child_text = labels
			.get(*current_state as usize)
			.cloned()
			.unwrap_or_default();
	}
}
