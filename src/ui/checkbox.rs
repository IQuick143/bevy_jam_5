//! Checkbox-like UI elements

use crate::{ui::prelude::InteractionQuery, AppSet};
use bevy::prelude::*;

pub(super) fn plugin(app: &mut App) {
	app.add_systems(
		Update,
		(
			update_checkbox_state.before(AppSet::RecordInput),
			update_checkbox_labels,
		)
			.chain(),
	);
}

/// Marks a checkbox node and holds its current state
#[derive(Component, Clone, Copy, PartialEq, Eq, Debug, Default, Deref, DerefMut)]
#[require(Button)]
pub struct Checkbox(pub bool);

/// Support component for [`Checkbox`] that looks like a button
/// with a label that indicates its current state
#[derive(Component, Clone, Debug)]
pub struct CheckboxLabels {
	/// Text to place on the checkbox when it is checked
	pub checked: String,
	/// Text to place on the checkbox when it is unchecked
	pub unchecked: String,
}

impl CheckboxLabels {
	pub fn new(checked: impl Into<String>, unchecked: impl Into<String>) -> Self {
		Self {
			checked: checked.into(),
			unchecked: unchecked.into(),
		}
	}
}

fn update_checkbox_state(mut query: InteractionQuery<&mut Checkbox>) {
	for (interaction, mut checkbox) in &mut query {
		if *interaction == Interaction::Pressed {
			**checkbox = !**checkbox;
		}
	}
}

fn update_checkbox_labels(
	checkbox_q: Query<(&Checkbox, &CheckboxLabels, &Children), Changed<Checkbox>>,
	mut text_q: Query<&mut Text>,
) {
	for (Checkbox(is_checked), labels, children) in &checkbox_q {
		let maybe_child_text = children.first().and_then(|id| text_q.get_mut(*id).ok());
		let Some(mut child_text) = maybe_child_text else {
			warn!("Checkbox with CheckboxLabels does not have a text child");
			continue;
		};
		**child_text = if *is_checked {
			labels.checked.clone()
		} else {
			labels.unchecked.clone()
		};
	}
}
