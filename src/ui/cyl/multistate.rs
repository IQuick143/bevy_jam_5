//! Base functionality of all multistate widgets

use crate::ui::cyl::events::CylSubnavigate;

use super::{CylWidget, events::CylInteract, input::TakesFocusFromPointer};
use bevy::prelude::*;

pub(super) fn plugin(app: &mut App) {
	app.add_observer(trigger_multistates)
		.add_observer(trigger_multistates_with_interact);
}

/// Marker component for generic multistate widget
#[derive(Component, Clone, Copy, Debug, Default)]
#[require(CylWidget, TakesFocusFromPointer)]
pub struct CylMultistate {
	/// Whether an interact-type input should also trigger the widget
	pub trigger_on_interact: bool,
}

#[derive(EntityEvent)]
pub struct CylMultistateTrigger {
	#[event_target]
	pub entity: Entity,
	pub direction: MultistateTriggerDirection,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum MultistateTriggerDirection {
	Forward,
	Backward,
}

impl MultistateTriggerDirection {
	pub fn to_i32(self) -> i32 {
		match self {
			Self::Forward => 1,
			Self::Backward => -1,
		}
	}
}

fn trigger_multistates(
	event: On<CylSubnavigate>,
	mut commands: Commands,
	query: Query<(), With<CylMultistate>>,
) {
	// Only respond to the event on mutistate widgets
	if !query.contains(event.entity) {
		return;
	}

	let direction = match event.is_positive {
		false => MultistateTriggerDirection::Backward,
		true => MultistateTriggerDirection::Forward,
	};

	commands.trigger(CylMultistateTrigger {
		entity: event.entity,
		direction,
	});
}

fn trigger_multistates_with_interact(
	event: On<CylInteract>,
	mut commands: Commands,
	query: Query<&CylMultistate>,
) {
	let entity = event.event_target();
	if query
		.get(event.event_target())
		.ok()
		.is_some_and(|m| m.trigger_on_interact)
	{
		commands.trigger(CylMultistateTrigger {
			entity,
			direction: MultistateTriggerDirection::Forward,
		});
	}
}
