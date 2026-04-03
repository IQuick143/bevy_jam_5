use super::hierarchy::CylParentWidget;
use bevy::prelude::*;

/// Cyl [`Event`] describing an interaction happening, represents
/// something like a button press, checkbox toggle, or confirmation.
/// Does not automatically propagate
#[derive(EntityEvent, Debug)]
pub struct CylInteract(pub Entity);

/// Cyl [`Event`] describing an subnavigation happening, represents
/// something like moving left/right on a slider.
/// Does not automatically propagate, but [`navigation_system`] emits it for each entity where a subnavigation happened.
#[derive(EntityEvent, Debug)]
pub struct CylSubnavigate {
	#[event_target]
	pub entity: Entity,
	pub is_positive: bool,
}

/// Cyl [`Event`] describing a change in a selector, caught by a [`CylSelector`].
///
/// Note: The selector may choose to ignore this event. (For example because of )
/// Automatically propagates up the widget hiearchy until stopped
///
/// [`CylSelector`]: super::widgets::CylSelector
#[derive(EntityEvent, Debug)]
#[entity_event(propagate = &'static CylParentWidget, auto_propagate)]
pub struct CylSelect {
	#[event_target]
	pub selector: Entity,
	pub value: i64,
	/// Whether this value should be used as a relative offset (false)
	/// or directly set the absolute value (true)
	pub absolute: bool,
}
