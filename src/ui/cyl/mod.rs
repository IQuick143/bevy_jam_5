//! Cyl UI - A UI widget framework focusing on simple UI structures
//! that can be fully navigated by keyboards and gamepads.
//!
//! # Core design:
//! UI should be structured into a tree of widgets (via the relations
//! [`CylSubWidget`], [`CylParentWidget`]). One of these widgets may have
//! focus and events will be dispatched to that widget. Events from constituents
//! of a widget may bubble upwards.
//!
//! # Input:
//! There are two kinds of base input events:
//! Pointer Events
//!  - Pointer hover is automatically converted into navigation focus
//!  - Pointer click on widgets labelled with [`CylClickable`]
//!    is converted into [`CylInteract`]
//!
//! Non-pointing Events, which include:
//!  - [`NavigationInput`] - Directional input (4 cardinal directions, think WASD or arrow keys)
//!  - [`InteractInput`] - Interaction (one input, think enter key),
//!    dispatches to [`CylInteract`] on a focused widget.
//!  - These events are input agnostic, the user is responsible for emitting
//!    them based on whatever input schema they support
//!  - These are automatically dispatched to the focused entity or they change
//!    which entity is being focused.
//!
//! # Navigation tree:
//! Widgets that contain subwidgets by default are treated as a list of widgets
//! that should be navigated through via up/down navigation.
//! You can change this behaviour via [`CylSubwidgetNavigationOverride`],
//! which in strict mode can avoid navigation falling from this widget down
//! to subwidgets, or only to a selected subwidget.
//!
//! Note: Pointer hover can still set navigation to a subwidget bypassing the override.
//! Widgets may use [`CylChangeFocus`] events to address this however they like.
//!
//! # Widgets:
//!  - [`CylButton`] - a simple button that responds to clicks and interactions,
//!    emits [`CylInteract`]
//!  - [`CylCheckbox`] - a checkbox that behaves similarly to the button,
//!    but keeps toggle state
//!  - [`CylSelector`] - a selector base widget that uses subnavigation
//!    (left/right) to change a selected variant
//!    - [`CylSubmenuSelector`] - a selector that allows for changing between
//!      a preset amount of submenus TODO!
//!
//! [`CylButton`]: widgets::CylButton
//! [`CylCheckbox`]: widgets::CylCheckbox
//! [`CylSelector`]: widgets::CylSelector
//! [`CylInteract`]: events::CylInteract
//! [`NavigationInput`]: input::NavigationInput
//! [`InteractInput`]: input::InteractInput
//! [`CylChangeFocus`]: navigation::CylChangeFocus

pub mod events;
pub mod input;
pub mod navigation;
pub mod widgets;

#[allow(unused_imports)]
pub mod prelude {
	pub use super::events::*;
	pub use super::input::*;
	pub use super::navigation::*;
	pub use super::widgets::*;
}

use bevy::{
	app::App,
	ecs::{
		component::Component,
		entity::Entity,
		query::{With, Without},
		system::{Query, SystemParam},
	},
};

use crate::ui::cyl::prelude::{CylSubnavigation, CylSubwidgetNavigationOverride};

pub fn plugin(app: &mut App) {
	app.add_plugins((input::plugin, navigation::plugin, widgets::plugin));
}

/// Convenience [`SystemParam`] for accessing data about hiearchies of widgets.
#[derive(SystemParam)]
struct CylWidgetData<'w, 's> {
	widget_query: Query<
		'w,
		's,
		(
			Option<&'static CylSubWidget>,
			Option<&'static CylParentWidget>,
			Option<&'static CylSubwidgetNavigationOverride>,
		),
		With<CylWidget>,
	>,
	widget_root_query: Query<'w, 's, Entity, (With<CylWidget>, Without<CylParentWidget>)>,
}

impl CylWidgetData<'_, '_> {
	/// Tests if an entity has a [`CylWidget`] marker
	pub fn is_widget(&self, entity: Entity) -> bool {
		self.widget_query.contains(entity)
	}

	/// Finds a parent widget for the entity, if exists
	pub fn get_parent(&self, entity: Entity) -> Option<Entity> {
		self.widget_query
			.get(entity)
			.ok()
			.and_then(|(_, parent, _)| parent)
			.map(|CylParentWidget(entity)| entity)
			.copied()
	}

	/// Find a subwidget that should be navigated to, respecting subwidget_override if possible.
	pub fn find_navigation_subwidget(&self, entity: Entity, prefer_last: bool) -> Option<Entity> {
		let Ok((Some(subwidgets), _, subwidget_override)) = self.widget_query.get(entity) else {
			return None;
		};
		let strict_override = subwidget_override.map_or(false, |over| over.strict);
		let mut override_entity =
			subwidget_override
				.map(|over| over.widget)
				.flatten()
				.filter(|override_entity| {
					// Check that the override is actually a subwidget
					if let Ok((_, Some(parent), _)) = self.widget_query.get(*override_entity) {
						parent.0 == entity
					} else {
						false
					}
				});
		if strict_override || override_entity.is_some() {
			return override_entity;
		}
		// No override was found, so we find our own candidate.
		if prefer_last {
			subwidgets.0.last().copied()
		} else {
			subwidgets.0.first().copied()
		}
	}

	/// Selects a root widget.
	pub fn pick_root(&self) -> Option<Entity> {
		self.widget_root_query.iter().next()
	}
}

/// Marker component that defines a widget
#[derive(Component, Default)]
#[require(CylSubnavigation)]
pub struct CylWidget;

/// Marker component that defines that a widget reacts to pointer events.
#[derive(Component, Default)]
pub struct CylClickable;

// Relationships

#[derive(Component)]
#[relationship(relationship_target = CylSubWidget)]
#[require(CylWidget)]
pub struct CylParentWidget(pub Entity);

#[derive(Component)]
#[relationship_target(relationship = CylParentWidget)]
#[require(CylWidget)]
pub struct CylSubWidget(Vec<Entity>);
