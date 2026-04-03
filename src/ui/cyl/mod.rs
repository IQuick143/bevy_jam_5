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
//! [`CylSubWidget`]: hierarchy::CylSubWidget
//! [`CylParentWidget`]: hierarchy::CylParentWidget
//! [`CylButton`]: widgets::CylButton
//! [`CylCheckbox`]: widgets::CylCheckbox
//! [`CylSelector`]: widgets::CylSelector
//! [`CylInteract`]: events::CylInteract
//! [`NavigationInput`]: input::NavigationInput
//! [`InteractInput`]: input::InteractInput
//! [`CylChangeFocus`]: navigation::CylChangeFocus

pub mod events;
pub mod hierarchy;
pub mod input;
pub mod list;
pub mod multistate;
pub mod navigation;
pub mod widgets;

#[allow(unused_imports)]
pub mod prelude {
	pub use super::{
		CylClickable, CylWidget, events::*, hierarchy::*, input::*, list::*, multistate::*,
		navigation::*, widgets::*,
	};
}

use bevy::prelude::*;

pub(super) fn plugin(app: &mut App) {
	app.add_plugins((
		hierarchy::plugin,
		input::plugin,
		list::plugin,
		multistate::plugin,
		navigation::plugin,
		widgets::plugin,
	))
	.configure_sets(
		PostUpdate,
		(CylSystems::HierarchySetup, CylSystems::RootSetup).chain(),
	);
}

#[derive(SystemSet, Clone, Copy, PartialEq, Eq, Debug, Hash)]
enum CylSystems {
	HierarchySetup,
	RootSetup,
}

/// Marker component that defines a widget
#[derive(Component, Default)]
pub struct CylWidget;

/// Marker component that defines that a widget reacts to pointer events.
#[derive(Component, Default)]
pub struct CylClickable;
