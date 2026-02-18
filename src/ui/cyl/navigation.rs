///! Navigation design:
///! Entities that can be navigated by Cyl must be [`CylWidget`]'s
///! and should be structured in a hiearchy of [``]

use crate::ui::cyl::{CylParentWidget, CylWidgetData};
use super::prelude::*;
use bevy::prelude::*;

pub fn plugin(app: &mut App) {
	app.init_resource::<CylCurrentFocusedWidget>()
		.add_observer(subnavigation_propagation_observer);
}

/// Resource for keeping track of focus state.
#[derive(Resource, Default)]
pub struct CylCurrentFocusedWidget {
	/// Focus of the cursor.
	focus_entities: FocusHiearchy,
	/// Whether the entity is actively focused and hovered, or the focus is just there to be able to regain focus.
	hard_focus: bool,
}

/// A hiearchy of entities representing a focused object in the widget tree.
/// The first entity is the true desired entity to be focused, but if it is absent, entities higher up the tree can be used to regain focus.
#[derive(Default)]
pub struct FocusHiearchy(Vec<Entity>);

impl CylCurrentFocusedWidget {
	pub fn new() -> Self {
		CylCurrentFocusedWidget {
			focus_entities: FocusHiearchy(Vec::new()),
			hard_focus: false,
		}
	}

	/// Gets the entity that should currently be hovered.
	/// `None` if [`Self::hard_focus`] is `false` or there are no focus_entities.
	pub fn hovered_widget(&self) -> Option<Entity> {
		if self.hard_focus {
			self.focus_entities.0.first().copied()
		} else {
			None
		}
	}

	/// Clears all focus, removing all entities from the focus hiearchy and disables hard focus.
	pub fn clear_focus(&mut self) {
		self.focus_entities.0.clear();
		self.hard_focus = false;
	}

	/// Tries re-obtaining a valid widget to focus at and enables hard_focus iff it succeeds.
	/// `prefer_last` dictates whether the first or last entity should be preferred when trying to pick a path down a tree.
	///
	/// Returns whether the focused entity or `hard_focus` changed
	pub fn refocus(&mut self, prefer_last: bool, widget_data: &CylWidgetData) -> bool {
		let initial_focus = self.focus_entities.0.first().copied();
		let initial_hard_focus = self.hard_focus;

		let mut entity_to_start_focus = None;
		// Look for best match - first existing entity from the end of the list.
		for &entity in self.focus_entities.0.iter() {
			if widget_data.is_widget(entity) {
				entity_to_start_focus = Some(entity);
				break;
			}
		}
		// If nothing was found, try to look for a root node to start from.
		if entity_to_start_focus.is_none() {
			entity_to_start_focus = widget_data.pick_root();
		}
		// Clear current focus data.
		// Give up, no focus.
		let Some(mut entity_to_focus) = entity_to_start_focus else {
			self.clear_focus();
			// Focus changed iff there was some at the start.
			return initial_focus.is_some() || initial_hard_focus;
		};

		// Travel down the tree to find a suitable leaf to inherit the focus throne
		for _i in 0..255 {
			if let Some(entity) =
				widget_data.find_navigation_subwidget(entity_to_focus, prefer_last)
			{
				entity_to_focus = entity;
			} else {
				break;
			}
		}

		self.focus(entity_to_focus, widget_data);

		initial_focus == self.focus_entities.0.first().copied()
			|| initial_hard_focus == self.hard_focus
	}

	/// Focuses the given widget and enables `hard_focus`
	pub fn focus(&mut self, entity: Entity, widget_data: &CylWidgetData) {
		self.clear_focus();
		// We have something to focus, so we will end up with hard focus enabled.
		self.hard_focus = true;
		// Travel up the tree to note the focus entity with its noble ancestry
		let mut entity_to_focus = entity;
		for _i in 0..255 {
			self.focus_entities.0.push(entity_to_focus);
			if let Some(entity) = widget_data.get_parent(entity_to_focus) {
				entity_to_focus = entity;
			} else {
				break;
			}
		}
	}

	/// Checks if the provided entity is being hovered/hard focused, and if so, disables hard focus on it.
	pub fn unhover(&mut self, entity_losing_focus: Entity) {
		if self.hovered_widget() == Some(entity_losing_focus) {
			self.hard_focus = false;
		}
	}
}

/// Cyl [`event`] describing that the user is trying to navigate from this entity to another one
/// Automatically propagates up the widget hiearchy until stopped
#[derive(EntityEvent)]
#[entity_event(propagate = &'static CylParentWidget, auto_propagate)]
pub struct CylNavigate {
	#[event_target]
	pub entity: Entity,
	pub navigation_direction: NavigationDirection,
}

// TODO: Support flipping the direction of navigation and subnavigation
impl CylNavigate {
	pub fn is_subnavigation(&self) -> bool {
		!self.navigation_direction.is_vertical()
	}

	pub fn subnavigation_value(&self) -> i32 {
		match self.navigation_direction {
			NavigationDirection::Up => 0,
			NavigationDirection::Down => 0,
			NavigationDirection::Left => -1,
			NavigationDirection::Right => 1,
		}
	}
}

#[derive(Clone, Copy)]
pub enum NavigationDirection {
	Up,
	Down,
	Left,
	Right,
}

impl NavigationDirection {
	pub fn is_vertical(&self) -> bool {
		match self {
			Self::Up => true,
			Self::Down => true,
			Self::Left => false,
			Self::Right => false,
		}
	}

	pub fn going_up(&self) -> bool {
		matches!(self, NavigationDirection::Up)
	}
}

/// Component describing the behaviour this entity has when encountering [`CylNavigate`] events.
#[derive(Component, Default)]
pub struct CylSubnavigation(pub SubnavigationMode);

#[derive(Default)]
pub enum SubnavigationMode {
	/// Capture and converts subnavigation events and converts them to interactions ([`super::events::CylInteract`]).
	SubNavigateToInteract,
	/// Capture the subnavigation event, default for a widget.
	#[default]
	SubNavigateCapture,
	/// Send the event up the hiearchy
	SubNavigatePropagate,
}

/// Component overriding the default choices of subwidget for navigation
#[derive(Component)]
pub struct CylSubwidgetNavigationOverride {
	/// Subwidget that should ideally be the chosen target
	pub widget: Option<Entity>,
	/// If the widget selected is not applicable for whatever reason; whether one should prefer to have no subwidget.
	pub strict: bool,
}

/// Cyl [`Event`] describing a change in focus, usually emitted by a widget to draw focus to itself
pub struct CylChangeFocus(pub Entity);

fn subnavigation_propagation_observer(
	mut event: On<CylNavigate>,
	subnav_settings: Query<&CylSubnavigation>,
	mut commands: Commands,
) {
	if event.is_subnavigation() {
		if let Ok(CylSubnavigation(mode)) = subnav_settings.get(event.event_target()) {
			match mode {
				SubnavigationMode::SubNavigateToInteract => {
					event.propagate(false);
					commands.trigger(CylInteract(event.event_target()));
				}
				SubnavigationMode::SubNavigateCapture => {
					event.propagate(false);
				}
				SubnavigationMode::SubNavigatePropagate => {}
			}
		}
	}
}
