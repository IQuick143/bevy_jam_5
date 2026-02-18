use crate::ui::cyl::{
	navigation::{CylSubnavigation, SubnavigationMode},
	CylClickable, CylParentWidget, CylSubWidget, CylWidget,
};

use super::prelude::*;
use bevy::prelude::*;

pub(super) fn plugin(app: &mut App) {
	app.add_observer(checkbox_interaction)
		.add_observer(selector_move_button_interact)
		.add_observer(selector_select_event_capture)
		.add_observer(selector_subnavigation_capture);
}

/// A clickable Cyl button.
/// By default converts subnavigation to interactions.
/// To listen for when the button triggers, add an observer to [`CylInteract`].
#[derive(Component, Default)]
#[require(
	CylSubnavigation(SubnavigationMode::SubNavigateToInteract),
	CylClickable,
	CylWidget
)]
pub struct CylButton;

/// A clickable Cyl checkbox.
/// By default converts subnavigation to interactions.
/// To listen for when the checkbox changes, add an observer to [`TODO`].
#[derive(Component, Default)]
#[require(
	CylSubnavigation(SubnavigationMode::SubNavigateToInteract),
	CylClickable,
	CylWidget
)]
pub struct CylCheckbox(pub bool);

fn checkbox_interaction(event: On<CylInteract>, mut checkboxes: Query<&mut CylCheckbox>) {
	let entity = event.0;
	if let Ok(mut checkbox) = checkboxes.get_mut(entity) {
		checkbox.0 = !checkbox.0;
		// TODO: Emit an event?
	}
}

/// A movable Cyl selector logical entity.
/// Reacts to [`CylSelect`] and emits [`TODO`]
/// By default converts subnavigation to [`CylSelect`] events with offset +-1.
/// To listen for when the value changes, add an observer to [`TODO`].
#[derive(Component)]
#[require(CylSubnavigation(SubnavigationMode::SubNavigateCapture), CylWidget)]
pub struct CylSelector {
	/// The value currently being selected
	value: i64,
	/// Inclusive minimum
	minimum: i64,
	/// Inclusive maximum
	maximum: i64,
	/// Whether attempting going beyond one limit should jump to the other end
	pub is_wrapping: bool,
}

impl CylSelector {
	/// Return the internal value
	pub fn get(&self) -> i64 {
		self.value
	}

	/// Set the internal value, clamping it to the allowed range
	pub fn set_value_with_clamp(&mut self, mut value: i64) {
		if self.minimum > self.maximum {
			(self.maximum, self.minimum) = (self.minimum, self.maximum);
		}
		if value < self.minimum {
			value = self.minimum;
		}
		if value > self.maximum {
			value = self.maximum;
		}
		self.value = value;
	}

	/// Changes the internal value by an offset, clamping or wrapping it to the allowed range
	pub fn shift_value_by(&mut self, offset: i64) {
		// Yes I know this is overengineered, I was having fun.
		if !self.is_wrapping {
			self.set_value_with_clamp(i64::saturating_add(self.value, offset));
		} else {
			// Ensure current value is correctly clamped
			self.set_value_with_clamp(self.value);
			let interval_length_minus_1 = i64::abs_diff(self.minimum, self.maximum);
			// There is only one possible option and it had to have been set by `set_value_with_clamp`
			if interval_length_minus_1 == 0 {
				return;
			}
			let Some(interval_length) = u64::checked_add(interval_length_minus_1, 1) else {
				// The interval must include every `i64` number, so we can just use wrapping add
				self.value = i64::wrapping_add(self.value, offset);
				return;
			};
			let offset_from_start = i64::abs_diff(self.value, self.minimum);
			let new_unwrapped_offset = offset_from_start as i128 + offset as i128;
			let wrapped_offset =
				i128::rem_euclid(new_unwrapped_offset, interval_length as i128) as u64;
			self.value = self.minimum.saturating_add_unsigned(wrapped_offset);
		}
	}
}

fn selector_subnavigation_capture(
	event: On<CylNavigate>,
	selectors: Query<(), With<CylSelector>>,
	mut commands: Commands,
) {
	if event.is_subnavigation() {
		let entity = event.entity;
		if selectors.contains(entity) {
			commands.trigger(CylSelect {
				selector: entity,
				value: event.subnavigation_value() as i64,
				absolute: false,
			});
		}
	}
}

fn selector_select_event_capture(mut event: On<CylSelect>, mut selectors: Query<&mut CylSelector>) {
	let entity = event.selector;
	if let Ok(mut selector) = selectors.get_mut(entity) {
		event.propagate(false);
		if event.absolute {
			selector.set_value_with_clamp(event.value);
		} else {
			selector.shift_value_by(event.value);
		}
		// TODO: Emit an event?
	}
}

/// A specialised button for changing a selector which is a parent in the widget tree.
#[derive(Component)]
#[require(CylSubnavigation(SubnavigationMode::SubNavigatePropagate), CylButton)]
pub struct CylSelectorMoveButton {
	/// Value to change by/to the selector.
	value: i64,
	/// Whether to set absolutely (true) or be an offset (false)
	absolute: bool,
}

#[derive(Component, Default)]
#[require(
	CylSubnavigation(SubnavigationMode::SubNavigatePropagate),
	CylClickable,
	CylWidget
)]
pub struct CylSelectSubwidget;

fn selector_move_button_interact(
	event: On<CylInteract>,
	selector_buttons: Query<&CylSelectorMoveButton>,
	mut commands: Commands,
) {
	let entity = event.0;
	if let Ok(button) = selector_buttons.get(entity) {
		commands.trigger(CylSelect {
			selector: entity,
			value: button.value,
			absolute: button.absolute,
		});
	}
}

fn submenuselector_constructor(
	menu_constructors: Vec<(
		Box<dyn FnOnce(&mut Commands) -> Entity>,
		Box<dyn FnOnce(&mut Commands) -> Entity>,
	)>,
	wrapping: bool,
	allow_subnavigation_events_from_submenus: bool,
) -> impl FnOnce(&mut Commands) -> Entity {
	move |commands: &mut Commands| {
		let n_values = menu_constructors.len();
		let mut headers = Vec::new();
		let mut submenus = Vec::new();
		for (index, (header_constructor, menu_constructor)) in
			menu_constructors.into_iter().enumerate()
		{
			let header = header_constructor(commands);
			let submenu = menu_constructor(commands);
			headers.push(header);
			submenus.push(submenu);

			commands.entity(header).insert((
				CylParentWidget(Entity::PLACEHOLDER),
				CylSelectorMoveButton {
					value: index as i64,
					absolute: true,
				},
			));
		}
		let cyl_selector = commands
			.spawn(CylSelector {
				value: 0,
				minimum: 0,
				maximum: (n_values as i64) - 1,
				is_wrapping: wrapping,
			})
			.id();
		let header_holder = commands
			.spawn((
				CylParentWidget(cyl_selector),
				ChildOf(cyl_selector),
				// Subnavigation events are propagated to the selector which captures them
				CylSubnavigation(SubnavigationMode::SubNavigatePropagate),
				CylSelectSubwidget,
				CylWidget,
				Children::spawn(WithRelated(headers.clone().into_iter())),
			))
			.id();
		for header in headers {
			commands
				.entity(header)
				.insert(CylParentWidget(header_holder));
		}
		cyl_selector
	}
}
