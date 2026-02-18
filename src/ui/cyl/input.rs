use crate::ui::cyl::{CylClickable, CylWidgetData};
use bevy::prelude::*;

use super::prelude::*;

pub fn plugin(app: &mut App) {
	app.add_observer(dispatch_navigation)
		.add_observer(dispatch_input)
		.add_observer(on_pointer_click)
		.add_observer(on_pointer_out)
		.add_observer(on_pointer_over);
}

/// Event that should be fired in order to perform navigation by keyboard (or controller).
#[derive(Event)]
pub struct NavigationInput(NavigationDirection);

/// Event that should be fired in order to perform an input by keyboard (or controller).
#[derive(Event)]
pub struct InteractInput;

fn dispatch_navigation(
	event: On<NavigationInput>,
	mut focused_entity: Option<ResMut<CylCurrentFocusedWidget>>,
	widget_data: CylWidgetData,
	mut commands: Commands,
) {
	let mut focused_entity = match focused_entity {
		Some(focused_entity) => *focused_entity,
		None => CylCurrentFocusedWidget::new(),
	};
	let start_from_end = event.0.going_up();
	focused_entity.refocus(start_from_end, &widget_data);

	let hover = focused_entity.hovered_widget();
	commands.insert_resource(focused_entity);

	if let Some(entity) = hover {
		commands.trigger(CylNavigate {
			entity,
			navigation_direction: event.0,
		});
	}
}

fn dispatch_input(
	event: On<InteractInput>,
	widget_data: CylWidgetData,
	mut focused_entity: Option<ResMut<CylCurrentFocusedWidget>>,
	mut commands: Commands,
) {
	if let Some(mut focused_entity) = focused_entity {
		if !focused_entity.refocus(false, &widget_data) {
			if let Some(focus) = focused_entity.hovered_widget() {
				commands.trigger(CylInteract(focus));
			}
		}
	}
}

fn on_pointer_click(
	mut click: On<Pointer<Click>>,
	q_state: Query<(), With<CylClickable>>,
	mut commands: Commands,
) {
	if q_state.contains(click.entity) {
		click.propagate(false);
		commands.trigger(CylInteract(click.entity));
	}
}

fn on_pointer_over(
	over: On<Pointer<Over>>,
	widget_data: CylWidgetData,
	focused_entity: Option<ResMut<CylCurrentFocusedWidget>>,
	mut commands: Commands,
) {
	let entity = over.event_target();
	if let Some(mut focused_entity) = focused_entity {
		focused_entity.focus(entity, &widget_data);
	} else {
		let mut focus = CylCurrentFocusedWidget::new();
		focus.focus(entity, &widget_data);
		commands.insert_resource(focus);
	}
}

fn on_pointer_out(out: On<Pointer<Out>>, focused_entity: Option<ResMut<CylCurrentFocusedWidget>>) {
	let entity = out.event_target();
	// If the current entity is focused, unfocus it. (But keep previous focus.)
	if let Some(mut focused_entity) = focused_entity {
		focused_entity.unhover(entity);
	}
}
