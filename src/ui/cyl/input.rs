use super::prelude::*;
use crate::send_event;
use bevy::{input::common_conditions::input_just_pressed, prelude::*};

pub(super) fn plugin(app: &mut App) {
	app.add_observer(dispatch_input)
		.add_observer(on_pointer_click)
		.add_observer(on_pointer_out)
		.add_observer(on_pointer_over)
		.add_observer(|e: On<CylInteract>| info!("{:?}", *e))
		.add_systems(
			PostUpdate,
			(
				send_event(NavigationInput(NavigationDirection::Up))
					.run_if(input_just_pressed(KeyCode::ArrowUp)),
				send_event(NavigationInput(NavigationDirection::Down))
					.run_if(input_just_pressed(KeyCode::ArrowDown)),
				send_event(NavigationInput(NavigationDirection::Left))
					.run_if(input_just_pressed(KeyCode::ArrowLeft)),
				send_event(NavigationInput(NavigationDirection::Right))
					.run_if(input_just_pressed(KeyCode::ArrowRight)),
				send_event(InteractInput).run_if(input_just_pressed(KeyCode::Enter)),
			),
		);
}

/// Event that should be fired in order to perform navigation by keyboard (or controller).
#[derive(Event, Clone, Copy, Debug, Deref)]
pub struct NavigationInput(NavigationDirection);

/// Event that should be fired in order to perform an input by keyboard (or controller).
#[derive(Event, Clone, Copy, Debug, Default)]
pub struct InteractInput;

/// Marker component for widgets that take focus when hovered
#[derive(Component, Clone, Copy, Debug, Default)]
#[require(CylWidget)]
pub struct TakesFocusFromPointer;

fn dispatch_input(
	_: On<InteractInput>,
	focused_entity: Res<CylCurrentFocusedWidget>,
	mut commands: Commands,
) {
	if let Some(focus) = focused_entity.hovered_widget() {
		commands.trigger(CylInteract(focus));
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
	query: Query<(), With<TakesFocusFromPointer>>,
	mut commands: Commands,
) {
	let entity = over.event_target();
	if query.contains(over.event_target()) {
		commands.trigger(CylSetFocus {
			entity,
			reason: SetFocusReason::PointerOver,
		});
	}
}

fn on_pointer_out(
	out: On<Pointer<Out>>,
	focus: Res<CylCurrentFocusedWidget>,
	mut commands: Commands,
) {
	let entity = out.event_target();
	// If the current entity is focused, unfocus it. (But keep previous focus.)
	if focus.hovered_widget() == Some(entity) {
		commands.trigger(CylSuspendFocus);
	}
}
