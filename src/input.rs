use crate::AppSet;
use bevy::ecs::schedule::ScheduleLabel;
use bevy::ecs::system::{RunSystemError, RunSystemOnce};
use bevy::input::keyboard::Key;
use bevy::prelude::*;

pub mod prelude {
	pub use super::{CurrentAction, InputAction, PressedActions, ProcessInputs};
}

pub fn plugin(app: &mut App) {
	app.init_schedule(ProcessInputs)
		.init_resource::<KeyBindings>()
		.init_resource::<CurrentAction>()
		.init_resource::<PressedActions>()
		.add_systems(Update, process_inputs.in_set(AppSet::ExecuteInput));
}

#[derive(ScheduleLabel, Clone, Debug, PartialEq, Eq, Hash, Default)]
pub struct ProcessInputs;

/// Resource describing the action that should be currently processed by the [`ProcessInputs`] schedule.
#[derive(Resource, Default, Clone, Copy, PartialEq, Eq)]
pub struct CurrentAction(pub Option<InputAction>);

/// Resource describing the actions that are currently held.
#[derive(Resource, Default, Clone, PartialEq, Eq)]
pub struct PressedActions(pub Vec<InputAction>);

// TODO: Impl `Display`
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum InputAction {
	Click,
	/// Zoom camera
	Zoom(i8),
	/// Turn a cycle, amount is clamped to +-1
	Turn(i8),
	/// Directional input, navigate menus (TODO) or move camera
	Direction(Direction),
	Confirm,
	/// Go back, return to previous screen, close (cancel) a menu
	GoBack,
	Undo,
	Redo,
	Reset,
	NextLevel,
	/// Switch from one player cycle to the next adjacent one (TODO)
	Swap,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum Direction {
	Up,
	Down,
	Left,
	Right,
}

// TODO: Impl format
#[derive(Clone)]
pub enum InputKey {
	Mouse(MouseButton),
	Keyboard(Key),
}

pub struct Binding {
	#[expect(unused, reason = "This will be relevant when we do rebinding.")]
	/// Whether the first binding is hardcoded
	hard_coded_bind: bool,
	action: InputAction,
	inputs: Vec<InputKey>,
}

#[derive(Resource)]
pub struct KeyBindings {
	bindings: Vec<Binding>,
}

impl KeyBindings {
	fn get_actions(
		&self,
		keyboard: &ButtonInput<Key>,
		mouse: &ButtonInput<MouseButton>,
	) -> Vec<InputAction> {
		let mut actions = Vec::new();
		for binding in self.bindings.iter() {
			for input in binding.inputs.iter() {
				if match input {
					InputKey::Mouse(mouse_key) => mouse.pressed(*mouse_key),
					InputKey::Keyboard(key) => match key {
						Key::Character(character) => {
							keyboard.pressed(Key::Character(character.to_lowercase().into()))
								|| keyboard.pressed(Key::Character(character.to_uppercase().into()))
						}
						key => keyboard.pressed(key.clone()),
					},
				} {
					actions.push(binding.action);
				}
			}
		}
		actions
	}
}

impl Default for KeyBindings {
	fn default() -> Self {
		#[inline]
		fn bind(action: InputAction, binds: &[InputKey]) -> Binding {
			Binding {
				hard_coded_bind: false,
				action,
				inputs: binds.to_vec(),
			}
		}
		#[inline]
		fn bind_hard(action: InputAction, binds: &[InputKey]) -> Binding {
			Binding {
				hard_coded_bind: true,
				action,
				inputs: binds.to_vec(),
			}
		}
		let a_key: Key = Key::Character("a".into());
		let d_key: Key = Key::Character("d".into());
		let n_key: Key = Key::Character("n".into());
		let p_key: Key = Key::Character("p".into());
		let r_key: Key = Key::Character("r".into());
		let y_key: Key = Key::Character("y".into());
		let z_key: Key = Key::Character("z".into());
		let plus_key: Key = Key::Character("+".into());
		let minus_key: Key = Key::Character("-".into());
		const U: InputAction = InputAction::Direction(super::input::Direction::Up);
		const D: InputAction = InputAction::Direction(super::input::Direction::Down);
		const L: InputAction = InputAction::Direction(super::input::Direction::Left);
		const R: InputAction = InputAction::Direction(super::input::Direction::Right);
		use InputAction::*;
		use InputKey::*;
		#[rustfmt::skip]
		return KeyBindings {
			bindings: vec![
				bind_hard(Click,	&[Mouse(MouseButton::Left)]),
				bind(Turn(-1),		&[Keyboard(a_key), Mouse(MouseButton::Right)]),
				bind(Turn( 1),		&[Keyboard(d_key), Mouse(MouseButton::Left)]),
				bind_hard(U,		&[Keyboard(Key::ArrowUp)]),
				bind_hard(D,		&[Keyboard(Key::ArrowDown)]),
				bind_hard(L,		&[Keyboard(Key::ArrowLeft)]),
				bind_hard(R,		&[Keyboard(Key::ArrowRight)]),
				bind_hard(Confirm,	&[Keyboard(Key::Enter)]),
				bind_hard(GoBack,	&[Keyboard(Key::Escape), Mouse(MouseButton::Back)]),
				bind(Zoom(-1),		&[Keyboard(minus_key)]),
				bind(Zoom( 1),		&[Keyboard(plus_key)]),
				bind(Swap,			&[Keyboard(p_key)]),
				bind(Undo,			&[Keyboard(z_key)]),
				bind(Redo,			&[Keyboard(y_key)]),
				bind(Reset,			&[Keyboard(r_key)]),
				bind(NextLevel,		&[Keyboard(n_key), Mouse(MouseButton::Forward)]),
			]
		};
	}
}

fn parse_inputs(
	mouse: Res<ButtonInput<MouseButton>>,
	keyboard: Res<ButtonInput<Key>>,
	// controller: Res<todo!()>,
	bindings: Res<KeyBindings>,
) -> Vec<InputAction> {
	#[cfg(feature = "dev")]
	for input in keyboard.get_just_pressed() {
		info!("Pressed: {input:?}");
	}
	#[cfg(feature = "dev")]
	for input in keyboard.get_just_released() {
		info!("Released: {input:?}");
	}
	bindings.get_actions(&keyboard, &mouse)
}

fn process_inputs(world: &mut World) -> Result<(), RunSystemError> {
	let actions: Vec<InputAction> = world.run_system_once(parse_inputs)?;
	let just_pressed_actions = world.resource_scope(
		|_world: &mut World, mut active_actions: Mut<PressedActions>| {
			let new_actions: Vec<InputAction> = actions
				.iter()
				.filter(|x| !active_actions.0.contains(x))
				.copied()
				.collect();
			active_actions.0 = actions;
			new_actions
		},
	);
	for action in just_pressed_actions.into_iter() {
		world.insert_resource(CurrentAction(Some(action)));
		world.run_schedule(ProcessInputs);
	}
	world.insert_resource(CurrentAction(None));
	Ok(())
}
