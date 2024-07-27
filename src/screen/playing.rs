//! The screen state for the main game loop.

use bevy::{input::common_conditions::input_just_pressed, prelude::*};

use super::{QueueScreenTransition, Screen};

pub(super) fn plugin(app: &mut App) {
	app.add_systems(
		Update,
		return_to_level_select_screen
			.run_if(is_on_level_screen.and_then(input_just_pressed(KeyCode::Escape))),
	);
}

fn is_on_level_screen(s: Option<Res<State<Screen>>>) -> bool {
	s.is_some_and(|s| matches!(s.get(), Screen::Level(_)))
}

fn return_to_level_select_screen(mut next_screen: EventWriter<QueueScreenTransition>) {
	next_screen.send(QueueScreenTransition::fade(Screen::LevelSelect));
}
