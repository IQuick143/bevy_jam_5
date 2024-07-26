//! The screen state for the main game loop.

use bevy::{input::common_conditions::input_just_pressed, prelude::*};

use super::{QueueScreenTransition, Screen};

pub(super) fn plugin(app: &mut App) {
	app.add_systems(
		Update,
		return_to_title_screen
			.run_if(in_state(Screen::LevelSelect).and_then(input_just_pressed(KeyCode::Escape))),
	);
}

fn return_to_title_screen(mut next_screen: EventWriter<QueueScreenTransition>) {
	next_screen.send(QueueScreenTransition::instant(Screen::Title));
}
