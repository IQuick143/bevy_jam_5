//! The game's main screen states and transitions between them.

mod credits;
mod level_select;
mod loading;
mod playing;
mod settings;
mod splash;
mod title;

use crate::ui::prelude::*;
use bevy::{input::common_conditions::input_just_pressed, prelude::*};

#[allow(unused_imports)]
pub use playing::{GotoNextLevel, LoadLevel, PlayingLevel, PlayingLevelListEntry};

pub(super) fn plugin(app: &mut App) {
	app.init_state::<Screen>();
	app.add_fade_message::<DoScreenTransition>();

	app.add_plugins((
		splash::plugin,
		loading::plugin,
		title::plugin,
		credits::plugin,
		playing::plugin,
		level_select::plugin,
		settings::plugin,
	));

	app.add_systems(
		Update,
		(
			go_to_return_screen.run_if(input_just_pressed(KeyCode::Escape).and(ui_not_frozen)),
			do_screen_transitions,
		),
	);
}

/// Message that instantly causes a screen transition when sent.
/// Use with [`FadeAnimation`].
#[derive(Message, Component, Clone, Copy, PartialEq, Eq, Deref, DerefMut, Debug)]
pub struct DoScreenTransition(pub Screen);

/// Extension trait for [`Commands`] that adds a method
/// for triggering screen transitions
pub trait DoScreenTransitionCommands {
	/// Shorthand for triggering a screen transition accompanied by a screen fade
	fn do_screen_transition(&mut self, next_screen: Screen);
}

impl DoScreenTransitionCommands for Commands<'_, '_> {
	fn do_screen_transition(&mut self, next_screen: Screen) {
		self.spawn((
			FadeAnimationBundle::default(),
			DoScreenTransition(next_screen),
		));
	}
}

fn do_screen_transitions(
	mut events: MessageReader<DoScreenTransition>,
	mut next_screen: ResMut<NextState<Screen>>,
) {
	if let Some(screen) = events.read().last() {
		next_screen.set(screen.0);
	}
}

fn go_to_return_screen(current_screen: Res<State<Screen>>, mut commands: Commands) {
	if let Some(next) = current_screen.return_screen() {
		commands.do_screen_transition(next);
	}
}

/// The game's main screen states.
#[derive(States, Debug, Hash, PartialEq, Eq, Clone, Copy, Default, Reflect)]
pub enum Screen {
	#[default]
	Splash,
	Loading,
	Title,
	Settings,
	Credits,
	LevelSelect,
	/// The actual playing screen of the game.
	Playing,
}

impl Screen {
	/// Which screen should we return to
	fn return_screen(self) -> Option<Self> {
		match self {
			Self::Settings => Some(Self::Title),
			Self::Credits => Some(Self::Title),
			Self::LevelSelect => Some(Self::Title),
			Self::Playing => Some(Self::LevelSelect),
			_ => None,
		}
	}
}
