//! The game's main screen states and transitions between them.

mod credits;
mod level_select;
mod loading;
mod playing;
mod splash;
mod title;

use crate::ui::prelude::*;
use bevy::{input::common_conditions::input_just_pressed, prelude::*};

pub use playing::LoadLevel;
pub use playing::PlayingLevel;

pub(super) fn plugin(app: &mut App) {
	app.init_state::<Screen>();
	app.add_fade_event::<DoScreenTransition>();
	app.enable_state_scoped_entities::<Screen>();

	app.add_plugins((
		splash::plugin,
		loading::plugin,
		title::plugin,
		credits::plugin,
		playing::plugin,
		level_select::plugin,
	));

	app.add_systems(
		Update,
		(
			go_to_return_screen.run_if(input_just_pressed(KeyCode::Escape).and(ui_not_frozen)),
			do_screen_transitions,
		),
	);
}

/// Event that instantly causes a screen transition when sent.
/// Use with [`FadeAnimation`].
#[derive(Event, Clone, Copy, PartialEq, Eq, Deref, DerefMut, Debug)]
pub struct DoScreenTransition(pub Screen);

fn do_screen_transitions(
	mut events: EventReader<DoScreenTransition>,
	mut next_screen: ResMut<NextState<Screen>>,
) {
	if let Some(screen) = events.read().last() {
		next_screen.set(screen.0);
	}
}

fn go_to_return_screen(current_screen: Res<State<Screen>>, mut commands: Commands) {
	if let Some(next) = current_screen.return_screen() {
		commands.spawn((FadeAnimationBundle::default(), DoScreenTransition(next)));
	}
}

/// The game's main screen states.
#[derive(States, Debug, Hash, PartialEq, Eq, Clone, Copy, Default)]
pub enum Screen {
	#[default]
	Splash,
	Loading,
	Title,
	Credits,
	LevelSelect,
	/// The actual playing screen of the game.
	Playing,
}

impl Screen {
	/// Which screen should we return to
	fn return_screen(self) -> Option<Self> {
		match self {
			Self::Credits => Some(Self::Title),
			Self::LevelSelect => Some(Self::Title),
			Self::Playing => Some(Self::LevelSelect),
			_ => None,
		}
	}
}
