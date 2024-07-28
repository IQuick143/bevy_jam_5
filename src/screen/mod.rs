//! The game's main screen states and transitions between them.

mod credits;
mod level_select;
mod loading;
mod playing;
mod splash;
mod title;

use bevy::{input::common_conditions::input_just_pressed, prelude::*};

use crate::ui::{
	freeze::{ui_not_frozen, FreezeUi},
	screen_fade::Fader,
};

pub use playing::PlayingLevel;

pub(super) fn plugin(app: &mut App) {
	app.init_state::<Screen>();
	app.add_event::<QueueScreenTransition<Screen>>();
	app.init_resource::<PendingTransition<Screen>>();
	app.enable_state_scoped_entities::<Screen>();

	app.add_plugins((
		splash::plugin,
		loading::plugin,
		title::plugin,
		credits::plugin,
		playing::plugin,
		level_select::plugin,
	));

	app.add_systems(Update, process_enqueued_transitions::<Screen>);
	app.add_systems(
		Update,
		go_to_return_screen.run_if(input_just_pressed(KeyCode::Escape).and_then(ui_not_frozen)),
	);
}

fn go_to_return_screen(
	current_screen: Res<State<Screen>>,
	mut next_screen: EventWriter<QueueScreenTransition<Screen>>,
) {
	if let Some(next) = current_screen.return_screen() {
		next_screen.send(QueueScreenTransition::fade(next));
	}
}

fn process_enqueued_transitions<S: bevy::state::state::FreelyMutableState + Clone>(
	mut in_events: EventReader<QueueScreenTransition<S>>,
	mut state: ResMut<NextState<S>>,
	mut fader: ResMut<Fader>,
	mut pending: ResMut<PendingTransition<S>>,
	mut freeze: ResMut<FreezeUi>,
) {
	if let Some(event) = in_events.read().last() {
		if !event.fade {
			state.set(event.next_screen.clone());
			pending.next_screen = None;
		} else {
			fader.start_fade(crate::ui::screen_fade::FadeTarget::FadeOut);
			pending.next_screen = Some(event.next_screen.clone());
			freeze.0 = true;
		}
	}
	if fader.is_faded_out() {
		if let Some(next_state) = pending.next_screen.clone() {
			state.set(next_state);
			pending.next_screen = None;
			fader.start_fade(crate::ui::screen_fade::FadeTarget::FadeIn);
			freeze.0 = false;
		}
		// If there is no transition enqueued, stay faded out
		// It most likely means a different specialization of this system
		// has enqueued a transition
	}
}

#[derive(Event, PartialEq, Eq, Clone, Copy)]
pub struct QueueScreenTransition<S: States> {
	pub next_screen: S,
	pub fade: bool,
}

#[allow(dead_code)]
impl<S: States> QueueScreenTransition<S> {
	fn new(next_screen: S, fade: bool) -> Self {
		Self { next_screen, fade }
	}

	pub fn fade(next_screen: S) -> Self {
		Self {
			next_screen,
			fade: true,
		}
	}

	pub fn instant(next_screen: S) -> Self {
		Self {
			next_screen,
			fade: false,
		}
	}
}

#[derive(Resource, PartialEq, Eq, Clone, Copy, Default)]
struct PendingTransition<S: States> {
	pub next_screen: Option<S>,
}

/// The game's main screen states.
/// # To change a screenstate, `send` a [`QueueScreenTransition`] instead
#[derive(States, Debug, Hash, PartialEq, Eq, Clone, Copy, Default)]
pub enum Screen {
	#[default]
	Splash,
	Loading,
	Title,
	Credits,
	LevelSelect,
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
