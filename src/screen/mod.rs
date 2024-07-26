//! The game's main screen states and transitions between them.

mod credits;
mod loading;
mod playing;
mod splash;
mod title;

use bevy::prelude::*;

use crate::{game::LevelID, ui::screen_fade::Fader};

pub(super) fn plugin(app: &mut App) {
	app.init_state::<Screen>();
	app.add_event::<QueueScreenTransition>();
	app.init_resource::<PendingTransition>();
	app.enable_state_scoped_entities::<Screen>();

	app.add_plugins((
		splash::plugin,
		loading::plugin,
		title::plugin,
		credits::plugin,
		playing::plugin,
	));

	app.add_systems(Update, (transition, delete_objects_on_transition).chain());
}

fn transition(
	mut in_events: EventReader<QueueScreenTransition>,
	mut state: ResMut<NextState<Screen>>,
	mut fader: ResMut<Fader>,
	mut pending: ResMut<PendingTransition>,
) {
	if let Some(event) = in_events.read().last() {
		if !event.fade {
			state.set(event.next_screen);
			pending.next_screen = None;
		} else {
			fader.start_fade(crate::ui::screen_fade::FadeTarget::FadeOut);
			pending.next_screen = Some(event.next_screen);
		}
	}
	if fader.is_faded_out() {
		if let Some(next_state) = pending.next_screen {
			state.set(next_state);
			pending.next_screen = None;
		}
		fader.start_fade(crate::ui::screen_fade::FadeTarget::FadeIn);
	}
}

fn delete_objects_on_transition(
	mut commands: Commands,
	mut events: EventReader<StateTransitionEvent<Screen>>,
	objects: Query<Entity, With<DestroyOnTransition>>,
) {
	if events.read().last().is_some() {
		for object in objects.iter() {
			commands.entity(object).despawn_recursive();
		}
	}
}

#[derive(Event, PartialEq, Eq, Clone, Copy)]
pub struct QueueScreenTransition {
	pub next_screen: Screen,
	pub fade: bool,
}

#[allow(dead_code)]
impl QueueScreenTransition {
	fn new(next_screen: Screen, fade: bool) -> Self {
		QueueScreenTransition { next_screen, fade }
	}

	pub fn fade(next_screen: Screen) -> Self {
		QueueScreenTransition {
			next_screen,
			fade: true,
		}
	}

	fn instant(next_screen: Screen) -> Self {
		QueueScreenTransition {
			next_screen,
			fade: false,
		}
	}
}

#[derive(Resource, PartialEq, Eq, Clone, Copy, Default)]
struct PendingTransition {
	pub next_screen: Option<Screen>,
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
	Level(LevelID),
}

#[derive(Component, Clone, Copy, Default)]
pub struct DestroyOnTransition;
