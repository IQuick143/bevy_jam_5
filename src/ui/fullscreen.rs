//! Full screen view handling

use crate::input::prelude::*;
use bevy::{prelude::*, window::WindowMode};

pub(super) fn plugin(app: &mut App) {
	app.init_resource::<IsFullScreen>()
		.add_systems(
			Update,
			apply_full_screen.run_if(resource_changed::<IsFullScreen>),
		)
		.add_systems(
			ProcessInputs,
			(|mut f: ResMut<IsFullScreen>| f.toggle()).run_if(resource_equals(CurrentAction(
				Some(InputAction::FullScreen),
			))),
		);
}

#[derive(Resource, Clone, Copy, PartialEq, Eq, Default, Deref, DerefMut)]
pub struct IsFullScreen(pub bool);

impl IsFullScreen {
	pub fn toggle(&mut self) {
		**self = !**self;
	}
}

fn apply_full_screen(mut window: Single<&mut Window>, is_fullscreen: Res<IsFullScreen>) {
	if **is_fullscreen {
		window.mode = WindowMode::BorderlessFullscreen(MonitorSelection::Current);
	} else {
		window.mode = WindowMode::Windowed;
	}
}
