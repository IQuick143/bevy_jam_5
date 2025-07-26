//! Settings relating to the game interface

use crate::persistent::{RegisterSaveableResource as _, Saveable};
use bevy::{app::App, ecs::resource::Resource};

pub fn plugin(app: &mut App) {
	app.register_saveable_resource::<Settings>();
}

#[derive(Resource, Default)]
pub struct Settings {}

impl Saveable for Settings {
	const FILENAME: &'static str = "settings";

	fn write_json(&self, _store: &mut serde_json::Value) {
		// TODO
	}

	fn read_json(&mut self, _store: &serde_json::Value) {
		// TODO
	}
}
