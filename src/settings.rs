use bevy::{app::App, ecs::resource::Resource};

use crate::persistent::{register_saveable_resource, Saveable};

pub fn plugin(app: &mut App) {
	register_saveable_resource::<Settings>(app);
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
