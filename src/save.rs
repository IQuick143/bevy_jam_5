//! Persistent state of the game's progression

use crate::persistent::{RegisterSaveableResource as _, Saveable};
use bevy::{app::App, ecs::resource::Resource, platform::collections::HashSet};

pub fn plugin(app: &mut App) {
	app.register_saveable_resource::<SaveGame>();
}

/// Persistent state of a game
#[derive(Resource, Default)]
pub struct SaveGame {
	completed_levels: HashSet<String>,
}

/// String identifier for the map storing the completed levels
const COMPLETED_LEVELS: &str = "completed_levels";

impl Saveable for SaveGame {
	const FILENAME: &str = "save";

	fn write_json(&self, store: &mut serde_json::Value) {
		use serde_json::{Map, Value};
		if !store.is_object() {
			*store = Value::Object(Map::new());
		}
		let store_dict = store.as_object_mut().unwrap();
		let arr = self
			.completed_levels
			.iter()
			.map(String::to_owned)
			.map(Value::String)
			.collect();
		store_dict.insert(COMPLETED_LEVELS.to_owned(), Value::Array(arr));
	}

	fn read_json(&mut self, store: &serde_json::Value) {
		let completed_levels = store
			.as_object()
			.and_then(|m| m.get(COMPLETED_LEVELS))
			.and_then(serde_json::Value::as_array)
			.into_iter()
			.flatten()
			.filter_map(serde_json::Value::as_str)
			.map(str::to_owned);
		self.completed_levels = HashSet::from_iter(completed_levels);
	}
}

impl SaveGame {
	pub fn is_level_completed(&self, level_id: &str) -> bool {
		self.completed_levels.contains(level_id)
	}

	pub fn set_level_completion(&mut self, level_id: &str, completion: bool) {
		if completion {
			self.completed_levels.insert(level_id.to_owned());
		} else {
			self.completed_levels.remove(level_id);
		}
	}
}
