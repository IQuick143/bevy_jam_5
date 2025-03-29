use bevy::{app::App, ecs::resource::Resource, platform_support::collections::HashMap};

use crate::persistent::{register_saveable_resource, Saveable};

pub fn plugin(app: &mut App) {
	register_saveable_resource::<SaveGame>(app);
}

#[derive(Resource, Default)]
pub struct SaveGame {
	completed_levels: HashMap<String, bool>,
}

/// String identifier for the map storign the completed levels
const COMPLETED_LEVELS: &str = "completed_levels";

fn get_mut_or_create_map<'a>(
	map: &'a mut serde_json::Map<String, serde_json::Value>,
	key: &str,
) -> &'a mut serde_json::Map<String, serde_json::Value> {
	if !map.contains_key(key) {
		map.insert(
			key.to_string(),
			serde_json::Value::Object(serde_json::Map::new()),
		);
	}
	match map.get_mut(key) {
		Some(serde_json::Value::Object(dict)) => dict,
		Some(other) => {
			*other = serde_json::Value::Object(serde_json::Map::new());
			other.as_object_mut().unwrap()
		}
		None => unreachable!(),
	}
}

impl Saveable for SaveGame {
	const FILENAME: &str = "save";

	fn write_json(&self, store: &mut serde_json::Value) {
		use serde_json::Map;
		use serde_json::Value::*;
		if !store.is_object() {
			*store = Object(Map::new());
		}
		let Object(store_dict) = store else {
			unreachable!()
		};
		let completed_levels = get_mut_or_create_map(store_dict, COMPLETED_LEVELS);
		for (level, completion) in self.completed_levels.iter() {
			completed_levels.insert(level.to_string(), Bool(*completion));
		}
	}

	fn read_json(&mut self, store: &serde_json::Value) {
		use serde_json::Value::*;
		let Object(main_map) = store else {
			return;
		};
		if let Some(Object(completed_levels)) = main_map.get(COMPLETED_LEVELS) {
			for (level_name, value) in completed_levels.iter() {
				// If it's not a true bool, we pretend we didn't see that
				if let Bool(value) = value {
					self.completed_levels.insert(level_name.clone(), *value);
				}
			}
		}
	}
}
