//! Extension for more concise parsing of jsons

use serde_json::{Map, Value};

pub trait MapExt {
	fn get_bool(&self, key: &str) -> Option<bool>;
	fn get_float(&self, key: &str) -> Option<f64>;
	fn write(&mut self, key: &str, value: impl Into<serde_json::Value>);
	/// Reads a bool from the dictionary, if it's set it overwrites the destination
	fn read_bool(&self, key: &str, destination: &mut bool) {
		if let Some(value) = self.get_bool(key) {
			*destination = value;
		}
	}
	/// Reads a float from the dictionary and clamps it to [0-1], if it's set it overwrites the destination
	fn read_percentage(&self, key: &str, destination: &mut f32) {
		if let Some(value) = self.get_float(key) {
			// Clamp to both ensure that the result is in a valid range,
			// but also to deal with NaN and +-inf
			#[allow(
				clippy::manual_clamp,
				reason = "standard clamp does not take care of NaN"
			)]
			{
				*destination = value.max(0.0).min(1.0) as f32;
			}
		}
	}
	/// Reads a value that can be fallibly converted from a [`serde_json::Value`]
	fn read_from<T>(&self, key: &str, destination: &mut T)
	where
		for<'a> &'a serde_json::Value: TryInto<T>;
	/// If the provided key does not contain an object,
	/// places an object there and returns it
	fn put_object_at(&mut self, key: &str) -> &mut Map<String, Value>;
}

impl MapExt for serde_json::Map<String, serde_json::Value> {
	fn write(&mut self, key: &str, value: impl Into<serde_json::Value>) {
		self.insert(key.to_owned(), value.into());
	}

	fn get_bool(&self, key: &str) -> Option<bool> {
		self.get(key).and_then(serde_json::Value::as_bool)
	}

	fn get_float(&self, key: &str) -> Option<f64> {
		self.get(key).and_then(serde_json::Value::as_f64)
	}

	fn read_from<T>(&self, key: &str, destination: &mut T)
	where
		for<'a> &'a serde_json::Value: TryInto<T>,
	{
		if let Some(value) = self.get(key) {
			if let Ok(value) = value.try_into() {
				*destination = value;
			}
		}
	}

	fn put_object_at(&mut self, key: &str) -> &mut Map<String, Value> {
		if !self.get(key).is_some_and(Value::is_object) {
			self.insert(key.to_owned(), Map::new().into());
		}
		self[key].as_object_mut().unwrap()
	}
}
