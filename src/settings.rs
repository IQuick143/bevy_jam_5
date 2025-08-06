//! Settings that can be adjusted by the player

use crate::{
	camera::parallax::EnableParallax,
	persistent::{RegisterSaveableResource, Saveable},
	ui::background::BackgroundMode,
};
use bevy::prelude::*;

pub(super) fn plugin(app: &mut App) {
	app.register_saveable_resource::<Settings>().add_systems(
		Update,
		update_settings_proxy_resources.run_if(resource_changed::<Settings>),
	);
}

/// Settings that can be adjusted by the player
#[derive(Resource, Debug, Reflect)]
#[reflect(Resource)]
pub struct Settings {
	/// Volume of sound effects, as a fraction of maximum volume
	pub sfx_volume: f32,
	/// Volume of music, as a fraction of maximum volume
	pub soundtrack_volume: f32,
	/// How the background should be rendered
	pub background_mode: BackgroundMode,
	/// Whether parallax should be applied to background elements
	pub enable_parallax: bool,
}

impl Default for Settings {
	fn default() -> Self {
		Self {
			sfx_volume: 0.5,
			soundtrack_volume: 0.25,
			background_mode: BackgroundMode::Animated,
			enable_parallax: true,
		}
	}
}

/// String identifier for the sfx_volume
const SFX_VOLUME: &str = "sfx_volume";
const SOUNDTRACK_VOLUME: &str = "soundtrack_volume";
const BACKGROUND_MODE: &str = "background_mode";
const ENABLE_PARALLAX: &str = "enable_parallax";

impl Saveable for Settings {
	const FILENAME: &str = "settings";

	fn write_json(&self, store: &mut serde_json::Value) {
		use serde_json::{Map, Value};
		if !store.is_object() {
			*store = Value::Object(Map::new());
		}
		let store_dict = store.as_object_mut().unwrap();
		store_dict.write(SFX_VOLUME, self.sfx_volume);
		store_dict.write(SOUNDTRACK_VOLUME, self.soundtrack_volume);
		store_dict.write(BACKGROUND_MODE, self.background_mode);
		store_dict.write(ENABLE_PARALLAX, self.enable_parallax);
	}

	fn read_json(&mut self, store: &serde_json::Value) {
		if let Some(m) = store.as_object() {
			m.read_percentage(SFX_VOLUME, &mut self.sfx_volume);
			m.read_percentage(SOUNDTRACK_VOLUME, &mut self.soundtrack_volume);
			m.read_from(BACKGROUND_MODE, &mut self.background_mode);
			m.read_bool(ENABLE_PARALLAX, &mut self.enable_parallax);
		}
	}
}

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
			#[allow(clippy::manual_clamp)] // standard clamp does not take care of NaN
			{
				*destination = value.max(0.0).min(1.0) as f32;
			}
		}
	}
	/// Reads a value that can be fallibly converted from a [`serde_json::Value`]
	fn read_from<T>(&self, key: &str, destination: &mut T)
	where
		for<'a> &'a serde_json::Value: TryInto<T>;
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
}

impl From<BackgroundMode> for serde_json::Value {
	fn from(value: BackgroundMode) -> Self {
		match value {
			BackgroundMode::None => 0.into(),
			BackgroundMode::Static => 1.into(),
			BackgroundMode::Animated => 2.into(),
		}
	}
}

impl TryFrom<&serde_json::Value> for BackgroundMode {
	type Error = ();
	fn try_from(value: &serde_json::Value) -> std::result::Result<Self, Self::Error> {
		match value.as_u64() {
			Some(0) => Ok(BackgroundMode::None),
			Some(1) => Ok(BackgroundMode::Static),
			Some(2) => Ok(BackgroundMode::Animated),
			_ => Err(()),
		}
	}
}

/// Mirrors updates in [`Settings`] into resources that control
/// specific aspects of the app
fn update_settings_proxy_resources(
	settings: Res<Settings>,
	mut enable_parallax: ResMut<EnableParallax>,
	mut commands: Commands,
) {
	enable_parallax.set_if_neq(EnableParallax(settings.enable_parallax));
	commands.trigger(settings.background_mode);
}
