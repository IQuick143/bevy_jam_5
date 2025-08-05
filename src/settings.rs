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
	/// Whether the background should be rendered
	pub render_background: bool,
	/// Whether the background should be animated
	pub animate_background: bool,
	/// Whether parallax should be applied to background elements
	pub enable_parallax: bool,
}

impl Default for Settings {
	fn default() -> Self {
		Self {
			sfx_volume: 0.5,
			soundtrack_volume: 0.25,
			render_background: true,
			animate_background: true,
			enable_parallax: true,
		}
	}
}

/// String identifier for the sfx_volume
const SFX_VOLUME: &str = "sfx_volume";
const SOUNDTRACK_VOLUME: &str = "soundtrack_volume";
const RENDER_BACKGROUND: &str = "render_background";
const ANIMATE_BACKGROUND: &str = "animate_background";
const ENABLE_PARALLAX: &str = "enable_parallax";

impl Saveable for Settings {
	const FILENAME: &str = "settings";

	fn write_json(&self, store: &mut serde_json::Value) {
		use serde_json::{Map, Value};
		if !store.is_object() {
			*store = Value::Object(Map::new());
		}
		let store_dict = store.as_object_mut().unwrap();
		store_dict.insert(SFX_VOLUME.to_owned(), self.sfx_volume.into());
		store_dict.insert(SOUNDTRACK_VOLUME.to_owned(), self.soundtrack_volume.into());
		store_dict.insert(RENDER_BACKGROUND.to_owned(), self.render_background.into());
		store_dict.insert(
			ANIMATE_BACKGROUND.to_owned(),
			self.animate_background.into(),
		);
		store_dict.insert(ENABLE_PARALLAX.to_owned(), self.enable_parallax.into());
	}

	fn read_json(&mut self, store: &serde_json::Value) {
		if let Some(m) = store.as_object() {
			if let Some(x) = m.get_float(SFX_VOLUME) {
				// `f32::clamp` to both ensure that the result is in a valid range,
				// but also to deal with NaN and +-inf
				self.sfx_volume = x.clamp(0.0, 1.0) as f32;
			}
			if let Some(x) = m.get_float(SOUNDTRACK_VOLUME) {
				// `f32::clamp` to both ensure that the result is in a valid range,
				// but also to deal with NaN and +-inf
				self.soundtrack_volume = x.clamp(0.0, 1.0) as f32;
			}
			if let Some(x) = m.get_bool(RENDER_BACKGROUND) {
				self.render_background = x;
			}
			if let Some(x) = m.get_bool(ANIMATE_BACKGROUND) {
				self.animate_background = x;
			}
			if let Some(x) = m.get_bool(ENABLE_PARALLAX) {
				self.enable_parallax = x;
			}
		}
	}
}

pub trait MapExt {
	fn get_bool(&self, key: &str) -> Option<bool>;
	fn get_float(&self, key: &str) -> Option<f64>;
}

impl MapExt for serde_json::Map<String, serde_json::Value> {
	fn get_bool(&self, key: &str) -> Option<bool> {
		self.get(key).and_then(serde_json::Value::as_bool)
	}

	fn get_float(&self, key: &str) -> Option<f64> {
		self.get(key).and_then(serde_json::Value::as_f64)
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
	if !settings.render_background {
		commands.trigger(BackgroundMode::None);
	} else if !settings.animate_background {
		commands.trigger(BackgroundMode::Static);
	} else {
		commands.trigger(BackgroundMode::Animated);
	}
}
