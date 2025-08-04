//! Settings that can be adjusted by the player

use crate::{game::camera::EnableParallax, ui::background::IsBackgroundEnabled};
use bevy::prelude::*;

pub(super) fn plugin(app: &mut App) {
	app.init_resource::<Settings>().add_systems(
		Update,
		update_settings_proxy_resources.run_if(resource_changed::<Settings>),
	);
}

/// Settings that can be adjusted by the player
#[derive(Resource, Debug, Default, Reflect)]
#[reflect(Resource)]
pub struct Settings {
	/// Volume of sound effects, as a fraction of maximum volume
	pub sfx_volume: f32,
	/// Volume of music, as a fraction of maximum volume
	pub soundtrack_volume: f32,
	/// Whether the background should be rendered
	pub render_background: bool,
	/// Whether parallax should be applied to background elements
	pub enable_parallax: bool,
}

/// Mirrors updates in [`Settings`] into resources that control
/// specific aspects of the app
fn update_settings_proxy_resources(
	settings: Res<Settings>,
	mut render_background: ResMut<IsBackgroundEnabled>,
	mut enable_parallax: ResMut<EnableParallax>,
) {
	render_background.set_if_neq(IsBackgroundEnabled(settings.render_background));
	enable_parallax.set_if_neq(EnableParallax(settings.enable_parallax));
}
