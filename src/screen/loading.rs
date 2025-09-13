//! A loading screen during which game assets are loaded.
//! This reduces stuttering, especially for audio on WASM.

use bevy::prelude::*;

use super::Screen;
use crate::{
	assets::{self, *},
	audio,
	ui::prelude::*,
};

pub(super) fn plugin(app: &mut App) {
	app.add_systems(OnEnter(Screen::Loading), enter_loading);
	app.add_systems(
		Update,
		continue_to_title.run_if(in_state(Screen::Loading).and(all_assets_loaded)),
	);
}

fn enter_loading(mut commands: Commands, font: Res<GlobalFont>) {
	commands
		.ui_root()
		.insert(DespawnOnExit(Screen::Loading))
		.with_children(|children| {
			children.label("Loading...", font.0.clone_weak());
		});
}

fn all_assets_loaded(
	asset_server: Res<AssetServer>,
	image_handles: Res<HandleMap<ImageKey>>,
	sfx_handles: Res<HandleMap<SfxKey>>,
	soundtrack_handles: Res<HandleMap<SoundtrackKey>>,
	font: Res<GlobalFont>,
	// This resource gets initialized later, so it needs to be optional
	level_list: Option<Res<LoadedLevelList>>,
) -> bool {
	image_handles.all_loaded(&asset_server)
		&& sfx_handles.all_loaded(&asset_server)
		&& soundtrack_handles.all_loaded(&asset_server)
		&& asset_server.is_loaded_with_dependencies(font.0.id())
		&& level_list.is_some_and(|list| list.all_loaded(&asset_server))
}

fn continue_to_title(mut commands: Commands, mut next_screen: ResMut<NextState<Screen>>) {
	next_screen.set(Screen::Title);
	// Start playing soundtrack as soon as we proceed to title
	commands.trigger(audio::soundtrack::PlaySoundtrack::Key(
		assets::SoundtrackKey::Gameplay,
	));
}
