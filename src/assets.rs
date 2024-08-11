use bevy::{prelude::*, utils::HashMap};

use crate::game::{
	components::CycleTurnability,
	level::{
		asset::{plugin as level_asset_plugin, LevelAsset},
		GlyphType, ObjectType, ThingType,
	},
};

pub(super) fn plugin(app: &mut App) {
	app.register_type::<HandleMap<ImageKey>>();
	app.init_resource::<HandleMap<ImageKey>>();

	app.register_type::<HandleMap<SfxKey>>();
	app.init_resource::<HandleMap<SfxKey>>();

	app.register_type::<HandleMap<SoundtrackKey>>();
	app.init_resource::<HandleMap<SoundtrackKey>>();

	app.add_plugins(level_asset_plugin);
	app.init_resource::<LevelList>();

	app.init_resource::<GlobalFont>();
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Reflect)]
pub enum ImageKey {
	Object(ThingType),
	CycleCenter(CycleTurnability),
	CycleRotationArrow,
	Background,
	Title,
}

impl AssetKey for ImageKey {
	type Asset = Image;
}

impl FromWorld for HandleMap<ImageKey> {
	fn from_world(world: &mut World) -> Self {
		let asset_server = world.resource::<AssetServer>();
		[
			(
				ImageKey::Object(ThingType::Object(ObjectType::Box)),
				asset_server.load("images/box.png"),
			),
			(
				ImageKey::Object(ThingType::Object(ObjectType::Player)),
				asset_server.load("images/player.png"),
			),
			(
				ImageKey::Object(ThingType::Glyph(GlyphType::Button)),
				asset_server.load("images/button.png"),
			),
			(
				ImageKey::Object(ThingType::Glyph(GlyphType::Flag)),
				asset_server.load("images/flag.png"),
			),
			(
				ImageKey::CycleCenter(CycleTurnability::Always),
				asset_server.load("images/cycle-engine.png"),
			),
			(
				ImageKey::CycleCenter(CycleTurnability::WithPlayer),
				asset_server.load("images/cycle-player.png"),
			),
			(
				ImageKey::CycleCenter(CycleTurnability::Never),
				asset_server.load("images/cycle-still.png"),
			),
			(
				ImageKey::CycleRotationArrow,
				asset_server.load("images/cycle-rot.png"),
			),
			(
				ImageKey::Background,
				asset_server.load("images/background.png"),
			),
			(ImageKey::Title, asset_server.load("images/title.png")),
		]
		.into()
	}
}

/// How much bigger is the title image than its viewport
pub const TITLE_IMAGE_OVERFLOW: f32 = 1.2;

#[derive(Copy, Clone, Eq, PartialEq, Hash, Reflect)]
pub enum SfxKey {
	ButtonHover,
	ButtonPress,
	Victory,
	EnterLevel,
}

impl SfxKey {
	pub fn volume_multiplier(self) -> f32 {
		match self {
			// These sounds are just too loud by default
			Self::ButtonHover | Self::ButtonPress => 0.3,
			// This one cannot be quieter than GoalComplete
			Self::Victory => 3.0,
			Self::EnterLevel => 1.5,
			//_ => 1.0,
		}
	}
}

impl AssetKey for SfxKey {
	type Asset = AudioSource;
}

impl FromWorld for HandleMap<SfxKey> {
	fn from_world(world: &mut World) -> Self {
		let asset_server = world.resource::<AssetServer>();
		[
			(
				SfxKey::ButtonHover,
				asset_server.load("audio/sfx/button_hover.ogg"),
			),
			(
				SfxKey::ButtonPress,
				asset_server.load("audio/sfx/button_press.ogg"),
			),
			(
				SfxKey::Victory,
				asset_server.load("audio/sfx/level_complete.ogg"),
			),
			(
				SfxKey::EnterLevel,
				asset_server.load("audio/sfx/transition.ogg"),
			),
		]
		.into()
	}
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Reflect)]
pub enum SoundtrackKey {
	Gameplay,
}

impl AssetKey for SoundtrackKey {
	type Asset = AudioSource;
}

impl FromWorld for HandleMap<SoundtrackKey> {
	fn from_world(world: &mut World) -> Self {
		let asset_server = world.resource::<AssetServer>();
		[(
			SoundtrackKey::Gameplay,
			asset_server.load("audio/soundtracks/blorbitality.ogg"),
		)]
		.into()
	}
}

#[derive(Resource, Reflect, Deref, DerefMut)]
#[reflect(Resource)]
pub struct LevelList(Vec<Handle<LevelAsset>>);

impl FromWorld for LevelList {
	#[rustfmt::skip]
	fn from_world(world: &mut World) -> Self {
		let asset_server = world.resource::<AssetServer>();
		LevelList(vec![
			asset_server.load("levels/tutorials/1_intro.txt"),
			asset_server.load("levels/tutorials/2_transfer.txt"),
			asset_server.load("levels/tutorials/3_boxes.txt"),
			asset_server.load("levels/tutorials/4_manual.txt"),
			asset_server.load("levels/tutorials/5_sync.txt"),
			asset_server.load("levels/tutorials/6_sync2.txt"),
			asset_server.load("levels/tutorials/7_colors.txt"),
			asset_server.load("levels/1_swap.txt"),
			asset_server.load("levels/2_sort.txt"),
			asset_server.load("levels/bicycle.txt"),
			asset_server.load("levels/tricycle.txt"),
			asset_server.load("levels/cargo.txt"),
			asset_server.load("levels/cargo-single.txt"),
			asset_server.load("levels/lotus.txt"),
			asset_server.load("levels/three-row-simple.txt"),
			asset_server.load("levels/three-row.txt"),
			asset_server.load("levels/car.txt"),
			asset_server.load("levels/olympic.txt"),
			asset_server.load("levels/linkage/disrupt.txt"),
			asset_server.load("levels/linkage/send.txt"),
			asset_server.load("levels/teamwork.txt"),
			asset_server.load("levels/linkage/linked_sort.txt"),
			asset_server.load("levels/rubik.txt"),
		])
	}
}

/// The font to be used for rendering all text
#[derive(Resource, Debug)]
pub struct GlobalFont(pub Handle<Font>);

impl FromWorld for GlobalFont {
	fn from_world(world: &mut World) -> Self {
		let asset_server = world.resource::<AssetServer>();
		Self(asset_server.load("fonts/Comfortaa-SemiBold.ttf"))
	}
}

pub trait AssetKey: Sized {
	type Asset: Asset;
}

#[derive(Resource, Reflect, Deref, DerefMut)]
#[reflect(Resource)]
pub struct HandleMap<K: AssetKey>(HashMap<K, Handle<K::Asset>>);

impl<K: AssetKey, T> From<T> for HandleMap<K>
where
	T: Into<HashMap<K, Handle<K::Asset>>>,
{
	fn from(value: T) -> Self {
		Self(value.into())
	}
}

impl<K: AssetKey> HandleMap<K> {
	pub fn all_loaded(&self, asset_server: &AssetServer) -> bool {
		self.values()
			.all(|x| asset_server.is_loaded_with_dependencies(x))
	}
}