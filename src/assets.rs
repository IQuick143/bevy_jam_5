use bevy::{prelude::*, utils::HashMap};

use crate::game::level::{
	asset::plugin as level_asset_plugin, list::LevelList,
	list_asset::plugin as level_list_asset_plugin, CycleTurnability, GlyphType, ObjectType,
	ThingType,
};

pub(super) fn plugin(app: &mut App) {
	app.register_type::<HandleMap<ImageKey>>();
	app.init_resource::<HandleMap<ImageKey>>();

	app.register_type::<HandleMap<SfxKey>>();
	app.init_resource::<HandleMap<SfxKey>>();

	app.register_type::<HandleMap<SoundtrackKey>>();
	app.init_resource::<HandleMap<SoundtrackKey>>();

	app.add_plugins(level_asset_plugin);
	app.add_plugins(level_list_asset_plugin);
	app.init_resource::<LoadedLevelList>();

	app.init_resource::<BoxColorSpriteAtlas>();
	app.init_resource::<DigitAtlas>();
	app.init_resource::<GlobalFont>();
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Reflect)]
pub enum ImageKey {
	Object(ThingType),
	CycleCenter(CycleTurnability),
	CycleRotationArrow,
	Background,
	Title,
	TitleBack,
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
			(
				ImageKey::TitleBack,
				asset_server.load("images/title-back.png"),
			),
		]
		.into()
	}
}

/// Texture atlas for the box sprites
#[derive(Resource, Reflect)]
#[reflect(Resource)]
pub struct BoxColorSpriteAtlas {
	/// The image with the full sprite sheet
	pub image: Handle<Image>,
	/// Atlas layout for the sprite sheet
	pub layout: Handle<TextureAtlasLayout>,
}

impl BoxColorSpriteAtlas {
	/// Path to the sprite sheet image
	const IMAGE_ASSET_PATH: &'static str = "images/box-sprites.png";

	/// Size of each digit's tile in pixels
	const TILE_SIZE: u32 = 136;
	/// Width of the gap between tiles and at the edges
	/// of the sheet, in pixels
	const PADDING_WIDTH: u32 = 15;
	/// How many sprites there are (rows and columns)
	const SPRITE_COUNTS: UVec2 = UVec2::splat(10);

	/// Constructs a [`TextureAtlasLayout`] for the sprite sheet
	fn construct_atlas_layout() -> TextureAtlasLayout {
		TextureAtlasLayout::from_grid(
			UVec2::splat(Self::TILE_SIZE),
			Self::SPRITE_COUNTS.x,
			Self::SPRITE_COUNTS.y,
			Some(UVec2::splat(Self::PADDING_WIDTH)),
			Some(UVec2::splat(Self::PADDING_WIDTH)),
		)
	}
}

impl FromWorld for BoxColorSpriteAtlas {
	fn from_world(world: &mut World) -> Self {
		let mut layouts = world.resource_mut::<Assets<TextureAtlasLayout>>();
		let layout = layouts.add(Self::construct_atlas_layout());
		let asset_server = world.resource::<AssetServer>();
		let image = asset_server.load(Self::IMAGE_ASSET_PATH);
		Self { image, layout }
	}
}

/// Sprite sheet for typesetting digits
#[derive(Resource, Reflect)]
#[reflect(Resource)]
pub struct DigitAtlas {
	/// The image with the full sprite sheet
	pub image: Handle<Image>,
	/// Atlas layout for the sprite sheet
	pub layout: Handle<TextureAtlasLayout>,
}

impl DigitAtlas {
	/// Path to the sprite sheet image
	const IMAGE_ASSET_PATH: &'static str = "images/digits.png";

	/// Size of each digit's tile in pixels
	const TILE_SIZE: u32 = 136;
	/// Width of the gap between tiles and at the edges
	/// of the sheet, in pixels
	const PADDING_WIDTH: u32 = 15;
	/// How many sprites there are
	const SPRITE_COUNT: u32 = 10;

	/// Constructs a [`TextureAtlasLayout`] for the sprite sheet
	fn construct_atlas_layout() -> TextureAtlasLayout {
		TextureAtlasLayout::from_grid(
			UVec2::splat(Self::TILE_SIZE),
			Self::SPRITE_COUNT,
			1,
			Some(UVec2::splat(Self::PADDING_WIDTH)),
			Some(UVec2::splat(Self::PADDING_WIDTH)),
		)
	}
}

impl FromWorld for DigitAtlas {
	fn from_world(world: &mut World) -> Self {
		let mut layouts = world.resource_mut::<Assets<TextureAtlasLayout>>();
		let layout = layouts.add(Self::construct_atlas_layout());
		let asset_server = world.resource::<AssetServer>();
		let image = asset_server.load(Self::IMAGE_ASSET_PATH);
		Self { image, layout }
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

/// Intermediate handle to a loading level list asset
/// The resource will be removed when the asset is loaded
#[derive(Resource, Debug)]
pub struct LoadedLevelList(pub Handle<LevelList>);

impl FromWorld for LoadedLevelList {
	fn from_world(world: &mut World) -> Self {
		let asset_server = world.resource::<AssetServer>();
		Self(asset_server.load("levels/levels.txt"))
	}
}

impl LoadedLevelList {
	pub fn all_loaded(&self, asset_server: &AssetServer) -> bool {
		asset_server.is_loaded_with_dependencies(&self.0)
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
