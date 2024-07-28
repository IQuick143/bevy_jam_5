use bevy::{asset::AsyncReadExt, prelude::*, utils::HashMap};

use super::{level::ThingType, prelude::CycleTurnability, LevelID};

pub(super) fn plugin(app: &mut App) {
	app.register_type::<HandleMap<ImageKey>>();
	app.init_resource::<HandleMap<ImageKey>>();

	app.register_type::<HandleMap<SfxKey>>();
	app.init_resource::<HandleMap<SfxKey>>();

	app.register_type::<HandleMap<SoundtrackKey>>();
	app.init_resource::<HandleMap<SoundtrackKey>>();

	app.init_asset_loader::<PlainTextLoader>();
	app.init_asset::<PlainText>();
	app.init_resource::<HandleMap<LevelID>>();
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Reflect)]
pub enum ImageKey {
	Object(ThingType),
	CycleCenter(CycleTurnability),
	CycleRotationArrow,
}

impl AssetKey for ImageKey {
	type Asset = Image;
}

impl FromWorld for HandleMap<ImageKey> {
	fn from_world(world: &mut World) -> Self {
		let asset_server = world.resource::<AssetServer>();
		[
			(
				ImageKey::Object(ThingType::Object(super::level::ObjectType::Box)),
				asset_server.load("images/box.png"),
			),
			(
				ImageKey::Object(ThingType::Object(super::level::ObjectType::Player)),
				asset_server.load("images/player.png"),
			),
			(
				ImageKey::Object(ThingType::Glyph(super::level::GlyphType::Button)),
				asset_server.load("images/button.png"),
			),
			(
				ImageKey::Object(ThingType::Glyph(super::level::GlyphType::Flag)),
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
		]
		.into()
	}
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Reflect)]
pub enum SfxKey {
	ButtonHover,
	ButtonPress,
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
		]
		.into()
	}
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Reflect)]
pub enum SoundtrackKey {
	Credits,
	Gameplay,
}

impl AssetKey for SoundtrackKey {
	type Asset = AudioSource;
}

impl FromWorld for HandleMap<SoundtrackKey> {
	fn from_world(world: &mut World) -> Self {
		let asset_server = world.resource::<AssetServer>();
		[
			(
				SoundtrackKey::Credits,
				asset_server.load("audio/soundtracks/Monkeys Spinning Monkeys.ogg"),
			),
			(
				SoundtrackKey::Gameplay,
				asset_server.load("audio/soundtracks/Fluffing A Duck.ogg"),
			),
		]
		.into()
	}
}

#[derive(Asset, Clone, Deref, DerefMut, Debug, Reflect)]
pub struct PlainText(pub String);

#[derive(Default)]
struct PlainTextLoader;

impl bevy::asset::AssetLoader for PlainTextLoader {
	type Asset = PlainText;
	type Error = std::io::Error;
	type Settings = ();

	fn load<'a>(
		&'a self,
		reader: &'a mut bevy::asset::io::Reader,
		_settings: &'a Self::Settings,
		_load_context: &'a mut bevy::asset::LoadContext,
	) -> impl bevy::utils::ConditionalSendFuture<Output = Result<Self::Asset, Self::Error>> {
		async {
			let mut s = String::new();
			reader.read_to_string(&mut s).await?;
			Ok(PlainText(s))
		}
	}

	fn extensions(&self) -> &[&str] {
		&["txt"]
	}
}

impl AssetKey for LevelID {
	type Asset = PlainText;
}

impl FromWorld for HandleMap<LevelID> {
	#[rustfmt::skip]
	fn from_world(world: &mut World) -> Self {
		use LevelID::*;
		let asset_server = world.resource::<AssetServer>();
		[
			(Intro, asset_server.load("levels/tutorials/1_intro.txt")),
			(Transfer, asset_server.load("levels/tutorials/2_transfer.txt")),
			(Boxes, asset_server.load("levels/tutorials/3_boxes.txt")),
			(Manual, asset_server.load("levels/tutorials/4_manual.txt")),
			(Sync, asset_server.load("levels/tutorials/5_sync.txt")),
			(Swap, asset_server.load("levels/1_swap.txt")),
			(Sort, asset_server.load("levels/2_sort.txt")),
			(Bicycle, asset_server.load("levels/bicycle.txt")),
			(Tricycle, asset_server.load("levels/tricycle.txt")),
			(CargoTricycle, asset_server.load("levels/cargo.txt")),
			(CargoSinglePlayer, asset_server.load("levels/cargo-single.txt")),
			(Lotus, asset_server.load("levels/lotus.txt")),
			(ThreeInARowSimple, asset_server.load("levels/three-row-simple.txt")),
			(ThreeInARow, asset_server.load("levels/three-row.txt")),
			(Car, asset_server.load("levels/car.txt")),
			(Olympic, asset_server.load("levels/olympic.txt")),
			(Disrupt, asset_server.load("levels/linkage/disrupt.txt")),
			(Send, asset_server.load("levels/linkage/send.txt")),
			(Teamwork, asset_server.load("levels/teamwork.txt")),
			(Sort2, asset_server.load("levels/linkage/linked_sort.txt")),
		]
		.into()
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
