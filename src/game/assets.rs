use bevy::{asset::AsyncReadExt, prelude::*, utils::HashMap};

use super::{level::ThingType, prelude::CycleTurnability};

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
		]
		.into()
	}
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Reflect)]
pub enum SfxKey {
	ButtonHover,
	ButtonPress,
	Step1,
	Step2,
	Step3,
	Step4,
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
			(SfxKey::Step1, asset_server.load("audio/sfx/step1.ogg")),
			(SfxKey::Step2, asset_server.load("audio/sfx/step2.ogg")),
			(SfxKey::Step3, asset_server.load("audio/sfx/step3.ogg")),
			(SfxKey::Step4, asset_server.load("audio/sfx/step4.ogg")),
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

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub enum LevelID {
	Cycle,
	Bicycle,
	Tricycle,
	CargoTricycle,
	SquareCycle,
	DiamondCycle,
	Lotus,
	ThreeInARow,
	TripleRing,
	Car,
	Olympic,
	Pedalo,
	Pyramid,
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
	fn from_world(world: &mut World) -> Self {
		let asset_server = world.resource::<AssetServer>();
		[
			(LevelID::Cycle, asset_server.load("levels/cycle.txt")),
			(LevelID::Bicycle, asset_server.load("levels/bicycle.txt")),
			(LevelID::Tricycle, asset_server.load("levels/tricycle.txt")),
			(
				LevelID::CargoTricycle,
				asset_server.load("levels/cargo.txt"),
			),
			(LevelID::SquareCycle, asset_server.load("levels/square.txt")),
			(
				LevelID::DiamondCycle,
				asset_server.load("levels/diamond.txt"),
			),
			(LevelID::Lotus, asset_server.load("levels/lotus.txt")),
			(
				LevelID::ThreeInARow,
				asset_server.load("levels/three-row.txt"),
			),
			(LevelID::TripleRing, asset_server.load("levels/triple.txt")),
			(LevelID::Car, asset_server.load("levels/car.txt")),
			(LevelID::Olympic, asset_server.load("levels/olympic.txt")),
			(LevelID::Pedalo, asset_server.load("levels/pedalo.txt")),
			(LevelID::Pyramid, asset_server.load("levels/pyramid.txt")),
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
