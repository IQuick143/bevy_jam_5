use bevy::asset::AsyncReadExt;

use super::*;

pub fn plugin(app: &mut App) {
	app.init_asset_loader::<LevelLoader>();
	app.init_asset::<LevelData>();
}

#[derive(Default)]
struct LevelLoader;

#[derive(Debug)]
pub enum LevelLoadingError {
	IO(std::io::Error),
	Parsing(parser::LevelParsingError),
}

impl From<std::io::Error> for LevelLoadingError {
	fn from(value: std::io::Error) -> Self {
		Self::IO(value)
	}
}

impl From<parser::LevelParsingError> for LevelLoadingError {
	fn from(value: parser::LevelParsingError) -> Self {
		Self::Parsing(value)
	}
}

impl std::fmt::Display for LevelLoadingError {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			LevelLoadingError::IO(e) => f.write_fmt(format_args!(
				"Level could not be loaded because of an IO error: {}",
				e
			)),
			LevelLoadingError::Parsing(e) => f.write_fmt(format_args!(
				"Level could not be loaded because of a parsing error: {}",
				e
			)),
		}
	}
}

impl std::error::Error for LevelLoadingError {}

impl bevy::asset::AssetLoader for LevelLoader {
	type Asset = LevelData;
	type Error = LevelLoadingError;
	type Settings = ();

	fn load(
		&self,
		reader: &mut dyn bevy::asset::io::Reader,
		_settings: &Self::Settings,
		_load_context: &mut bevy::asset::LoadContext,
	) -> impl bevy::utils::ConditionalSendFuture<Output = Result<Self::Asset, Self::Error>> {
		async {
			let mut s = String::new();
			reader.read_to_string(&mut s).await?;
			let level_data = parser::parse(&s)?;
			Ok(level_data)
		}
	}

	fn extensions(&self) -> &[&str] {
		&["txt"]
	}
}
