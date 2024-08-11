use bevy::asset::AsyncReadExt;

use super::*;

pub fn plugin(app: &mut App) {
	app.init_asset_loader::<LevelLoader>();
	app.init_asset::<LevelAsset>();
}

#[derive(Asset, Clone, Debug, Reflect)]
pub struct LevelAsset {
	pub name: String,
	pub hint: Option<String>,
	pub data: ValidLevelData,
	pub layout: layout::LevelLayout,
}

#[derive(Default)]
struct LevelLoader;

#[derive(Debug)]
pub enum LevelLoadingError {
	IO(std::io::Error),
	Validation(LevelDataValidationError),
	Parsing(parser::LevelParsingError),
	Layout(layout::LevelLayoutError),
}

impl From<std::io::Error> for LevelLoadingError {
	fn from(value: std::io::Error) -> Self {
		Self::IO(value)
	}
}

impl From<LevelDataValidationError> for LevelLoadingError {
	fn from(value: LevelDataValidationError) -> Self {
		Self::Validation(value)
	}
}

impl From<parser::LevelParsingError> for LevelLoadingError {
	fn from(value: parser::LevelParsingError) -> Self {
		Self::Parsing(value)
	}
}

impl From<layout::LevelLayoutError> for LevelLoadingError {
	fn from(value: layout::LevelLayoutError) -> Self {
		Self::Layout(value)
	}
}

impl std::fmt::Display for LevelLoadingError {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			LevelLoadingError::IO(e) => f.write_fmt(format_args!(
				"Level could not be loaded because of an IO error: {}",
				e
			)),
			LevelLoadingError::Validation(e) => f.write_fmt(format_args!(
				"Level could not be loaded because it is invalid: {}",
				e
			)),
			LevelLoadingError::Parsing(e) => f.write_fmt(format_args!(
				"Level could not be loaded because of a parsing error: {}",
				e
			)),
			LevelLoadingError::Layout(e) => f.write_fmt(format_args!(
				"Level could not be loaded because of an error during layout calculations: {}",
				e
			)),
		}
	}
}

impl std::error::Error for LevelLoadingError {}

impl bevy::asset::AssetLoader for LevelLoader {
	type Asset = LevelAsset;
	type Error = LevelLoadingError;
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
			let level_data = parser::parse(&s)?;
			let validated = ValidLevelData::try_from(level_data.data)?;
			let layout = {
				let mut builder = layout::LevelLayoutBuilder::new(&validated);
				for placement in level_data.layout {
					builder.add_placement(placement)?;
				}
				builder.build()?
			};
			Ok(LevelAsset {
				name: level_data
					.metadata
					.get("name")
					.cloned()
					.unwrap_or("MISSING_NAME".into()),
				hint: level_data.metadata.get("hint").cloned(),
				data: validated,
				layout,
			})
		}
	}

	fn extensions(&self) -> &[&str] {
		&["txt"]
	}
}
