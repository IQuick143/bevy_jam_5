use bevy::asset::AsyncReadExt;

use super::{list::*, *};

pub fn plugin(app: &mut App) {
	app.init_asset::<LevelListAsset>();
	app.init_asset_loader::<LevelListLoader>();
}

/// An asset that represents a loaded level list
#[derive(Asset, Clone, Debug, Reflect)]
pub struct LevelListAsset {
	/// The level list
	pub list: LevelList,
	/// IDs/slugs of levels. Correspond to level asset file names.
	pub slugs: Vec<String>,
}

#[derive(Default)]
struct LevelListLoader;

#[derive(Debug)]
enum LevelListLoadError {
	IO(std::io::Error),
	Lex(lex::LexError),
	Build(LevelListBuildError),
	UnknownAssignmentKey(String),
	UnknownKeyword(String),
}

impl std::error::Error for LevelListLoadError {}

impl std::fmt::Display for LevelListLoadError {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::IO(e) => e.fmt(f),
			Self::Lex(e) => e.fmt(f),
			Self::Build(e) => e.fmt(f),
			Self::UnknownAssignmentKey(key) => write!(f, "Cannot assign to unknown key {key}."),
			Self::UnknownKeyword(kw) => write!(f, "Unknown keyword {kw}."),
		}
	}
}

impl From<std::io::Error> for LevelListLoadError {
	fn from(value: std::io::Error) -> Self {
		Self::IO(value)
	}
}

impl From<lex::LexError> for LevelListLoadError {
	fn from(value: lex::LexError) -> Self {
		Self::Lex(value)
	}
}

impl From<LevelListBuildError> for LevelListLoadError {
	fn from(value: LevelListBuildError) -> Self {
		Self::Build(value)
	}
}

impl bevy::asset::AssetLoader for LevelListLoader {
	type Asset = LevelListAsset;
	type Error = LevelListLoadError;
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
			let mut level_slugs = Vec::new();
			let mut builder = LevelListBuilder::new();
			for line in lex::parse(&s) {
				let (_, statement) = line?;
				match statement {
					lex::RawStatement::Action(statement) => match statement.verb {
						"LEVEL" => {
							for arg in &statement.values {
								builder.declare_level(arg)?;
								level_slugs.push(arg.to_string());
							}
						}
						"SECTION" => {
							builder.begin_section(statement.values.first().copied())?;
						}
						other => return Err(LevelListLoadError::UnknownKeyword(other.to_owned())),
					},
					lex::RawStatement::Assignment(statement) => {
						if !statement.key.eq_ignore_ascii_case("name") {
							return Err(LevelListLoadError::UnknownAssignmentKey(
								statement.key.to_owned(),
							));
						}
						builder.set_current_section_name(statement.value)?;
					}
				}
			}
			let level_list = builder.build()?;
			Ok(LevelListAsset {
				list: level_list,
				slugs: level_slugs,
			})
		}
	}

	fn extensions(&self) -> &[&str] {
		&["txt"]
	}
}
