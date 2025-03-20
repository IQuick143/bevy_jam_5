use bevy::asset::AsyncReadExt;

use super::{
	backend::{domain::*, level_pack::*},
	list::*,
	*,
};
use crate::epilang::*;

pub fn plugin(app: &mut App) {
	app.init_asset::<LevelList>();
	app.init_asset_loader::<LevelListLoader>();
}

#[derive(Default)]
struct LevelListLoader;

#[derive(Debug)]
enum LevelListLoadError {
	IO(std::io::Error),
	Compile(CompileError),
	Runtime(InterpreterError<RuntimeError, DomainType>),
	Build(LevelListBuildError),
	Timeout,
}

impl std::error::Error for LevelListLoadError {}

impl std::fmt::Display for LevelListLoadError {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::IO(e) => e.fmt(f),
			Self::Compile(e) => e.fmt(f),
			Self::Runtime(e) => e.fmt(f),
			Self::Build(e) => write!(f, "in finalization: {e}"),
			Self::Timeout => f.write_str("Epilang program timed out"),
		}
	}
}

impl From<std::io::Error> for LevelListLoadError {
	fn from(value: std::io::Error) -> Self {
		Self::IO(value)
	}
}

impl From<CompileError> for LevelListLoadError {
	fn from(value: CompileError) -> Self {
		Self::Compile(value)
	}
}

impl From<InterpreterError<RuntimeError, DomainType>> for LevelListLoadError {
	fn from(value: InterpreterError<RuntimeError, DomainType>) -> Self {
		Self::Runtime(value)
	}
}

impl From<LevelListBuildError> for LevelListLoadError {
	fn from(value: LevelListBuildError) -> Self {
		Self::Build(value)
	}
}

impl bevy::asset::AssetLoader for LevelListLoader {
	type Asset = LevelList;
	type Error = LevelListLoadError;
	type Settings = ();

	fn load(
		&self,
		reader: &mut dyn bevy::asset::io::Reader,
		_settings: &Self::Settings,
		load_context: &mut bevy::asset::LoadContext,
	) -> impl bevy::tasks::ConditionalSendFuture<Output = Result<Self::Asset, Self::Error>> {
		async {
			let mut s = String::new();
			reader.read_to_string(&mut s).await?;
			let module = compile(&s)?;
			let mut interpreter = Interpreter::new(&module, LevelListBuilder::new());
			match interpreter.run(backend::MAX_INTERPRETER_ITERATIONS) {
				InterpreterEndState::Timeout => return Err(LevelListLoadError::Timeout),
				InterpreterEndState::Halted(result) => result?,
			}
			for warning in interpreter.get_warnings() {
				log::warn!("{}: {warning}", load_context.asset_path());
			}
			Ok(interpreter.backend.build(load_context)?)
		}
	}

	fn extensions(&self) -> &[&str] {
		&["txt"]
	}
}
