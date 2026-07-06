//! Loading of [`PlaybackSequence`] from assets

use super::script::PlaybackSequence;
use crate::epilang::{
	CompileError, Interpreter, InterpreterEndState, InterpreterError, builtins::NoDomainValue,
	compile,
};
use crate::game::level::backend;
use bevy::asset::{AssetLoader, AsyncReadExt as _, LoadContext};
use bevy::prelude::*;

pub(super) fn plugin(app: &mut App) {
	app.init_asset::<PlaybackSequenceRepository>()
		.init_asset_loader::<PlaybackSequenceLoader>()
		.init_resource::<PlaybackSequenceRepoHandle>();
}

/// List of all [`PlaybackSequence`]s available
#[derive(Asset, TypePath, Deref)]
pub struct PlaybackSequenceRepository(pub Vec<PlaybackSequence>);

/// Asset loader for playback scripts
#[derive(TypePath, Default)]
struct PlaybackSequenceLoader;

#[derive(Debug)]
enum PlaybackSequenceLoadError {
	IO(std::io::Error),
	Compile(CompileError),
	Timeout,
	Runtime(InterpreterError<std::convert::Infallible, NoDomainValue>),
}

impl std::fmt::Display for PlaybackSequenceLoadError {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			PlaybackSequenceLoadError::IO(e) => write!(f, "io error: {e}"),
			PlaybackSequenceLoadError::Compile(e) => write!(f, "{e}"),
			PlaybackSequenceLoadError::Timeout => f.write_str("interpreter timed out"),
			PlaybackSequenceLoadError::Runtime(e) => write!(f, "{e}"),
		}
	}
}

impl std::error::Error for PlaybackSequenceLoadError {
	fn cause(&self) -> Option<&dyn std::error::Error> {
		match self {
			PlaybackSequenceLoadError::IO(e) => Some(e),
			PlaybackSequenceLoadError::Compile(e) => Some(e),
			PlaybackSequenceLoadError::Timeout => None,
			PlaybackSequenceLoadError::Runtime(e) => Some(e),
		}
	}
}

impl AssetLoader for PlaybackSequenceLoader {
	type Asset = PlaybackSequenceRepository;
	type Settings = ();
	type Error = PlaybackSequenceLoadError;

	fn extensions(&self) -> &[&str] {
		&["txt", "epilang"]
	}

	fn load(
		&self,
		reader: &mut dyn bevy::asset::io::Reader,
		_settings: &Self::Settings,
		load_context: &mut LoadContext,
	) -> impl bevy::tasks::ConditionalSendFuture<Output = Result<Self::Asset, Self::Error>> {
		async {
			let mut s = String::new();
			reader
				.read_to_string(&mut s)
				.await
				.map_err(PlaybackSequenceLoadError::IO)?;
			let module = compile(&s).map_err(PlaybackSequenceLoadError::Compile)?;
			let builder = PlaybackSequenceBuilder::default();
			let mut interpreter = Interpreter::new(&module, builder);
			match interpreter.run(backend::MAX_INTERPRETER_ITERATIONS) {
				InterpreterEndState::Timeout => return Err(PlaybackSequenceLoadError::Timeout),
				InterpreterEndState::Halted(result) => {
					result.map_err(PlaybackSequenceLoadError::Runtime)?
				}
			}
			for warning in interpreter.get_warnings() {
				log::warn!("{}: {warning}", load_context.path());
			}
			Ok(interpreter.backend.build())
		}
	}
}

/// [`InterpreterBackend`] for constructing a [`PlaybackSequence`]
///
/// [`InterpreterBackend`]: crate::epilang::interpreter::InterpreterBackend
#[derive(Default)]
pub struct PlaybackSequenceBuilder {
	pub sequences: Vec<PlaybackSequence>,
	pub current_sequence: PlaybackSequence,
}

impl PlaybackSequenceBuilder {
	pub fn build(mut self) -> PlaybackSequenceRepository {
		// Commit the last sequence if there is anything
		if !self.current_sequence.is_empty() {
			self.flush_sequence();
		}
		PlaybackSequenceRepository(self.sequences)
	}

	pub fn flush_sequence(&mut self) {
		self.sequences
			.push(std::mem::take(&mut self.current_sequence));
	}
}

#[derive(Resource, Deref)]
pub struct PlaybackSequenceRepoHandle(pub Handle<PlaybackSequenceRepository>);

impl FromWorld for PlaybackSequenceRepoHandle {
	fn from_world(world: &mut World) -> Self {
		let asset_server = world.resource::<AssetServer>();
		Self(asset_server.load("recordings/recordings.epilang"))
	}
}
