//! Finalization of the [`LevelBuilder`] Epilang backend

use super::super::{
	super::{
		builder::{error::LevelBuilderError, LevelBuilder},
		*,
	},
	domain::*,
};
use crate::{
	epilang::interpreter::{LoadedVariableTypeError, VariablePool},
	game::level::builder::ResultNonExclusive,
};

/// Finalization procedure for the [`LevelBuilder`] backend.
/// Call this when the Epilang program is done executing.
pub fn finalize(
	mut builder: LevelBuilder,
	variable_pool: &VariablePool<DomainValue>,
	mut warning_handler: impl FnMut(FinalizeWarning),
) -> ResultNonExclusive<LevelData, FinalizeError> {
	match variable_pool.load_as::<&str>("name").transpose() {
		Ok(Some(level_name)) => {
			if let Err(err) = builder.set_level_name(level_name.to_owned()) {
				return FinalizeError::BuilderError(err).into();
			}
		}
		Ok(None) => {
			warning_handler(FinalizeWarning::LevelNameNotSet);
		}
		Err(err) => {
			return FinalizeError::InvalidVariableType(err).into();
		}
	}

	match variable_pool.load_as::<&str>("hint").transpose() {
		Ok(Some(level_hint)) => {
			if let Err(err) = builder.set_level_hint(level_hint.to_owned()) {
				return FinalizeError::BuilderError(err).into();
			}
		}
		Ok(None) => {}
		Err(err) => {
			return FinalizeError::InvalidVariableType(err).into();
		}
	}

	match variable_pool
		.load_as::<f32>("local_to_world_units")
		.transpose()
	{
		Ok(Some(level_scale)) => {
			builder.set_level_scale(level_scale);
		}
		Ok(None) => {}
		Err(err) => {
			return FinalizeError::InvalidVariableType(err).into();
		}
	}

	builder.build().map_err(FinalizeError::BuilderError)
}

#[derive(Clone, PartialEq, Debug)]
pub enum FinalizeError {
	/// Level builder has failed during finalization
	BuilderError(LevelBuilderError),
	/// A variable that is interpreted by finalization procedure has incorrect type
	InvalidVariableType(LoadedVariableTypeError<DomainType>),
}

impl std::error::Error for FinalizeError {}

impl std::fmt::Display for FinalizeError {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::BuilderError(e) => write!(f, "while finishing level build: {e}"),
			Self::InvalidVariableType(e) => e.fmt(f),
		}
	}
}

impl From<LevelBuilderError> for FinalizeError {
	fn from(value: LevelBuilderError) -> Self {
		Self::BuilderError(value)
	}
}

impl From<LoadedVariableTypeError<DomainType>> for FinalizeError {
	fn from(value: LoadedVariableTypeError<DomainType>) -> Self {
		Self::InvalidVariableType(value)
	}
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum FinalizeWarning {
	/// Level name has not been set
	LevelNameNotSet,
}

impl std::error::Error for FinalizeWarning {}

impl std::fmt::Display for FinalizeWarning {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::LevelNameNotSet => {
				f.write_str("level name has not been set (assign a string to variable 'name')")
			}
		}
	}
}
