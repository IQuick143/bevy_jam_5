//! Finalization of the [`LevelBuilder`] Epilang backend

use super::super::{
	super::{
		builder::{error::LevelBuilderError, LevelBuilder},
		*,
	},
	domain::*,
};
use crate::epilang::interpreter::{LoadedVariableTypeError, VariablePool};

/// Finalization procedure for the [`LevelBuilder`] backend.
/// Call this when the Epilang program is done executing.
pub fn finalize(
	mut builder: LevelBuilder,
	variable_pool: &VariablePool<DomainValue>,
	mut warning_handler: impl FnMut(FinalizeWarning),
) -> Result<LevelData, FinalizeError> {
	if let Some(level_name) = variable_pool.load_as::<&str>("name").transpose()? {
		builder.set_level_name(level_name.to_owned())?;
	} else {
		warning_handler(FinalizeWarning::LevelNameNotSet);
	}

	if let Some(level_hint) = variable_pool.load_as::<&str>("hint").transpose()? {
		builder.set_level_hint(level_hint.to_owned())?;
	}

	Ok(builder.build()?)
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
