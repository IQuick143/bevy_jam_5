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
	warning_handler: impl FnMut(FinalizeWarning),
) -> ResultNonExclusive<LevelData, FinalizeError> {
	match read_variables_before_finalize(&mut builder, variable_pool, warning_handler) {
		Ok(()) => builder.build().map_err(FinalizeError::BuilderError),
		Err(err) => err.into(),
	}
}

fn read_variables_before_finalize(
	builder: &mut LevelBuilder,
	variable_pool: &VariablePool<DomainValue>,
	mut warning_handler: impl FnMut(FinalizeWarning),
) -> Result<(), FinalizeError> {
	if let Some(level_name) = variable_pool.load_as::<&str>("name").transpose()? {
		if let Err(err) = builder.set_level_name(level_name.to_owned()) {
			return Err(FinalizeError::BuilderError(err));
		}
	} else {
		warning_handler(FinalizeWarning::LevelNameNotSet);
	}

	if let Some(level_hint) = variable_pool.load_as::<&str>("hint").transpose()? {
		if let Err(err) = builder.set_level_hint(level_hint.to_owned()) {
			return Err(FinalizeError::BuilderError(err));
		}
	}

	if let Some(level_scale) = variable_pool
		.load_as::<f32>("local_to_world_units")
		.transpose()?
	{
		builder.set_level_scale(level_scale);
	}

	if let Some(bb_left) = variable_pool.load_as::<f32>("bb_left").transpose()? {
		builder.explicit_bounding_box().left = Some(bb_left);
	}

	if let Some(bb_left) = variable_pool.load_as::<f32>("bb_top").transpose()? {
		builder.explicit_bounding_box().top = Some(bb_left);
	}

	if let Some(bb_left) = variable_pool.load_as::<f32>("bb_right").transpose()? {
		builder.explicit_bounding_box().right = Some(bb_left);
	}

	if let Some(bb_left) = variable_pool.load_as::<f32>("bb_bottom").transpose()? {
		builder.explicit_bounding_box().bottom = Some(bb_left);
	}

	if let Some(init_scale) = variable_pool.load_as::<f32>("init_scale").transpose()? {
		builder.set_initial_zoom(init_scale);
	}

	if let Some(init_cam_x) = variable_pool.load_as::<f32>("init_cam_x").transpose()? {
		builder.explicit_initial_camera_pos().x = Some(init_cam_x);
	}

	if let Some(init_cam_y) = variable_pool.load_as::<f32>("init_cam_y").transpose()? {
		builder.explicit_initial_camera_pos().y = Some(init_cam_y);
	}

	Ok(())
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
