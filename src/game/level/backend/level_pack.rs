//! Epilang backend for [`LevelListBuilder`]

use super::{super::list::*, domain::*};
use crate::epilang::{
	builtins,
	interpreter::{ArgumentError::*, FunctionCallError::*, *},
	values::{DomainVariableValue, VariableValue},
};
use itertools::Itertools as _;

impl InterpreterBackend for LevelListBuilder {
	type Value = DomainValue;
	type Error = RuntimeError;
	type Warning = RuntimeWarning;

	fn call_function(
		&mut self,
		function_name: &str,
		args: ArgumentStream<Self::Value>,
		warnings: WarningSink<Self::Warning>,
	) -> CallResult {
		// Use the default built-in math functions first, only proceed if none match
		let builtin_function_result =
			builtins::DefaultInterpreterBackend::call_function(function_name, args.clone());
		if !matches!(builtin_function_result, Err(FunctionDoesNotExist)) {
			return Ok(builtin_function_result
				.map_err(|err| err.map_domain(Into::into))?
				.map_domain(|_| unreachable!()));
		}

		match function_name {
			"level" => self.call_level(args),
			"hub" => self.call_hub(args),
			"next" => self.call_next(args, warnings),
			_ => Err(FunctionDoesNotExist),
		}
	}
}

type CallResult =
	Result<ReturnValue<'static, DomainValue>, FunctionCallError<RuntimeError, DomainType>>;

impl LevelListBuilder {
	fn call_level(&mut self, mut args: ArgumentStream<DomainValue>) -> CallResult {
		let level_path: &str = args.read_single_as()?;
		let level_id = self.add_level(level_path)?;
		Ok(ReturnValue::pure(LevelId(level_id).into()))
	}

	fn call_hub(&mut self, mut args: ArgumentStream<DomainValue>) -> CallResult {
		use DomainValue::*;
		use VariableValue::*;

		let hub_id = self.add_hub()?;

		while let Some(arg) = args.read_until_end_or_separator() {
			match arg {
				Domain(Level(LevelId(level_id))) => self.set_parent_for_level(*level_id, hub_id)?,
				Domain(Hub(HubId(other_id))) => self.set_parent_for_hub(*other_id, hub_id)?,
				_ => return Err(TypeError(arg.get_type()).into()),
			}
		}

		args.read_end()?;
		Ok(ReturnValue::with_side_effect(HubId(hub_id).into()))
	}

	fn call_next(
		&mut self,
		mut args: ArgumentStream<DomainValue>,
		mut warnings: WarningSink<RuntimeWarning>,
	) -> CallResult {
		let mut levels = Vec::new();
		while let Some(LevelId(level_id)) = args.read_as_until_end_or_separator()? {
			levels.push(level_id);
		}
		args.read_end()?;

		if levels.len() < 2 {
			warnings.emit(RuntimeWarning::EmptyNext.into());
		}

		for (a, b) in levels.iter().copied().tuple_windows() {
			self.set_next_level(a, b)?;
		}

		Ok(ReturnValue::void())
	}
}

#[derive(Debug)]
pub enum RuntimeError {
	Builder(LevelListBuildError),
	ArithmeticOverflow,
}

impl From<builtins::ArithmeticOverflowError> for RuntimeError {
	fn from(_: builtins::ArithmeticOverflowError) -> Self {
		Self::ArithmeticOverflow
	}
}

impl From<LevelListBuildError> for RuntimeError {
	fn from(value: LevelListBuildError) -> Self {
		Self::Builder(value)
	}
}

impl From<LevelListBuildError> for FunctionCallError<RuntimeError, DomainType> {
	fn from(value: LevelListBuildError) -> Self {
		RuntimeError::from(value).into()
	}
}

impl std::error::Error for RuntimeError {}

impl std::fmt::Display for RuntimeError {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::ArithmeticOverflow => f.write_str("arithmetic overflow"),
			Self::Builder(e) => e.fmt(f),
		}
	}
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum RuntimeWarning {
	/// next was called with less than two levels,
	/// creating no succession relation
	EmptyNext,
}

impl std::error::Error for RuntimeWarning {}

impl std::fmt::Display for RuntimeWarning {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::EmptyNext => f.write_str("too few levels to actually create a succession"),
		}
	}
}
