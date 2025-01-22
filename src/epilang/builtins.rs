use super::{interpreter::*, values::*};
use bevy::utils::HashMap;

macro_rules! float_method {
	( $name:ident ( $args:expr ) ) => {
		match $args {
			[ArgumentValue::Argument(VariableValue::Int(i))] => Ok(*i as f32),
			[ArgumentValue::Argument(VariableValue::Float(f))] => Ok(*f),
			[ArgumentValue::Argument(other)] => Err(FunctionCallError::TypeError(other.get_type())),
			_ => Err(FunctionCallError::BadArgumentCount),
		}
		.map(f32::$name)
		.map(VariableValue::Float)
		.map(ReturnValue::pure)
	};
}

pub fn default_builtin_variables<'a, T: DomainVariableValue + 'a>(
) -> HashMap<&'a str, VariableSlot<'a, T>> {
	HashMap::from_iter([(
		"pi",
		VariableSlot::builtin(VariableValue::Float(std::f32::consts::PI)),
	)])
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, Default)]
pub struct ArithmeticOverflowError;

impl std::error::Error for ArithmeticOverflowError {}

impl std::fmt::Display for ArithmeticOverflowError {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		f.write_str("integer arithmetic overflow")
	}
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum NoDomainValue {}

impl std::fmt::Display for NoDomainValue {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		unreachable!("This type cannot be constructed")
	}
}

impl DomainVariableValue for NoDomainValue {
	type Type = Self;

	fn get_type(&self) -> Self::Type {
		unreachable!("This type cannot be constructed")
	}
}

#[derive(Clone, Copy, Debug, Default)]
pub struct DefaultInterpreterBackend;

impl InterpreterBackend for DefaultInterpreterBackend {
	type Error = ArithmeticOverflowError;

	type Warning = std::convert::Infallible;

	type Value = NoDomainValue;

	fn call_function<'a>(
		&mut self,
		function_name: &str,
		args: &[ArgumentValue<'a, Self::Value>],
		_: WarningSink<Self::Warning>,
	) -> Result<
		ReturnValue<'a, Self::Value>,
		FunctionCallError<Self::Error, <Self::Value as DomainVariableValue>::Type>,
	> {
		Self::call_function(function_name, args)
	}
}

impl DefaultInterpreterBackend {
	pub fn call_function<'a>(
		function_name: &str,
		args: &[ArgumentValue<'a, <Self as InterpreterBackend>::Value>],
	) -> Result<
		ReturnValue<'a, <Self as InterpreterBackend>::Value>,
		FunctionCallError<
			<Self as InterpreterBackend>::Error,
			<<Self as InterpreterBackend>::Value as DomainVariableValue>::Type,
		>,
	> {
		match function_name {
			"sqrt" => float_method!(sqrt(args)),
			"sin" => float_method!(sin(args)),
			"cos" => float_method!(cos(args)),
			"tan" => float_method!(tan(args)),
			"floor" => float_method!(floor(args)),
			"ceil" => float_method!(ceil(args)),
			"round" => float_method!(round(args)),
			"abs" => Self::abs(args).map(ReturnValue::pure),
			"int" => Self::int(args).map(ReturnValue::pure),
			_ => Err(FunctionCallError::FunctionDoesNotExist),
		}
	}

	fn abs(
		args: &[ArgumentValue<<Self as InterpreterBackend>::Value>],
	) -> Result<
		VariableValue<'static, <Self as InterpreterBackend>::Value>,
		FunctionCallError<
			<Self as InterpreterBackend>::Error,
			<<Self as InterpreterBackend>::Value as DomainVariableValue>::Type,
		>,
	> {
		match args {
			[ArgumentValue::Argument(VariableValue::Int(i))] => i
				.checked_abs()
				.map(VariableValue::Int)
				.ok_or(ArithmeticOverflowError.into()),
			[ArgumentValue::Argument(VariableValue::Float(f))] => Ok(VariableValue::Float(f.abs())),
			[ArgumentValue::Argument(other)] => Err(FunctionCallError::TypeError(other.get_type())),
			_ => Err(FunctionCallError::BadArgumentCount),
		}
	}

	fn int(
		args: &[ArgumentValue<<Self as InterpreterBackend>::Value>],
	) -> Result<
		VariableValue<'static, <Self as InterpreterBackend>::Value>,
		FunctionCallError<
			<Self as InterpreterBackend>::Error,
			<<Self as InterpreterBackend>::Value as DomainVariableValue>::Type,
		>,
	> {
		match args {
			[ArgumentValue::Argument(VariableValue::Int(i))] => Ok(VariableValue::Int(*i)),
			[ArgumentValue::Argument(VariableValue::Float(f))] => Ok(VariableValue::Int(*f as i32)),
			[ArgumentValue::Argument(VariableValue::Bool(b))] => Ok(VariableValue::Int(*b as i32)),
			[ArgumentValue::Argument(other)] => Err(FunctionCallError::TypeError(other.get_type())),
			_ => Err(FunctionCallError::BadArgumentCount),
		}
	}
}
