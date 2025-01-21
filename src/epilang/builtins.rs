use super::{interpreter::*, values::VariableValue};
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

pub fn default_builtin_variables<'a>() -> HashMap<&'a str, VariableSlot<'a>> {
	HashMap::from_iter([(
		"pi",
		VariableSlot::builtin(VariableValue::Float(std::f32::consts::PI)),
	)])
}

#[derive(Clone, Copy, Debug, Default)]
pub struct DefaultInterpreterBackend;

impl InterpreterBackend for DefaultInterpreterBackend {
	type Error = std::convert::Infallible;

	type Warning = std::convert::Infallible;

	fn call_function<'a>(
		&mut self,
		function_name: &str,
		args: &[ArgumentValue<'a>],
		_: WarningSink<Self::Warning>,
	) -> Result<ReturnValue<'a>, FunctionCallError<Self::Warning>> {
		Self::call_function(function_name, args)
	}
}

impl DefaultInterpreterBackend {
	pub fn call_function<'a>(
		function_name: &str,
		args: &[ArgumentValue<'a>],
	) -> Result<ReturnValue<'a>, FunctionCallError<<Self as InterpreterBackend>::Error>> {
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
		args: &[ArgumentValue],
	) -> Result<VariableValue<'static>, FunctionCallError<<Self as InterpreterBackend>::Error>> {
		match args {
			[ArgumentValue::Argument(VariableValue::Int(i))] => i
				.checked_abs()
				.map(VariableValue::Int)
				.ok_or(FunctionCallError::ArithmeticOverflow),
			[ArgumentValue::Argument(VariableValue::Float(f))] => Ok(VariableValue::Float(f.abs())),
			[ArgumentValue::Argument(other)] => Err(FunctionCallError::TypeError(other.get_type())),
			_ => Err(FunctionCallError::BadArgumentCount),
		}
	}

	fn int(
		args: &[ArgumentValue],
	) -> Result<VariableValue<'static>, FunctionCallError<<Self as InterpreterBackend>::Error>> {
		match args {
			[ArgumentValue::Argument(VariableValue::Int(i))] => Ok(VariableValue::Int(*i)),
			[ArgumentValue::Argument(VariableValue::Float(f))] => Ok(VariableValue::Int(*f as i32)),
			[ArgumentValue::Argument(VariableValue::Bool(b))] => Ok(VariableValue::Int(*b as i32)),
			[ArgumentValue::Argument(other)] => Err(FunctionCallError::TypeError(other.get_type())),
			_ => Err(FunctionCallError::BadArgumentCount),
		}
	}
}
