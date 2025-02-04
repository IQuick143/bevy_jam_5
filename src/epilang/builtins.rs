use super::{interpreter::*, values::*};

pub fn default_builtin_variables<'a, T: DomainVariableValue + 'a>() -> VariablePool<'a, T> {
	VariablePool::from_iter([
		("pi", std::f32::consts::PI.into()),
		("true", true.into()),
		("false", false.into()),
	])
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
	fn fmt(&self, _: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
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
		args: ArgumentStream<'a, '_, Self::Value>,
		_: WarningSink<Self::Warning>,
	) -> Result<
		ReturnValue<'a, Self::Value>,
		FunctionCallError<Self::Error, <Self::Value as DomainVariableValue>::Type>,
	> {
		Self::call_function::<Self::Value>(function_name, args)
	}
}

impl DefaultInterpreterBackend {
	pub fn call_function<T: DomainVariableValue>(
		function_name: &str,
		mut args: ArgumentStream<T>,
	) -> Result<
		ReturnValue<'static, <Self as InterpreterBackend>::Value>,
		FunctionCallError<<Self as InterpreterBackend>::Error, T::Type>,
	> {
		let result = match function_name {
			"sqrt" => args.read_single_as::<f32>()?.sqrt().into(),
			"sin" => args.read_single_as::<f32>()?.sin().into(),
			"cos" => args.read_single_as::<f32>()?.cos().into(),
			"tan" => args.read_single_as::<f32>()?.tan().into(),
			"floor" => args.read_single_as::<f32>()?.floor().into(),
			"ceil" => args.read_single_as::<f32>()?.ceil().into(),
			"round" => args.read_single_as::<f32>()?.round().into(),
			"abs" => Self::abs(args)?,
			"int" => Self::int(args)?,
			_ => return Err(FunctionCallError::FunctionDoesNotExist),
		};
		Ok(ReturnValue::pure(result))
	}

	fn abs<T: DomainVariableValue>(
		mut args: ArgumentStream<T>,
	) -> Result<
		VariableValue<'static, <Self as InterpreterBackend>::Value>,
		FunctionCallError<<Self as InterpreterBackend>::Error, T::Type>,
	> {
		use VariableValue::*;
		match args.read_single()? {
			Int(i) => i
				.checked_abs()
				.map(VariableValue::Int)
				.ok_or(ArithmeticOverflowError.into()),
			Float(f) => Ok(f.abs().into()),
			other => Err(ArgumentError::TypeError(other.get_type()).into()),
		}
	}

	fn int<T: DomainVariableValue>(
		mut args: ArgumentStream<T>,
	) -> Result<
		VariableValue<'static, <Self as InterpreterBackend>::Value>,
		FunctionCallError<<Self as InterpreterBackend>::Error, T::Type>,
	> {
		use VariableValue::*;
		match args.read_single()? {
			Int(i) => Ok((*i).into()),
			Float(f) => Ok((*f as i32).into()),
			Bool(b) => Ok((*b as i32).into()),
			other => Err(ArgumentError::TypeError(other.get_type()).into()),
		}
	}
}
