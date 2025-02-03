use super::{ast::*, values::*, SourceLocation};
use bevy::utils::HashMap;
use std::ops::Range;

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum InterpreterError<E: std::error::Error, T: DomainVariableType> {
	/// One of the interpreter stacks has overflown.
	StackOverflow,
	/// An invariant of the interpreter has been unexpectedly violated.
	/// This indicates a bug in the interpreter and should never be seen.
	/// It is a valid approach to panic in this situation, but it can
	/// also be handled in a different way.
	InternalError,
	/// An error bound to a specific place in the source code
	/// Boxed to avoid returning very large types
	LogicError(Box<LogicError<E, T>>, Range<SourceLocation>),
}

impl<E: std::error::Error, T: DomainVariableType> std::error::Error for InterpreterError<E, T> {}

impl<E: std::error::Error, T: DomainVariableType> std::fmt::Display for InterpreterError<E, T> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::StackOverflow => f.write_str("Interpreter stack overflow"),
			Self::InternalError => f.write_str("internal interpreter error"),
			Self::LogicError(err, loc) => f.write_fmt(format_args!("{}: {err}", loc.start)),
		}
	}
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum LogicError<E: std::error::Error, T: DomainVariableType> {
	/// An expression used in conditional context (condition of if statement
	/// or short-circuiting boolean operator) evaluated to type other than boolean.
	IllegalConditionType(VariableType<T>),
	/// A variable was read but no variable of that name exists.
	VariableDoesNotExist(String),
	/// A unary operator was called on an unsupported type.
	IllegalTypeInUnaryOperator(UnaryOperator, VariableType<T>),
	/// A binary operator was called on unsupported types.
	IllegalTypeInBinaryOperator(BinaryOperator, VariableType<T>, VariableType<T>),
	/// Integer arithmetic has resulted in a value that cannot be represented.
	ArithmeticOverflow,
	/// Integer was raised to a negative integer power.
	NegativeIntPow,
	/// Error caused by a function call.
	FunctionCall(FunctionCallError<E, T>, String),
}

impl<E: std::error::Error, T: DomainVariableType> std::fmt::Display for LogicError<E, T> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::IllegalConditionType(ty) => f.write_fmt(format_args!(
				"value of invalid type {ty} passed to a condition"
			)),
			Self::VariableDoesNotExist(name) => {
				f.write_fmt(format_args!("variable {name} does not exist"))
			}
			Self::IllegalTypeInUnaryOperator(op, ty) => f.write_fmt(format_args!(
				"value of invalid type {ty} passed to unary operator {op}"
			)),
			Self::IllegalTypeInBinaryOperator(op, lty, rty) => f.write_fmt(format_args!(
				"values of invalid types {lty}, {rty} passed to binary operator {op}"
			)),
			Self::ArithmeticOverflow => f.write_str("integer arithmetic overflow"),
			Self::NegativeIntPow => f.write_str("integer raised to negative power"),
			Self::FunctionCall(err, name) => f.write_fmt(format_args!("in function {name}: {err}")),
		}
	}
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum FunctionCallError<E: std::error::Error, T: DomainVariableType> {
	/// No function of the given name exists
	FunctionDoesNotExist,
	/// Incorrect arguments were passed
	ArgumentError(ArgumentError<T>),
	/// Domain-specific error defined by [`InterpreterBackend`]
	Domain(E),
}

impl<E: std::error::Error, T: DomainVariableType> std::error::Error for FunctionCallError<E, T> {}

impl<E: std::error::Error, T: DomainVariableType> std::fmt::Display for FunctionCallError<E, T> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::FunctionDoesNotExist => f.write_str("function does not exist"),
			Self::ArgumentError(e) => e.fmt(f),
			Self::Domain(e) => std::fmt::Display::fmt(e, f),
		}
	}
}

impl<E: std::error::Error, T: DomainVariableType> From<E> for FunctionCallError<E, T> {
	fn from(value: E) -> Self {
		Self::Domain(value)
	}
}

impl<E: std::error::Error, T: DomainVariableType> FunctionCallError<E, T> {
	pub fn map_domain<F: std::error::Error>(
		self,
		map: impl FnOnce(E) -> F,
	) -> FunctionCallError<F, T> {
		match self {
			Self::FunctionDoesNotExist => FunctionCallError::FunctionDoesNotExist,
			Self::ArgumentError(e) => FunctionCallError::ArgumentError(e),
			Self::Domain(x) => FunctionCallError::Domain(map(x)),
		}
	}

	pub fn _map_type_domain<U: DomainVariableType>(
		self,
		map: impl FnOnce(T) -> U,
	) -> FunctionCallError<E, U> {
		match self {
			Self::FunctionDoesNotExist => FunctionCallError::FunctionDoesNotExist,
			Self::ArgumentError(e) => FunctionCallError::ArgumentError(e.map_domain(map)),
			Self::Domain(x) => FunctionCallError::Domain(x),
		}
	}
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum ArgumentError<T: DomainVariableType> {
	/// Incorrect number of arguments passed to the function
	BadArgumentCount,
	/// Incorrect types of arguments
	TypeError(VariableType<T>),
	/// Value of argument is out of range or not usable
	InvalidValue,
}

impl<T: DomainVariableType> ArgumentError<T> {
	pub fn map_domain<U: DomainVariableType>(self, map: impl FnOnce(T) -> U) -> ArgumentError<U> {
		match self {
			Self::BadArgumentCount => ArgumentError::BadArgumentCount,
			Self::TypeError(t) => ArgumentError::TypeError(t.map_domain(map)),
			Self::InvalidValue => ArgumentError::InvalidValue,
		}
	}
}

impl<T: DomainVariableType> std::fmt::Display for ArgumentError<T> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::BadArgumentCount => f.write_str("incorrect number of arguments"),
			Self::TypeError(ty) => write!(f, "argument has incorrect type {ty}"),
			Self::InvalidValue => f.write_str("argument value is invalid"),
		}
	}
}

impl<E: std::error::Error, T: DomainVariableType> From<ArgumentError<T>>
	for FunctionCallError<E, T>
{
	fn from(value: ArgumentError<T>) -> Self {
		Self::ArgumentError(value)
	}
}

pub trait IntoArgumentError<T: DomainVariableType> {
	fn into_argument_error(self) -> ArgumentError<T>;
}

impl<T: DomainVariableType, U: Into<ArgumentError<T>>> IntoArgumentError<T> for U {
	fn into_argument_error(self) -> ArgumentError<T> {
		self.into()
	}
}

impl<T: DomainVariableType> IntoArgumentError<T> for VariableType<T> {
	fn into_argument_error(self) -> ArgumentError<T> {
		ArgumentError::TypeError(self)
	}
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct InterpreterWarning<E: std::error::Error> {
	pub warning_code: WarningCode<E>,
	pub loc: Range<SourceLocation>,
}

impl<E: std::error::Error> std::fmt::Display for InterpreterWarning<E> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		f.write_fmt(format_args!("{}: {}", self.loc.start, self.warning_code))
	}
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum WarningCode<E: std::error::Error> {
	/// The value of an expression without side effects was not used
	DiscardedMustUse,
	/// A built-in variable was overwritten
	OverwrittenBuiltin(String),
	/// Domain-specific warning defined by [`InterpreterBackend`]
	Domain(E),
}

impl<E: std::error::Error> std::fmt::Display for WarningCode<E> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::DiscardedMustUse => f.write_str("Discarded value that must be used"),
			Self::OverwrittenBuiltin(name) => {
				f.write_fmt(format_args!("Assignment to builtin variable {name}"))
			}
			Self::Domain(w) => std::fmt::Display::fmt(w, f),
		}
	}
}

impl<E: std::error::Error> From<E> for WarningCode<E> {
	fn from(value: E) -> Self {
		Self::Domain(value)
	}
}

pub struct WarningSink<'a, E: std::error::Error> {
	warning_stash: &'a mut Vec<InterpreterWarning<E>>,
	loc: Range<SourceLocation>,
}

impl<'a, E: std::error::Error> WarningSink<'a, E> {
	pub fn emit(&mut self, warning_code: WarningCode<E>) {
		self.warning_stash.push(InterpreterWarning {
			warning_code,
			loc: self.loc.clone(),
		});
	}

	#[allow(unused)]
	pub fn reborrow(&'a mut self) -> Self {
		Self {
			warning_stash: self.warning_stash,
			loc: self.loc.clone(),
		}
	}
}

/// Container with arguments that gets passed to user implementations of functions
#[derive(Clone, Debug)]
pub struct ArgumentStream<'a, 's, T: DomainVariableValue + 'a>(&'s [ArgumentValue<'a, T>]);

impl<'a, 's, T: DomainVariableValue + 'a> ArgumentStream<'a, 's, T> {
	/// Removes the first argument from the stream if it iis a separator,
	/// fails otherwise
	pub fn read_separator(&mut self) -> Result<(), ArgumentError<T::Type>> {
		match self.peek() {
			Some(ArgumentValue::Separator) => {
				self.next();
				Ok(())
			}
			_ => Err(ArgumentError::BadArgumentCount),
		}
	}

	pub fn optional_separator(&mut self) {
		let _ = self.read_separator();
	}

	/// Fails if there are any remaining arguments, including separators
	pub fn read_end(&self) -> Result<(), ArgumentError<T::Type>> {
		if self.peek().is_some() {
			Err(ArgumentError::BadArgumentCount)
		} else {
			Ok(())
		}
	}

	/// Removes the first argument from the stream if it iis a separator,
	/// otherwise fails if there are any arguments left
	pub fn read_end_or_separator(&mut self) -> Result<(), ArgumentError<T::Type>> {
		match self.peek() {
			Some(ArgumentValue::Separator) => {
				self.next();
				Ok(())
			}
			None => Ok(()),
			_ => Err(ArgumentError::BadArgumentCount),
		}
	}

	/// Attempts to convert the next argument to the specified type,
	/// fails if there is not an argument or it does not have the correct type
	pub fn read_as<U>(&mut self) -> Result<U, ArgumentError<T::Type>>
	where
		&'s VariableValue<'a, T>: TryInto<U, Error: IntoArgumentError<T::Type>>,
	{
		match self.peek() {
			Some(ArgumentValue::Argument(arg)) => {
				let result = arg
					.try_into()
					.map_err(IntoArgumentError::into_argument_error);
				if result.is_ok() {
					self.next();
				}
				result
			}
			_ => Err(ArgumentError::BadArgumentCount),
		}
	}

	pub fn read_single_as<U>(&mut self) -> Result<U, ArgumentError<T::Type>>
	where
		&'s VariableValue<'a, T>: TryInto<U, Error: IntoArgumentError<T::Type>>,
	{
		if !self.is_on_last_argument() {
			return Err(ArgumentError::BadArgumentCount);
		}
		self.read_as()
	}

	pub fn read_as_until_end_or_separator<U>(&mut self) -> Result<Option<U>, ArgumentError<T::Type>>
	where
		&'s VariableValue<'a, T>: TryInto<U, Error: IntoArgumentError<T::Type>>,
	{
		match self.peek() {
			Some(ArgumentValue::Argument(arg)) => {
				let result = arg
					.try_into()
					.map_err(IntoArgumentError::into_argument_error);
				if result.is_ok() {
					self.next();
				}
				result.map(Some)
			}
			_ => Ok(None),
		}
	}

	pub fn optional_read_as<U>(&mut self) -> Option<U>
	where
		for<'u> &'u VariableValue<'a, T>: TryInto<U, Error: IntoArgumentError<T::Type>>,
	{
		self.read_as().ok()
	}

	pub fn read_as_or_blank_until_end_or_separator<U>(
		&mut self,
	) -> Result<Option<U>, ArgumentError<T::Type>>
	where
		&'s VariableValue<'a, T>: TryInto<U, Error: IntoArgumentError<T::Type>>,
	{
		self.read_blank()
			.map(|_| None)
			.or_else(|_| self.read_as_until_end_or_separator())
	}

	pub fn read_blank(&mut self) -> Result<(), ArgumentError<T::Type>> {
		match self.peek() {
			Some(ArgumentValue::Argument(VariableValue::Blank)) => {
				self.next();
				Ok(())
			}
			Some(ArgumentValue::Argument(other)) => Err(ArgumentError::TypeError(other.get_type())),
			_ => Err(ArgumentError::BadArgumentCount),
		}
	}

	pub fn read(&mut self) -> Result<&'s VariableValue<'a, T>, ArgumentError<T::Type>> {
		match self.peek() {
			Some(ArgumentValue::Argument(arg)) => {
				self.next();
				Ok(arg)
			}
			_ => Err(ArgumentError::BadArgumentCount),
		}
	}

	pub fn read_single(&mut self) -> Result<&'s VariableValue<'a, T>, ArgumentError<T::Type>> {
		if !self.is_on_last_argument() {
			return Err(ArgumentError::BadArgumentCount);
		}
		self.read()
	}

	pub fn read_until_end_or_separator(&mut self) -> Option<&'s VariableValue<'a, T>> {
		self.read().ok()
	}

	fn is_on_last_argument(&self) -> bool {
		self.0.len() == 1
	}

	fn peek(&self) -> Option<&'s ArgumentValue<'a, T>> {
		self.0.first()
	}

	fn next(&mut self) {
		self.0 = &self.0[1..];
	}
}

/// Collection of variables accessible to the interpreter
#[derive(Clone, Debug)]
pub struct VariablePool<'a, T: DomainVariableValue + 'a>(HashMap<&'a str, VariableSlot<'a, T>>);

impl<'a, T: DomainVariableValue + 'a> VariablePool<'a, T> {
	/// Constructs an empty variable pool
	pub fn new() -> Self {
		Self(HashMap::new())
	}

	/// Creates a new built-in variable and returns the previous value of that name, if any
	pub fn insert_builtin(
		&mut self,
		name: &'a str,
		value: impl Into<VariableValue<'a, T>>,
	) -> Option<VariableSlot<'a, T>> {
		self.store(name, VariableSlot::builtin(value.into()))
	}

	/// Loads a variable value
	pub fn load(&self, name: &str) -> Option<&VariableValue<'a, T>> {
		self.load_slot(name).map(|slot| &slot.value)
	}

	/// Loads a variable value and attempts to convert it to a requested type
	pub fn load_as<U>(&self, name: &str) -> Option<Result<U, LoadedVariableTypeError<T::Type>>>
	where
		for<'t> &'t VariableValue<'a, T>: TryInto<U, Error = VariableType<T::Type>>,
	{
		self.load(name).map(|v| {
			v.try_into().map_err(|t| LoadedVariableTypeError {
				variable_name: name.to_owned(),
				actual_type: t,
			})
		})
	}

	/// Loads a variable including metadata
	fn load_slot(&self, name: &str) -> Option<&VariableSlot<'a, T>> {
		self.0.get(name)
	}

	/// Stores a variable and returns the previous value of that name, if any
	///
	/// Intentionally private, this function should only be used by the interpreter.
	/// Users should use [`Self::insert_builtin`] to construct built-in variables.
	fn store(&mut self, name: &'a str, value: VariableSlot<'a, T>) -> Option<VariableSlot<'a, T>> {
		self.0.insert(name, value)
	}

	pub fn iter<'s>(&'s self) -> impl Iterator<Item = (&'a str, &'s VariableSlot<'a, T>)> {
		self.0.iter().map(|(&k, v)| (k, v))
	}

	pub fn into_keys_and_values(self) -> impl Iterator<Item = (&'a str, VariableValue<'a, T>)> {
		self.into_iter().map(|(k, v)| (k, v.value))
	}

	pub fn keys_and_values<'s>(
		&'s self,
	) -> impl Iterator<Item = (&'a str, &'s VariableValue<'a, T>)> {
		self.iter().map(|(k, v)| (k, &v.value))
	}
}

impl<'a, T: DomainVariableValue + 'a> Default for VariablePool<'a, T> {
	fn default() -> Self {
		Self::new()
	}
}

impl<'a, T: DomainVariableValue + 'a> FromIterator<(&'a str, VariableValue<'a, T>)>
	for VariablePool<'a, T>
{
	fn from_iter<I: IntoIterator<Item = (&'a str, VariableValue<'a, T>)>>(iter: I) -> Self {
		Self(HashMap::from_iter(
			iter.into_iter().map(|(k, v)| (k, VariableSlot::builtin(v))),
		))
	}
}

impl<'a, T: DomainVariableValue + 'a> IntoIterator for VariablePool<'a, T> {
	type IntoIter = <HashMap<&'a str, VariableSlot<'a, T>> as IntoIterator>::IntoIter;
	type Item = (&'a str, VariableSlot<'a, T>);
	fn into_iter(self) -> Self::IntoIter {
		self.0.into_iter()
	}
}

impl<'a, 's, T: DomainVariableValue + 'a> IntoIterator for &'s VariablePool<'a, T> {
	type IntoIter = <&'s HashMap<&'a str, VariableSlot<'a, T>> as IntoIterator>::IntoIter;
	type Item = (&'s &'a str, &'s VariableSlot<'a, T>);
	fn into_iter(self) -> Self::IntoIter {
		self.0.iter()
	}
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct LoadedVariableTypeError<T: DomainVariableType> {
	pub variable_name: String,
	pub actual_type: VariableType<T>,
}

/// Describes the reason why the [`Interpreter::run`] has returned
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum InterpreterEndState<E: std::error::Error, T: DomainVariableType> {
	/// Either the program ran successfully or it terminated with an error
	Halted(Result<(), InterpreterError<E, T>>),
	/// The program did not end within the allowed number of instructions.
	/// [`Interpreter::run`] may be called again to resume execution
	Timeout,
}

#[allow(unused)]
impl<E: std::error::Error, T: DomainVariableType> InterpreterEndState<E, T> {
	pub fn is_ok(&self) -> bool {
		matches!(self, Self::Halted(Ok(())))
	}

	pub fn is_err(&self) -> bool {
		matches!(self, Self::Halted(Err(_)))
	}

	pub fn is_timeout(&self) -> bool {
		matches!(self, Self::Timeout)
	}

	pub fn unwrap_halted(self) -> Result<(), InterpreterError<E, T>> {
		match self {
			Self::Halted(r) => r,
			other => panic!("unwrap_halted called on a value of {other:?}"),
		}
	}

	pub fn unwrap_err(self) -> InterpreterError<E, T> {
		self.unwrap_halted().unwrap_err()
	}
}

#[derive(Clone, Debug)]
enum ArgumentValue<'a, T: DomainVariableValue + 'a> {
	Argument(VariableValue<'a, T>),
	Separator,
}

#[derive(Clone, Debug, Default)]
pub struct ReturnValue<'a, T: DomainVariableValue + 'a> {
	pub value: VariableValue<'a, T>,
	pub must_use: bool,
}

impl<'a, T: DomainVariableValue + 'a> ReturnValue<'a, T> {
	pub fn new(value: VariableValue<'a, T>, must_use: bool) -> Self {
		Self { value, must_use }
	}

	pub fn pure(value: VariableValue<'a, T>) -> Self {
		Self::new(value, true)
	}

	pub fn with_side_effect(value: VariableValue<'a, T>) -> Self {
		Self::new(value, false)
	}

	pub fn void() -> Self {
		Self::with_side_effect(VariableValue::Blank)
	}

	pub fn map_domain<U: DomainVariableValue + 'a>(
		self,
		map: impl FnOnce(T) -> U,
	) -> ReturnValue<'a, U> {
		ReturnValue::new(self.value.map_domain(map), self.must_use)
	}
}

pub trait InterpreterBackend {
	/// Type of the domain-specific error this backend can emit
	type Error: std::error::Error;

	/// Type of the domain-specific warning this backend can emit
	type Warning: std::error::Error;

	/// Type of domain-specific variable values used by this backend
	type Value: DomainVariableValue;

	/// Calls a function by name.
	/// Performs side effects of the function (if any) and returns the value returned by it.
	fn call_function<'a>(
		&mut self,
		_function_name: &str,
		_args: ArgumentStream<'a, '_, Self::Value>,
		_warnings: WarningSink<Self::Warning>,
	) -> Result<
		ReturnValue<'a, Self::Value>,
		FunctionCallError<Self::Error, <Self::Value as DomainVariableValue>::Type>,
	> {
		Err(FunctionCallError::FunctionDoesNotExist)
	}
}

pub struct Interpreter<'ast, T: InterpreterBackend + 'ast> {
	value_stack: InterpreterStack<VariableValueFrame<'ast, T::Value>>,
	instruction_stack: InterpreterStack<Instruction<'ast, T::Value>>,
	is_halted: bool,
	pub variable_pool: VariablePool<'ast, T::Value>,
	pub backend: T,
	warnings: Vec<InterpreterWarning<T::Warning>>,
}

impl<'ast, T: InterpreterBackend + 'ast> Interpreter<'ast, T> {
	pub const DEFAULT_VALUE_STACK_LIMIT: usize = 200;
	pub const DEFAULT_INSTRUCTION_STACK_LIMIT: usize = 200;

	pub fn new(ast: &'ast Module, backend: T) -> Self {
		Self {
			value_stack: InterpreterStack::new(Self::DEFAULT_VALUE_STACK_LIMIT),
			instruction_stack: InterpreterStack {
				values: vec![Instruction::Execute(&ast.0 .0)],
				max_height: Self::DEFAULT_INSTRUCTION_STACK_LIMIT,
			},
			is_halted: false,
			variable_pool: VariablePool::new(),
			backend,
			warnings: Vec::new(),
		}
	}

	pub fn _set_value_stack_limit(&mut self, new_limit: usize) {
		self.value_stack.max_height = new_limit;
	}

	pub fn _set_instruction_stack_limit(&mut self, new_limit: usize) {
		self.instruction_stack.max_height = new_limit;
	}

	/// Clears all warnings that have been emitted by the interpreter
	/// and iterates through them
	pub fn get_warnings(&mut self) -> impl Iterator<Item = InterpreterWarning<T::Warning>> + '_ {
		self.warnings.drain(..)
	}

	pub fn run(
		&mut self,
		mut max_iterations: u32,
	) -> InterpreterEndState<T::Error, <T::Value as DomainVariableValue>::Type> {
		if self.is_halted {
			return InterpreterEndState::Halted(Ok(()));
		}
		if max_iterations == 0 {
			return InterpreterEndState::Timeout;
		}
		while let Some(instruction) = self.instruction_stack.pop() {
			if let Err(err) = self.execute_instruction(instruction) {
				self.is_halted = true;
				return InterpreterEndState::Halted(Err(err));
			}
			max_iterations -= 1;
			if max_iterations == 0 {
				return InterpreterEndState::Timeout;
			}
		}
		self.is_halted = true;
		if !self.value_stack.is_empty() {
			return InterpreterEndState::Halted(Err(InterpreterError::InternalError));
		}
		InterpreterEndState::Halted(Ok(()))
	}

	fn execute_instruction(
		&mut self,
		instruction: Instruction<'ast, T::Value>,
	) -> Result<(), InterpreterError<T::Error, <T::Value as DomainVariableValue>::Type>> {
		match instruction {
			Instruction::Execute(statements) => {
				if let Some(first_statement) = statements.first() {
					// Execute the rest of the block later
					self.instruction_stack
						.push(Instruction::Execute(&statements[1..]))?;
					// Execute the first instruction now
					match first_statement {
						Statement::Expression(expr) => {
							// The expression is evaluated and its value is discarded
							self.instruction_stack.push(Instruction::Discard)?;
							self.instruction_stack.push(Instruction::Evaluate(expr))?;
						}
						Statement::Condition(cond) => {
							self.instruction_stack
								.push(Instruction::Conditional(cond.as_view()))?;
						}
					}
				}
			}
			Instruction::Conditional(condition) => {
				if let Some(first_case) = condition.cases.first() {
					// If condition is true, run its handler
					self.instruction_stack
						.push(Instruction::Execute(&first_case.handler.0))?;
					// If condition is false, execute the rest of the chain
					self.instruction_stack.push(Instruction::Conditional(
						ConditionStatementView {
							cases: &condition.cases[1..],
							tail: condition.tail,
						},
					))?;
					self.instruction_stack.push(Instruction::Switch)?;
					// Evaluate the condition
					self.instruction_stack
						.push(Instruction::Evaluate(&first_case.condition))?;
				} else if let Some(tail) = condition.tail {
					self.instruction_stack.push(Instruction::Execute(&tail.0))?;
				}
			}
			Instruction::Evaluate(expression) => match &*expression.expression {
				Expression::BlankLiteral => {
					self.construct_value(VariableValue::Blank, expression.loc.clone())?
				}
				Expression::IntLiteral(value) => {
					self.construct_value(VariableValue::Int(*value), expression.loc.clone())?
				}
				Expression::FloatLiteral(value) => {
					self.construct_value(VariableValue::Float(*value), expression.loc.clone())?
				}
				Expression::StringLiteral(value) => {
					self.construct_value(VariableValue::String(value), expression.loc.clone())?
				}
				Expression::Callback(body) => {
					self.construct_value(VariableValue::Callback(body), expression.loc.clone())?
				}
				Expression::VariableValue(name) => {
					match self.variable_pool.load(name.as_str()).cloned() {
						Some(value) => self.construct_value(value, expression.loc.clone())?,
						None => {
							return Err(InterpreterError::LogicError(
								Box::new(LogicError::VariableDoesNotExist(name.to_owned())),
								expression.loc.clone(),
							))
						}
					}
				}
				Expression::Assignment(expr) => {
					self.instruction_stack
						.push(Instruction::Store(&expr.left, expression.loc.clone()))?;
					self.instruction_stack
						.push(Instruction::Evaluate(&expr.right))?;
				}
				Expression::UnaryOperator(expr) => {
					self.instruction_stack.push(Instruction::UnaryOperator(
						expr.operator,
						expression.loc.clone(),
					))?;
					self.instruction_stack
						.push(Instruction::Evaluate(&expr.operand))?;
				}
				Expression::BinaryOperator(expr) => match expr.operator {
					BinaryOperator::And => {
						self.instruction_stack
							.push(Instruction::Repush(expression.loc.clone()))?;
						self.instruction_stack
							.push(Instruction::Evaluate(&expr.right))?;
						self.instruction_stack
							.push(Instruction::Push(VariableValueFrame {
								value: VariableValue::Bool(false),
								loc: expression.loc.clone(),
								must_use: true,
							}))?;
						self.instruction_stack.push(Instruction::Switch)?;
						self.instruction_stack
							.push(Instruction::Evaluate(&expr.left))?;
					}
					BinaryOperator::Or => {
						self.instruction_stack
							.push(Instruction::Repush(expression.loc.clone()))?;
						self.instruction_stack
							.push(Instruction::Push(VariableValueFrame {
								value: VariableValue::Bool(true),
								loc: expression.loc.clone(),
								must_use: true,
							}))?;
						self.instruction_stack
							.push(Instruction::Evaluate(&expr.right))?;
						self.instruction_stack.push(Instruction::Switch)?;
						self.instruction_stack
							.push(Instruction::Evaluate(&expr.left))?;
					}
					_ => {
						self.instruction_stack.push(Instruction::BinaryOperator(
							expr.operator,
							expression.loc.clone(),
						))?;
						self.instruction_stack
							.push(Instruction::Evaluate(&expr.right))?;
						self.instruction_stack
							.push(Instruction::Evaluate(&expr.left))?;
					}
				},
				Expression::Call(expr) => {
					self.instruction_stack
						.push(Instruction::Call(CallInstruction {
							loc: expression.loc.clone(),
							function_name: &expr.function,
							argument_expressions: &expr.arguments,
							argument_values: Vec::new(),
						}))?;
				}
			},
			Instruction::Discard => {
				let discarded_value = self
					.value_stack
					.pop()
					.ok_or(InterpreterError::InternalError)?;
				if discarded_value.must_use {
					self.emit_warning(WarningCode::DiscardedMustUse, discarded_value.loc);
				}
			}
			Instruction::Push(value) => {
				self.value_stack.push(value)?;
			}
			Instruction::Repush(loc) => {
				let value = self
					.value_stack
					.top_mut()
					.ok_or(InterpreterError::InternalError)?;
				value.loc = loc;
				value.must_use = true;
			}
			Instruction::Store(variable_name, loc) => {
				let value = self
					.value_stack
					.top_mut()
					.ok_or(InterpreterError::InternalError)?;
				let old_value = self.variable_pool.store(
					variable_name,
					VariableSlot {
						value: value.value.clone(),
						origin: VariableOrigin::Constructed(value.loc.clone()),
					},
				);
				value.must_use = false;
				value.loc = loc.clone();
				if old_value.is_some_and(|value| value.origin == VariableOrigin::Builtin) {
					self.emit_warning(
						WarningCode::OverwrittenBuiltin(variable_name.to_owned()),
						loc,
					);
				}
			}
			Instruction::Switch => {
				let condition = self
					.value_stack
					.pop()
					.ok_or(InterpreterError::InternalError)?;
				let else_body = self
					.instruction_stack
					.pop()
					.ok_or(InterpreterError::InternalError)?;
				let is_condition_true: bool = condition.value.try_into().map_err(|actual| {
					InterpreterError::LogicError(
						Box::new(LogicError::IllegalConditionType(actual)),
						condition.loc,
					)
				})?;
				if !is_condition_true {
					self.instruction_stack
						.pop()
						.ok_or(InterpreterError::InternalError)?;
					self.instruction_stack.push(else_body)?;
				}
			}
			Instruction::Call(mut instruction) => {
				if let Some(argument) = instruction.argument_expressions.first() {
					instruction.argument_expressions = &instruction.argument_expressions[1..];
					match argument {
						ArgumentItem::Argument(expression) => {
							self.instruction_stack
								.push(Instruction::PassArgument(instruction))?;
							self.instruction_stack
								.push(Instruction::Evaluate(expression))?;
						}
						ArgumentItem::Separator => {
							instruction.argument_values.push(ArgumentValue::Separator);
							self.instruction_stack
								.push(Instruction::Call(instruction))?;
						}
					}
				} else {
					let warning_sink = WarningSink {
						warning_stash: &mut self.warnings,
						loc: instruction.loc.clone(),
					};
					match self.backend.call_function(
						instruction.function_name,
						ArgumentStream(&instruction.argument_values),
						warning_sink,
					) {
						Ok(returned_value) => {
							self.value_stack.push(VariableValueFrame {
								value: returned_value.value,
								loc: instruction.loc,
								must_use: returned_value.must_use,
							})?;
						}
						Err(err) => {
							return Err(InterpreterError::LogicError(
								Box::new(LogicError::FunctionCall(
									err,
									instruction.function_name.to_owned(),
								)),
								instruction.loc,
							))
						}
					}
				}
			}
			Instruction::PassArgument(mut instruction) => {
				let argument_value = self
					.value_stack
					.pop()
					.ok_or(InterpreterError::InternalError)?;
				instruction
					.argument_values
					.push(ArgumentValue::Argument(argument_value.value));
				self.instruction_stack
					.push(Instruction::Call(instruction))?;
			}
			Instruction::UnaryOperator(operator, loc) => {
				let operand = self
					.value_stack
					.pop()
					.ok_or(InterpreterError::InternalError)?;
				let value = match operator {
					UnaryOperator::Not => match operand.value {
						VariableValue::Bool(value) => VariableValue::Bool(!value),
						_ => {
							return Err(InterpreterError::LogicError(
								Box::new(LogicError::IllegalTypeInUnaryOperator(
									operator,
									operand.value.get_type(),
								)),
								loc,
							))
						}
					},
					UnaryOperator::Plus => match operand.value {
						VariableValue::Int(_) | VariableValue::Float(_) => operand.value,
						_ => {
							return Err(InterpreterError::LogicError(
								Box::new(LogicError::IllegalTypeInUnaryOperator(
									operator,
									operand.value.get_type(),
								)),
								loc,
							))
						}
					},
					UnaryOperator::Minus => match operand.value {
						VariableValue::Int(value) => {
							VariableValue::Int(value.checked_neg().ok_or_else(|| {
								InterpreterError::LogicError(
									Box::new(LogicError::ArithmeticOverflow),
									loc.clone(),
								)
							})?)
						}
						VariableValue::Float(value) => VariableValue::Float(-value),
						_ => {
							return Err(InterpreterError::LogicError(
								Box::new(LogicError::IllegalTypeInUnaryOperator(
									operator,
									operand.value.get_type(),
								)),
								loc,
							))
						}
					},
				};
				self.construct_value(value, loc)?;
			}
			Instruction::BinaryOperator(operator, loc) => {
				let right = self
					.value_stack
					.pop()
					.ok_or(InterpreterError::InternalError)?;
				let left = self
					.value_stack
					.pop()
					.ok_or(InterpreterError::InternalError)?;
				let value = match operator {
					BinaryOperator::Plus => {
						match NumericPair::convert_from(&left.value, &right.value) {
							Some(NumericPair::Int(left, right)) => {
								VariableValue::Int(left.checked_add(right).ok_or_else(|| {
									InterpreterError::LogicError(
										Box::new(LogicError::ArithmeticOverflow),
										loc.clone(),
									)
								})?)
							}
							Some(NumericPair::Float(left, right)) => {
								VariableValue::Float(left + right)
							}
							None => {
								return Err(InterpreterError::LogicError(
									Box::new(LogicError::IllegalTypeInBinaryOperator(
										operator,
										left.value.get_type(),
										right.value.get_type(),
									)),
									loc,
								))
							}
						}
					}
					BinaryOperator::Minus => {
						match NumericPair::convert_from(&left.value, &right.value) {
							Some(NumericPair::Int(left, right)) => {
								VariableValue::Int(left.checked_sub(right).ok_or_else(|| {
									InterpreterError::LogicError(
										Box::new(LogicError::ArithmeticOverflow),
										loc.clone(),
									)
								})?)
							}
							Some(NumericPair::Float(left, right)) => {
								VariableValue::Float(left - right)
							}
							None => {
								return Err(InterpreterError::LogicError(
									Box::new(LogicError::IllegalTypeInBinaryOperator(
										operator,
										left.value.get_type(),
										right.value.get_type(),
									)),
									loc,
								))
							}
						}
					}
					BinaryOperator::Mul => {
						match NumericPair::convert_from(&left.value, &right.value) {
							Some(NumericPair::Int(left, right)) => {
								VariableValue::Int(left.checked_mul(right).ok_or_else(|| {
									InterpreterError::LogicError(
										Box::new(LogicError::ArithmeticOverflow),
										loc.clone(),
									)
								})?)
							}
							Some(NumericPair::Float(left, right)) => {
								VariableValue::Float(left * right)
							}
							None => {
								return Err(InterpreterError::LogicError(
									Box::new(LogicError::IllegalTypeInBinaryOperator(
										operator,
										left.value.get_type(),
										right.value.get_type(),
									)),
									loc,
								))
							}
						}
					}
					BinaryOperator::Div => {
						let left_value = match left.value {
							VariableValue::Int(val) => val as f32,
							VariableValue::Float(val) => val,
							_ => {
								return Err(InterpreterError::LogicError(
									Box::new(LogicError::IllegalTypeInBinaryOperator(
										operator,
										left.value.get_type(),
										right.value.get_type(),
									)),
									loc,
								))
							}
						};
						let right_value = match right.value {
							VariableValue::Int(val) => val as f32,
							VariableValue::Float(val) => val,
							_ => {
								return Err(InterpreterError::LogicError(
									Box::new(LogicError::IllegalTypeInBinaryOperator(
										operator,
										left.value.get_type(),
										right.value.get_type(),
									)),
									loc,
								))
							}
						};
						VariableValue::Float(left_value / right_value)
					}
					BinaryOperator::IntDiv => match (&left.value, &right.value) {
						(&VariableValue::Int(left_val), &VariableValue::Int(right_val)) => {
							VariableValue::Int(left_val.checked_div(right_val).ok_or_else(
								|| {
									InterpreterError::LogicError(
										Box::new(LogicError::ArithmeticOverflow),
										loc.clone(),
									)
								},
							)?)
						}
						_ => {
							return Err(InterpreterError::LogicError(
								Box::new(LogicError::IllegalTypeInBinaryOperator(
									operator,
									left.value.get_type(),
									right.value.get_type(),
								)),
								loc,
							))
						}
					},
					BinaryOperator::Modulo => match (&left.value, &right.value) {
						(&VariableValue::Int(left_val), &VariableValue::Int(right_val)) => {
							VariableValue::Int(left_val.checked_rem(right_val).ok_or_else(
								|| {
									InterpreterError::LogicError(
										Box::new(LogicError::ArithmeticOverflow),
										loc.clone(),
									)
								},
							)?)
						}
						_ => {
							return Err(InterpreterError::LogicError(
								Box::new(LogicError::IllegalTypeInBinaryOperator(
									operator,
									left.value.get_type(),
									right.value.get_type(),
								)),
								loc,
							))
						}
					},
					BinaryOperator::Pow => {
						match NumericPair::convert_from(&left.value, &right.value) {
							Some(NumericPair::Int(left, right)) => {
								let right = right.try_into().map_err(|_| {
									InterpreterError::LogicError(
										Box::new(LogicError::NegativeIntPow),
										loc.clone(),
									)
								})?;
								VariableValue::Int(left.checked_pow(right).ok_or_else(|| {
									InterpreterError::LogicError(
										Box::new(LogicError::ArithmeticOverflow),
										loc.clone(),
									)
								})?)
							}
							Some(NumericPair::Float(left, right)) => {
								VariableValue::Float(left.powf(right))
							}
							None => {
								return Err(InterpreterError::LogicError(
									Box::new(LogicError::IllegalTypeInBinaryOperator(
										operator,
										left.value.get_type(),
										right.value.get_type(),
									)),
									loc,
								))
							}
						}
					}
					BinaryOperator::Equals => {
						match NumericPair::convert_from(&left.value, &right.value) {
							Some(NumericPair::Int(left, right)) => {
								VariableValue::Bool(left == right)
							}
							Some(NumericPair::Float(left, right)) => {
								VariableValue::Bool(left == right)
							}
							None => {
								return Err(InterpreterError::LogicError(
									Box::new(LogicError::IllegalTypeInBinaryOperator(
										operator,
										left.value.get_type(),
										right.value.get_type(),
									)),
									loc,
								))
							}
						}
					}
					BinaryOperator::NotEquals => {
						match NumericPair::convert_from(&left.value, &right.value) {
							Some(NumericPair::Int(left, right)) => {
								VariableValue::Bool(left != right)
							}
							Some(NumericPair::Float(left, right)) => {
								VariableValue::Bool(left != right)
							}
							None => {
								return Err(InterpreterError::LogicError(
									Box::new(LogicError::IllegalTypeInBinaryOperator(
										operator,
										left.value.get_type(),
										right.value.get_type(),
									)),
									loc,
								))
							}
						}
					}
					BinaryOperator::Less => {
						match NumericPair::convert_from(&left.value, &right.value) {
							Some(NumericPair::Int(left, right)) => {
								VariableValue::Bool(left < right)
							}
							Some(NumericPair::Float(left, right)) => {
								VariableValue::Bool(left < right)
							}
							None => {
								return Err(InterpreterError::LogicError(
									Box::new(LogicError::IllegalTypeInBinaryOperator(
										operator,
										left.value.get_type(),
										right.value.get_type(),
									)),
									loc,
								))
							}
						}
					}
					BinaryOperator::Greater => {
						match NumericPair::convert_from(&left.value, &right.value) {
							Some(NumericPair::Int(left, right)) => {
								VariableValue::Bool(left > right)
							}
							Some(NumericPair::Float(left, right)) => {
								VariableValue::Bool(left > right)
							}
							None => {
								return Err(InterpreterError::LogicError(
									Box::new(LogicError::IllegalTypeInBinaryOperator(
										operator,
										left.value.get_type(),
										right.value.get_type(),
									)),
									loc,
								))
							}
						}
					}
					BinaryOperator::LessEquals => {
						match NumericPair::convert_from(&left.value, &right.value) {
							Some(NumericPair::Int(left, right)) => {
								VariableValue::Bool(left <= right)
							}
							Some(NumericPair::Float(left, right)) => {
								VariableValue::Bool(left <= right)
							}
							None => {
								return Err(InterpreterError::LogicError(
									Box::new(LogicError::IllegalTypeInBinaryOperator(
										operator,
										left.value.get_type(),
										right.value.get_type(),
									)),
									loc,
								))
							}
						}
					}
					BinaryOperator::GreaterEquals => {
						match NumericPair::convert_from(&left.value, &right.value) {
							Some(NumericPair::Int(left, right)) => {
								VariableValue::Bool(left >= right)
							}
							Some(NumericPair::Float(left, right)) => {
								VariableValue::Bool(left >= right)
							}
							None => {
								return Err(InterpreterError::LogicError(
									Box::new(LogicError::IllegalTypeInBinaryOperator(
										operator,
										left.value.get_type(),
										right.value.get_type(),
									)),
									loc,
								))
							}
						}
					}
					BinaryOperator::And | BinaryOperator::Or => {
						// These operators should be handled by short-circuiting
						// and should never make it here
						return Err(InterpreterError::InternalError);
					}
				};
				self.construct_value(value, loc)?;
			}
		}
		Ok(())
	}

	fn construct_value(
		&mut self,
		value: VariableValue<'ast, T::Value>,
		loc: Range<SourceLocation>,
	) -> Result<(), InterpreterStackOverflow> {
		self.value_stack.push(VariableValueFrame {
			value,
			loc,
			must_use: true,
		})
	}

	fn emit_warning(&mut self, warning_code: WarningCode<T::Warning>, loc: Range<SourceLocation>) {
		self.warnings.push(InterpreterWarning { warning_code, loc });
	}
}

#[derive(Clone, Copy, Debug, Default)]
struct InterpreterStackOverflow;

#[derive(Clone, Debug)]
struct InterpreterStack<T> {
	values: Vec<T>,
	max_height: usize,
}

impl<T> InterpreterStack<T> {
	fn new(max_height: usize) -> Self {
		Self {
			values: Vec::new(),
			max_height,
		}
	}

	fn is_empty(&self) -> bool {
		self.values.is_empty()
	}

	fn push(&mut self, value: T) -> Result<(), InterpreterStackOverflow> {
		if self.values.len() >= self.max_height {
			Err(InterpreterStackOverflow)
		} else {
			self.values.push(value);
			Ok(())
		}
	}

	fn pop(&mut self) -> Option<T> {
		self.values.pop()
	}

	fn top_mut(&mut self) -> Option<&mut T> {
		self.values.last_mut()
	}
}

#[derive(Clone, Debug)]
struct VariableValueFrame<'ast, T: DomainVariableValue + 'ast> {
	value: VariableValue<'ast, T>,
	loc: Range<SourceLocation>,
	must_use: bool,
}

#[derive(Clone, PartialEq, Eq, Debug, Default)]
pub enum VariableOrigin {
	#[default]
	Builtin,
	Constructed(Range<SourceLocation>),
}

#[derive(Clone, Debug)]
pub struct VariableSlot<'ast, T: DomainVariableValue + 'ast> {
	pub value: VariableValue<'ast, T>,
	pub origin: VariableOrigin,
}

impl<'ast, T: DomainVariableValue + 'ast> VariableSlot<'ast, T> {
	pub fn builtin(value: VariableValue<'ast, T>) -> Self {
		Self {
			value,
			origin: VariableOrigin::Builtin,
		}
	}
}

#[derive(Clone, Debug)]
enum Instruction<'ast, T: DomainVariableValue + 'ast> {
	/// Executes a block of statements.
	Execute(&'ast [Statement]),
	/// Executes a conditional statement.
	Conditional(ConditionStatementView<'ast>),
	/// Evaluates an expression and pushes the result onto the stack.
	Evaluate(&'ast ExpressionNode),
	/// Pushes a value onto the stack
	Push(VariableValueFrame<'ast, T>),
	/// Pops the top value from the stack and discards it.
	/// A warning is emitted if [`VariableValueFrame::must_use`] is true.
	///
	/// Fails if the stack is empty.
	Discard,
	/// Updates the top value in the stack with a new source location
	/// and sets its [`VariableValueFrame::must_use`] to true.
	///
	/// Fails if the stack is empty.
	Repush(Range<SourceLocation>),
	/// Pops the top value from the stack. If it is true, cancels the top
	/// instruction in the instruction stack. If it is false, cancels the second-from-top
	/// instruction in the instruction stack.
	///
	/// The intended use is to execute instructions conditionally.
	///
	/// Fails if the popped value is not boolean.
	///
	/// Fails if there are not enough instructions on the stack.
	/// ```
	/// instruction_stack.push(action_if_true);
	/// instruction_stack.push(action_if_false);
	/// instruction_stack.push(Instruction::Switch);
	/// ```
	Switch,
	/// Reads the top value from the stack and stores it in a variable
	/// with the given name. The value is left on the stack.
	///
	/// Fails if the stack is empty.
	Store(&'ast str, Range<SourceLocation>),
	/// Pops the top value from the stack and evaluates the operation
	/// associated with the given operator, then pushes the result onto the stack.
	///
	/// Fails if the stack is empty.
	UnaryOperator(UnaryOperator, Range<SourceLocation>),
	/// Pops the top two values from the stack and evaluates the operation
	/// associated with the given operator, then pushes the result onto the stack.
	///
	/// Fails if the stack does not contain at least two values.
	BinaryOperator(BinaryOperator, Range<SourceLocation>),
	/// Calls a function with provided arguments.
	Call(CallInstruction<'ast, T>),
	/// Pops a value from the stack and adds it to the argument list of this
	/// [`CallInstruction`], then pushes a [`Call`](Instruction::Call) instruction
	/// so that the function may be called.
	PassArgument(CallInstruction<'ast, T>),
}

#[derive(Clone, Debug)]
struct CallInstruction<'ast, T: DomainVariableValue + 'ast> {
	loc: Range<SourceLocation>,
	function_name: &'ast str,
	argument_expressions: &'ast [ArgumentItem],
	argument_values: Vec<ArgumentValue<'ast, T>>,
}

impl<E: std::error::Error, T: DomainVariableType> From<InterpreterStackOverflow>
	for InterpreterError<E, T>
{
	fn from(_: InterpreterStackOverflow) -> Self {
		Self::StackOverflow
	}
}
