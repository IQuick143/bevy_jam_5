use super::{ast::*, lex::NeverTag, values::*, SourceLocation};
use bevy::utils::HashMap;
use std::ops::Range;

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum InterpreterError {
	/// One of the interpreter stacks has overflown.
	StackOverflow,
	/// An error bound to a specific place in the source code.
	LogicError(LogicError, Range<SourceLocation>),
}

impl std::error::Error for InterpreterError {}

impl std::fmt::Display for InterpreterError {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::StackOverflow => f.write_str("Interpreter stack overflow"),
			Self::LogicError(err, loc) => {
				f.write_fmt(format_args!("{}..{}: {err}", loc.start, loc.end))
			}
		}
	}
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum LogicError {
	/// An expression used in conditional context (condition of if statement
	/// or short-circuiting boolean operator) evaluated to type other than boolean.
	IllegalConditionType(VariableType),
	/// A variable was read but no variable of that name exists.
	VariableDoesNotExist(String),
	/// A unary operator was called on an unsupported type.
	IllegalTypeInUnaryOperator(UnaryOperator, VariableType),
	/// A binary operator was called on unsupported types.
	IllegalTypeInBinaryOperator(BinaryOperator, VariableType, VariableType),
	/// Integer arithmetic has resulted in a value that cannot be represented.
	ArithmeticOverflow,
	/// Integer was raised to a negative integer power.
	NegativeIntPow,
	/// Error caused by a function call.
	FunctionCall(FunctionCallError, String),
}

impl std::fmt::Display for LogicError {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::IllegalConditionType(ty) => f.write_fmt(format_args!(
				"value of invalid type {ty} passed to a condition"
			)),
			Self::VariableDoesNotExist(name) => {
				f.write_fmt(format_args!("variable {name} foes not exist"))
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
pub enum FunctionCallError {
	/// No function of the given name exists
	FunctionDoesNotExist,
	/// Incorrect number of arguments passed to the function
	BadArgumentCount,
	/// Incorrect types of arguments
	TypeError(VariableType),
	/// Integer arithmetic has resulted in a value that cannot be represented
	ArithmeticOverflow,
	/// Domain-specific error defined by the function implementations.
	/// Implementations should provide error details themselves.
	Domain,
}

impl std::error::Error for FunctionCallError {}

impl std::fmt::Display for FunctionCallError {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::FunctionDoesNotExist => f.write_str("function does not exist"),
			Self::BadArgumentCount => f.write_str("incorrect number of arguments"),
			Self::TypeError(ty) => f.write_fmt(format_args!("argument has incorrect type {ty}")),
			Self::ArithmeticOverflow => f.write_str("integer arithmetic overflow"),
			Self::Domain => f.write_str("domain-specific error"),
		}
	}
}

#[derive(Clone, Debug)]
pub struct InterpreterWarning {
	warning_code: WarningCode,
	loc: Range<SourceLocation>,
}

impl std::fmt::Display for InterpreterWarning {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		f.write_fmt(format_args!(
			"{}..{}: {}",
			self.loc.start, self.loc.end, self.warning_code
		))
	}
}

#[derive(Clone, Debug)]
pub enum WarningCode {
	/// The value of an expression without side effects was not used
	DiscardedMustUse,
	/// A built-in variable was overwritten
	OverwrittenBuiltin(String),
}

impl std::fmt::Display for WarningCode {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::DiscardedMustUse => f.write_str("Discarded value that must be used"),
			Self::OverwrittenBuiltin(name) => {
				f.write_fmt(format_args!("Assignment to builtin variable {name}"))
			}
		}
	}
}

/// Describes the reason why the [`Interpreter::run`] has returned
#[derive(Clone, Debug)]
pub enum InterpreterEndState {
	/// Either the program ran successfully or it terminated with an error
	Halted(Result<(), InterpreterError>),
	/// The program did not end within the allowed number of instructions.
	/// [`Interpreter::run`] may be called again to resume execution
	Timeout,
}

impl InterpreterEndState {
	pub fn is_ok(&self) -> bool {
		matches!(self, Self::Halted(Ok(())))
	}

	pub fn is_err(&self) -> bool {
		matches!(self, Self::Halted(Err(_)))
	}

	pub fn is_timeout(&self) -> bool {
		matches!(self, Self::Timeout)
	}

	pub fn unwrap_err(self) -> InterpreterError {
		match self {
			Self::Halted(Err(err)) => err,
			other => panic!("unwrap_err called on a value of {other:?}"),
		}
	}
}

#[derive(Clone, Debug)]
pub enum ArgumentValue<'a> {
	Argument(VariableValue<'a>),
	Separator,
}

pub struct ReturnValue<'a> {
	pub value: VariableValue<'a>,
	pub must_use: bool,
}

impl<'a> ReturnValue<'a> {
	pub fn new(value: VariableValue<'a>, must_use: bool) -> Self {
		Self { value, must_use }
	}

	pub fn pure(value: VariableValue<'a>) -> Self {
		Self::new(value, true)
	}

	pub fn with_side_effect(value: VariableValue<'a>) -> Self {
		Self::new(value, false)
	}
}

pub trait InterpreterBackend {
	/// Calls a function by name.
	/// Performs side effects of the function (if any) and returns the value returned by it.
	fn call_function<'a>(
		&mut self,
		function_name: &str,
		args: &[ArgumentValue<'a>],
	) -> Result<ReturnValue<'a>, FunctionCallError> {
		Err(FunctionCallError::FunctionDoesNotExist)
	}
}

pub struct Interpreter<'ast, T: InterpreterBackend + 'ast> {
	value_stack: InterpreterStack<VariableValueFrame<'ast>>,
	instruction_stack: InterpreterStack<Instruction<'ast>>,
	is_halted: bool,
	pub variable_pool: HashMap<&'ast str, VariableSlot<'ast>>,
	pub backend: T,
	pub warnings: Vec<InterpreterWarning>,
}

impl<'ast, T: InterpreterBackend + 'ast> Interpreter<'ast, T> {
	pub const DEFAULT_VALUE_STACK_LIMIT: usize = 200;
	pub const DEFAULT_INSTRUCTION_STACK_LIMIT: usize = 200;

	pub fn new(ast: &'ast Module, backend: T) -> Self {
		Self {
			value_stack: InterpreterStack {
				values: Vec::new(),
				max_height: Self::DEFAULT_VALUE_STACK_LIMIT,
			},
			instruction_stack: InterpreterStack {
				values: vec![Instruction::Execute(&ast.0 .0)],
				max_height: Self::DEFAULT_INSTRUCTION_STACK_LIMIT,
			},
			is_halted: false,
			variable_pool: HashMap::new(),
			backend,
			warnings: Vec::new(),
		}
	}

	pub fn set_value_stack_limit(&mut self, new_limit: usize) {
		self.value_stack.max_height = new_limit;
	}

	pub fn set_instruction_stack_limit(&mut self, new_limit: usize) {
		self.instruction_stack.max_height = new_limit;
	}

	pub fn run(&mut self, mut max_iterations: u32) -> InterpreterEndState {
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
		assert!(self.value_stack.is_empty(), "Epilang program terminated, but the interpreter's stack is not empty (this is a bug in the interpreter)");
		self.is_halted = true;
		InterpreterEndState::Halted(Ok(()))
	}

	fn execute_instruction(
		&mut self,
		instruction: Instruction<'ast>,
	) -> Result<(), InterpreterError> {
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
				Expression::VariableValue(name) => match self.variable_pool.get(name.as_str()) {
					Some(value) => {
						self.construct_value(value.value.clone(), expression.loc.clone())?
					}
					None => {
						return Err(InterpreterError::LogicError(
							LogicError::VariableDoesNotExist(name.to_owned()),
							expression.loc.clone(),
						))
					}
				},
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
							.push(Instruction::Repush(expression.loc.clone()));
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
							.push(Instruction::Repush(expression.loc.clone()));
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
				let discarded_value = self.value_stack.expect_pop();
				if discarded_value.must_use {
					self.emit_warning(WarningCode::DiscardedMustUse, discarded_value.loc);
				}
			}
			Instruction::Push(value) => {
				self.value_stack.push(value)?;
			}
			Instruction::Repush(loc) => {
				let value = self.value_stack.expect_top_mut();
				value.loc = loc;
				value.must_use = true;
			}
			Instruction::Store(variable_name, loc) => {
				let value = self.value_stack.expect_top_mut();
				let old_value = self.variable_pool.insert(
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
				let condition = self.value_stack.expect_pop();
				let else_body = self.instruction_stack.expect_pop();
				let is_condition_true: bool = condition.value.try_into().map_err(|actual| {
					InterpreterError::LogicError(
						LogicError::IllegalConditionType(actual),
						condition.loc,
					)
				})?;
				if !is_condition_true {
					self.instruction_stack.expect_pop();
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
					match self
						.backend
						.call_function(instruction.function_name, &instruction.argument_values)
					{
						Ok(returned_value) => {
							self.value_stack.push(VariableValueFrame {
								value: returned_value.value,
								loc: instruction.loc,
								must_use: returned_value.must_use,
							})?;
						}
						Err(err) => {
							return Err(InterpreterError::LogicError(
								LogicError::FunctionCall(err, instruction.function_name.to_owned()),
								instruction.loc,
							))
						}
					}
				}
			}
			Instruction::PassArgument(mut instruction) => {
				let argument_value = self.value_stack.expect_pop();
				instruction
					.argument_values
					.push(ArgumentValue::Argument(argument_value.value));
				self.instruction_stack
					.push(Instruction::Call(instruction))?;
			}
			Instruction::UnaryOperator(operator, loc) => {
				let operand = self.value_stack.expect_pop();
				let value = match operator {
					UnaryOperator::Not => match operand.value {
						VariableValue::Bool(value) => VariableValue::Bool(!value),
						_ => {
							return Err(InterpreterError::LogicError(
								LogicError::IllegalTypeInUnaryOperator(
									operator,
									operand.value.get_type(),
								),
								loc,
							))
						}
					},
					UnaryOperator::Plus => match operand.value {
						VariableValue::Int(_) | VariableValue::Float(_) => operand.value,
						_ => {
							return Err(InterpreterError::LogicError(
								LogicError::IllegalTypeInUnaryOperator(
									operator,
									operand.value.get_type(),
								),
								loc,
							))
						}
					},
					UnaryOperator::Minus => match operand.value {
						VariableValue::Int(value) => {
							VariableValue::Int(value.checked_neg().ok_or_else(|| {
								InterpreterError::LogicError(
									LogicError::ArithmeticOverflow,
									loc.clone(),
								)
							})?)
						}
						VariableValue::Float(value) => VariableValue::Float(-value),
						_ => {
							return Err(InterpreterError::LogicError(
								LogicError::IllegalTypeInUnaryOperator(
									operator,
									operand.value.get_type(),
								),
								loc,
							))
						}
					},
				};
				self.construct_value(value, loc)?;
			}
			Instruction::BinaryOperator(operator, loc) => {
				let right = self.value_stack.expect_pop();
				let left = self.value_stack.expect_pop();
				let value = match operator {
					BinaryOperator::Plus => {
						match NumericPair::convert_from(&left.value, &right.value) {
							Some(NumericPair::Int(left, right)) => {
								VariableValue::Int(left.checked_add(right).ok_or_else(|| {
									InterpreterError::LogicError(
										LogicError::ArithmeticOverflow,
										loc.clone(),
									)
								})?)
							}
							Some(NumericPair::Float(left, right)) => {
								VariableValue::Float(left + right)
							}
							None => {
								return Err(InterpreterError::LogicError(
									LogicError::IllegalTypeInBinaryOperator(
										operator,
										left.value.get_type(),
										right.value.get_type(),
									),
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
										LogicError::ArithmeticOverflow,
										loc.clone(),
									)
								})?)
							}
							Some(NumericPair::Float(left, right)) => {
								VariableValue::Float(left - right)
							}
							None => {
								return Err(InterpreterError::LogicError(
									LogicError::IllegalTypeInBinaryOperator(
										operator,
										left.value.get_type(),
										right.value.get_type(),
									),
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
										LogicError::ArithmeticOverflow,
										loc.clone(),
									)
								})?)
							}
							Some(NumericPair::Float(left, right)) => {
								VariableValue::Float(left * right)
							}
							None => {
								return Err(InterpreterError::LogicError(
									LogicError::IllegalTypeInBinaryOperator(
										operator,
										left.value.get_type(),
										right.value.get_type(),
									),
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
									LogicError::IllegalTypeInBinaryOperator(
										operator,
										left.value.get_type(),
										right.value.get_type(),
									),
									loc,
								))
							}
						};
						let right_value = match right.value {
							VariableValue::Int(val) => val as f32,
							VariableValue::Float(val) => val,
							_ => {
								return Err(InterpreterError::LogicError(
									LogicError::IllegalTypeInBinaryOperator(
										operator,
										left.value.get_type(),
										right.value.get_type(),
									),
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
										LogicError::ArithmeticOverflow,
										loc.clone(),
									)
								},
							)?)
						}
						_ => {
							return Err(InterpreterError::LogicError(
								LogicError::IllegalTypeInBinaryOperator(
									operator,
									left.value.get_type(),
									right.value.get_type(),
								),
								loc,
							))
						}
					},
					BinaryOperator::Modulo => match (&left.value, &right.value) {
						(&VariableValue::Int(left_val), &VariableValue::Int(right_val)) => {
							VariableValue::Int(left_val.checked_rem(right_val).ok_or_else(
								|| {
									InterpreterError::LogicError(
										LogicError::ArithmeticOverflow,
										loc.clone(),
									)
								},
							)?)
						}
						_ => {
							return Err(InterpreterError::LogicError(
								LogicError::IllegalTypeInBinaryOperator(
									operator,
									left.value.get_type(),
									right.value.get_type(),
								),
								loc,
							))
						}
					},
					BinaryOperator::Pow => {
						match NumericPair::convert_from(&left.value, &right.value) {
							Some(NumericPair::Int(left, right)) => {
								let right = right.try_into().map_err(|_| {
									InterpreterError::LogicError(
										LogicError::NegativeIntPow,
										loc.clone(),
									)
								})?;
								VariableValue::Int(left.checked_pow(right).ok_or_else(|| {
									InterpreterError::LogicError(
										LogicError::ArithmeticOverflow,
										loc.clone(),
									)
								})?)
							}
							Some(NumericPair::Float(left, right)) => {
								VariableValue::Float(left.powf(right))
							}
							None => {
								return Err(InterpreterError::LogicError(
									LogicError::IllegalTypeInBinaryOperator(
										operator,
										left.value.get_type(),
										right.value.get_type(),
									),
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
									LogicError::IllegalTypeInBinaryOperator(
										operator,
										left.value.get_type(),
										right.value.get_type(),
									),
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
									LogicError::IllegalTypeInBinaryOperator(
										operator,
										left.value.get_type(),
										right.value.get_type(),
									),
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
									LogicError::IllegalTypeInBinaryOperator(
										operator,
										left.value.get_type(),
										right.value.get_type(),
									),
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
									LogicError::IllegalTypeInBinaryOperator(
										operator,
										left.value.get_type(),
										right.value.get_type(),
									),
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
									LogicError::IllegalTypeInBinaryOperator(
										operator,
										left.value.get_type(),
										right.value.get_type(),
									),
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
									LogicError::IllegalTypeInBinaryOperator(
										operator,
										left.value.get_type(),
										right.value.get_type(),
									),
									loc,
								))
							}
						}
					}
					BinaryOperator::And | BinaryOperator::Or => {
						panic!("Binary operator {operator:?} was handled as a typical operator instead of short-circuiting (this is a bug in the interpreter)");
					}
				};
				self.construct_value(value, loc)?;
			}
		}
		Ok(())
	}

	fn construct_value(
		&mut self,
		value: VariableValue<'ast>,
		loc: Range<SourceLocation>,
	) -> Result<(), InterpreterStackOverflow> {
		self.value_stack.push(VariableValueFrame {
			value,
			loc,
			must_use: true,
		})
	}

	fn emit_warning(&mut self, warning_code: WarningCode, loc: Range<SourceLocation>) {
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

	fn expect_pop(&mut self) -> T {
		self.pop().expect(
			"Interpreter stack is empty but it should not be (this is a bug in the interpreter)",
		)
	}

	fn expect_top_mut(&mut self) -> &mut T {
		self.values.last_mut().expect(
			"Interpreter stack is empty but it should not be (this is a bug in the interpreter)",
		)
	}
}

#[derive(Clone, Debug)]
struct VariableValueFrame<'ast> {
	value: VariableValue<'ast>,
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
pub struct VariableSlot<'ast> {
	pub value: VariableValue<'ast>,
	pub origin: VariableOrigin,
}

impl<'ast> VariableSlot<'ast> {
	pub fn builtin(value: VariableValue<'ast>) -> Self {
		Self {
			value,
			origin: VariableOrigin::Builtin,
		}
	}
}

#[derive(Clone, Debug)]
enum Instruction<'ast> {
	/// Executes a block of statements.
	Execute(&'ast [Statement]),
	/// Executes a conditional statement.
	Conditional(ConditionStatementView<'ast>),
	/// Evaluates an expression and pushes the result onto the stack.
	Evaluate(&'ast ExpressionNode),
	/// Pushes a value onto the stack
	Push(VariableValueFrame<'ast>),
	/// Pops the top value from the stack and discards it.
	/// A warning is emitted if [`VariableValueFrame::must_use`] is true.
	///
	/// Panics if the stack is empty.
	Discard,
	/// Updates the top value in the stack with a new source location
	/// and sets its [`VariableValueFrame::must_use`] to true.
	///
	/// Panics if the stack is empty.
	Repush(Range<SourceLocation>),
	/// Pops the top value from the stack. If it is true, cancels the top
	/// instruction in the instruction stack. If it is false, cancels the second-from-top
	/// instruction in the instruction stack.
	///
	/// The intended use is to execute instructions conditionally.
	///
	/// Fails if the popped value is not boolean.
	///
	/// Panics if there are not enough instructions on the stack.
	/// ```
	/// instruction_stack.push(action_if_true);
	/// instruction_stack.push(action_if_false);
	/// instruction_stack.push(Instruction::Switch);
	/// ```
	Switch,
	/// Reads the top value from the stack and stores it in a variable
	/// with the given name. The value is left on the stack.
	///
	/// Panics if the stack is empty.
	Store(&'ast str, Range<SourceLocation>),
	/// Pops the top value from the stack and evaluates the operation
	/// associated with the given operator, then pushes the result onto the stack.
	///
	/// Panics if the stack is empty.
	UnaryOperator(UnaryOperator, Range<SourceLocation>),
	/// Pops the top two values from the stack and evaluates the operation
	/// associated with the given operator, then pushes the result onto the stack.
	///
	/// Panics if the stack does not contain at least two values.
	BinaryOperator(BinaryOperator, Range<SourceLocation>),
	/// Calls a function with provided arguments.
	Call(CallInstruction<'ast>),
	/// Pops a value from the stack and adds it to the argument list of this
	/// [`CallInstruction`], then pushes a [`Call`](Instruction::Call) instruction
	/// so that the function may be called.
	PassArgument(CallInstruction<'ast>),
}

#[derive(Clone, Debug)]
struct CallInstruction<'ast> {
	loc: Range<SourceLocation>,
	function_name: &'ast str,
	argument_expressions: &'ast [ArgumentItem],
	argument_values: Vec<ArgumentValue<'ast>>,
}

impl From<InterpreterStackOverflow> for InterpreterError {
	fn from(_: InterpreterStackOverflow) -> Self {
		Self::StackOverflow
	}
}
