use super::{
	ast::{Expression, ExpressionContent, OperationExpression, Sequence},
	SourceLocation,
};
use bevy::utils::HashMap;
use std::{borrow::Borrow, cell::RefCell, ops::Range, rc::Rc};

trait DynamicVariableValue {}

enum VariableValue {
	Blank,
	Int(i32),
	Float(f32),
	Dynamic(Box<dyn DynamicVariableValue>),
}

enum VariablePathSegment<'ast> {
	Method(&'ast str),
	Index(i32),
}

struct VariablePath<'ast> {
	variable: &'ast str,
	indirections: Vec<VariablePathSegment<'ast>>,
}

enum InterpreterValue<'ast> {
	Unit,
	Monad(Option<VariableValue>),
	Variable(VariableValue),
	MethodName(&'ast str),
	Path(VariablePath<'ast>),
}

struct VariableSlot {
	value: Option<VariableValue>,
	declaration_loc: Range<SourceLocation>,
}

enum ContextFrameType {
	TopLevel,
	Sequence,
	MacroArgument,
}

struct ContextFrame {
	macro_parameters: Option<Vec<VariableValue>>,
	variables: HashMap<String, VariableSlot>,
	context_loc: Option<Range<SourceLocation>>,
	context_type: ContextFrameType,
}

impl ContextFrame {
	/// Creates a ContextFrame for the top level.
	pub fn new_top_level(lines: Option<Range<SourceLocation>>) -> Self {
		ContextFrame {
			macro_parameters: None,
			variables: HashMap::new(),
			context_loc: lines,
			context_type: ContextFrameType::TopLevel,
		}
	}

	/// Creates a ContextFrame for a sequence.
	pub fn new_sequence() -> Self {
		ContextFrame {
			macro_parameters: None,
			variables: HashMap::new(),
			context_loc: None,
			context_type: ContextFrameType::Sequence,
		}
	}

	/// Creates a ContextFrame for a macro.
	pub fn new_macro() -> Self {
		ContextFrame {
			macro_parameters: None,
			variables: HashMap::new(),
			context_loc: None,
			context_type: ContextFrameType::MacroArgument,
		}
	}
}

enum Operation<'expr> {
	/// Either expands an [`Expression`] into multiple [`Operation`]s on the stack or executes it completely and converts it to a value.
	Eval(&'expr Expression),
	/// Evaluates the given operation using top stack values
	EvalOperation(&'expr OperationExpression),
	/// Repeats a given [`Expression`] multiple times, changing the macro parameters.
	Iterate {
		remaining_steps: usize,
		macro_value: usize,
		inner_expression: &'expr Expression,
	},
	//ToRvalue,
	/// Pops and discards a value from the value stack
	DiscardValue,
	/// Pushes a new [`ContextFrame`]
	PushContext(ContextFrame),
	/// Pops a [`ContextFrame`]
	PopContext,
	/// Collects n values from the value stack into a single array value on the stack
	Collect(usize),
	/// Assigns top value from the value stack to a variable with a given name in the
	LetAssign(String),
}

struct ContextStack(Vec<ContextFrame>);

struct OperationStack<'op>(Vec<Operation<'op>>);

impl<'op> OperationStack<'op> {
	fn push_expression(&mut self, expression: &'op Expression) {
		self.0.push(Operation::Eval(expression));
	}

	fn queue_expression(&mut self, expression: &'op Expression) {}

	fn queue_sequence(&mut self, sequence: &'op Sequence) {
		self.0.push(Operation::PopContext);
		if let Some(expression) = sequence.tail.borrow() {
			self.push_expression(expression);
		}
		for expression in sequence.statements.iter().rev() {
			//self.0.push(Operation::PopValue);
			self.push_expression(expression);
		}
		self.0
			.push(Operation::PushContext(ContextFrame::new_sequence()));
	}
}

struct ValueStack(Vec<VariableValue>);

impl ValueStack {
	pub fn push_int(&mut self, value: i32) {
		self.0.push(VariableValue::Int(value));
	}

	pub fn push_float(&mut self, value: f32) {
		self.0.push(VariableValue::Float(value));
	}

	pub fn push_string(&mut self, value: &str) {
		self.0.push(todo!());
	}

	pub fn push_blank(&mut self) {
		self.0.push(VariableValue::Blank);
	}
}

pub struct Interpreter<'ast> {
	script: &'ast Expression,
	/// Stack of [`ContextFrame`]'s
	context_stack: ContextStack,
	/// Stack of operations to be performed
	operation_stack: OperationStack<'ast>,
	/// Stack of values evaluated from expressions
	value_stack: ValueStack,
	/// Stack of values corresponding to $n registers
	dollar_stack: ValueStack,
}

impl<'ast> Interpreter<'ast> {
	pub fn new(script: &'ast Expression) -> Self {
		let mut interpreter = Interpreter {
			script,
			context_stack: ContextStack(Vec::new()),
			operation_stack: OperationStack(Vec::new()),
			value_stack: ValueStack(Vec::new()),
			dollar_stack: ValueStack(Vec::new()),
		};
		interpreter
			.context_stack
			.0
			.push(ContextFrame::new_top_level(Some(script.loc.clone())));
		interpreter.operation_stack.push_expression(script);
		interpreter
	}

	pub fn execute(&mut self, max_steps: usize) {
		let mut steps = 0;
		while self.operation_stack.0.len() > 0 && steps < max_steps {
			steps += 1;
			let Some(operation) = self.operation_stack.0.pop() else {
				break;
			};
			match operation {
				Operation::Eval(expresssion) => match expresssion.value.as_ref() {
					ExpressionContent::Sequence(sequence) => {
						self.operation_stack.queue_sequence(sequence)
					}
					ExpressionContent::Assignment(expression, expression1) => todo!(),
					ExpressionContent::IntLiteral(value) => self
						.value_stack
						.push_int((*value).try_into().unwrap_or(0) /* TODO */),
					ExpressionContent::FloatLiteral(value) => self.value_stack.push_float(*value),
					ExpressionContent::StringLiteral(value) => self.value_stack.push_string(value),
					ExpressionContent::Identifier(_) => {
						// TODO: Check if Identifier is a global macro and evaluate
						// TODO: Check if Identifier is a variable and evaluate it
						// TODO: Error handling otherwise
						todo!()
					}
					ExpressionContent::LetIdentifier(_) => {
						// TODO: Expand into a LetAssign operation and an expression
						todo!()
					}
					ExpressionContent::MacroParameter(_) => {
						// TODO: Evaluate to the given macro parameter value
						todo!()
					}
					ExpressionContent::Blank => self.value_stack.push_blank(),
					ExpressionContent::MacroCall(_, argument_list) => todo!(),
					ExpressionContent::MethodCall(expression, _, argument_list) => {
						todo!()
					}
					ExpressionContent::IndexAccess(expression, expression1) => todo!(),
					ExpressionContent::ArrayLiteral(vec) => todo!(),
					ExpressionContent::FillArrayLiteral(expression, expression1) => {
						// TODO: Expand into a macro call
						todo!()
					}
					ExpressionContent::Lambda(captures, expression) => {
						// TODO: Desugar
						// TODO: Account for vector iteration in step count
						for capture in captures.iter() {
							self.operation_stack.0.push(Operation::LetAssign());
						}
						todo!()
					}
					ExpressionContent::Operation(operation_expression) => {
						self.operation_stack
							.0
							.push(Operation::EvalOperation(operation_expression));
						match operation_expression {
							OperationExpression::Unary(_, expression) => {
								self.operation_stack.push_expression(expression)
							}
							OperationExpression::Binary(_, expression1, expression2) => {
								// NOTE: This is to evaluate left to right
								self.operation_stack.push_expression(expression2);
								self.operation_stack.push_expression(expression1);
							}
						}
					}
				},
				Operation::DiscardValue => {
					self.value_stack.0.pop(); // TODO: Check for [`None`]
				}
				Operation::PushContext(context_frame) => {
					self.context_stack.0.push(context_frame);
				}
				Operation::PopContext => {
					self.context_stack.0.pop(); // TODO: Check for [`None`]
				}
				Operation::Collect(_) => todo!(),
				Operation::Iterate {
					remaining_steps,
					macro_value,
					inner_expression,
				} => todo!(),
				Operation::EvalOperation(operation) => {
					match operation {
						OperationExpression::Unary(operator, expression) => {
							// TODO: Error handling for none.
							let input = self.value_stack.0.pop().unwrap();
							let result = match operator {
								super::ast::UnaryOperator::UnaryPlus => match &input {
									VariableValue::Blank => todo!(),
									VariableValue::Int(_) => input,
									VariableValue::Float(_) => input,
									VariableValue::Dynamic(_) => todo!(),
								},
								super::ast::UnaryOperator::UnaryMinus => match &input {
									VariableValue::Blank => todo!(),
									VariableValue::Int(value) => VariableValue::Int(-value),
									VariableValue::Float(value) => VariableValue::Float(-value),
									VariableValue::Dynamic(_) => todo!(),
								},
								super::ast::UnaryOperator::Not => todo!(),
							};
						}
						OperationExpression::Binary(binary_operator, expression, expression1) => {
							todo!()
						}
					}
				}
			}
		}
	}
}
