/// Module for the interpreter and language extensions (types and macros) and their related traits.
///
/// To the poor souls that lay their eyes upon this cursed piece of virtual estate,
/// I am sorry for the many many sins that have been commited and whose blood stains this soil.
/// I did what I had to, mistakes have been made, and those mistakes pushed the world into
/// an unrecoverable state of more and more poor decisions.
/// But I persevered, perhaps out of determination, perhaps out of folly, perhaps I shouldn't've.
/// Honestly, at least a part of it was just a morbid fascination with the fractal mess that
/// unfolded its many wings like a swarm of moths, and like a moth I too felt attracted to the
/// bright light shining from the burning remmants of good code practice whose ashes have
/// been burried here long ago.
///
/// ...and that is how we ended up trapped here, in this file, together,
/// yet separated by time.
///
/// Hence to bridge that gap I leave you this message: TODO: Refactor.
use super::{
	ast::{
		ArgumentList, BinaryOperator, Expression, ExpressionContent, OperationExpression, Sequence,
		UnaryOperator,
	},
	SourceLocation,
};
use epilang_macros::{ArrayFillLiteralMacro, ArrayLiteralMacro, ForMacro};
use std::{
	borrow::Borrow,
	cell::Cell,
	ops::{Deref, Range},
	rc::Rc,
};
use std::{collections::HashMap, ops::Mul};

pub trait DynamicVariableValue {
	/// Returns whether this variable is mutable (meaning that it does not currently have any borrows)
	fn is_mutable(&self) -> bool;

	/// Returns a reference to an internal value.
	fn index(&mut self, index: VariableValue) -> Option<ReferenceValue>; // TODO: Error handling

	/// Workaround for the object-insafety of [`Clone`]
	fn clone(&self) -> Box<dyn DynamicVariableValue>;

	/// Method that implements all the method calls on this object.
	fn call_method<'ast>(
		&mut self,
		name: &str,
		arguments: &'ast Option<ArgumentList>,
	) -> Option<Box<dyn Macro<'ast> + 'ast>>;
}

pub enum MacroReturn<'ast> {
	Operations(Vec<Operation<'ast>>),
	Return(StackValue),
}

type MacroConstructor<'ast> =
	dyn FnMut(Option<&'ast ArgumentList>) -> Option<Box<dyn Macro<'ast> + 'ast>> + 'ast; // TODO: Result type

pub trait Macro<'ast> {
	/// Perform one step of macro evaluation
	/// Input:
	///     On first call, the input value should be a [`Blank`](PlainValue::Blank), on subsequent calls, it is the value of the operations that have been previously emitted.
	/// Returns either:
	///     A list of [`Operation`]s that should be evaluated before the next call and *must* result in *one* value being pushed onto the stack, this value is then fed back into the macro next iteration.
	///     A return [`StackValue`] signaling the macro has finished and should no longer be bothered.
	fn poll(&mut self, input: StackValue) -> MacroReturn<'ast>;
}

#[derive(Clone, Copy, Debug)]
pub enum PlainValue {
	Blank,
	Bool(bool),
	Int(i32),
	Float(f32),
}

#[derive(Clone)]
pub struct ReferenceValue {
	counter: Rc<()>,
	value: Rc<Cell<VariableValue>>,
}

pub enum VariableValue {
	/// A plain value with copy semantics everywhere
	Plain(PlainValue),
	/// A reference to the inner part of some other value
	Dynamic(Box<dyn DynamicVariableValue>),
}

impl Clone for VariableValue {
	fn clone(&self) -> Self {
		match self {
			VariableValue::Plain(plain_value) => VariableValue::Plain(*plain_value),
			VariableValue::Dynamic(dynamic_variable_value) => {
				VariableValue::Dynamic(dynamic_variable_value.as_ref().clone())
			}
		}
	}
}

impl Default for VariableValue {
	fn default() -> Self {
		VariableValue::Plain(PlainValue::Blank)
	}
}

#[derive(Clone)]
pub enum StackValue {
	Value(VariableValue),
	Reference(ReferenceValue),
}

trait CellValueExt {
	type Inner;
	/// Calls a function on the inner value, allowing the caller to extract data from the [`Cell`]
	fn introspect<U>(&self, func: impl FnOnce(&Self::Inner) -> U) -> U;
	/// Calls a function on the inner value, allowing the caller to extract data from the [`Cell`] and/or mutate the contents.
	fn introspect_mut<U>(&self, func: impl FnOnce(&mut Self::Inner) -> U) -> U;
}

impl<T: Default> CellValueExt for Cell<T> {
	type Inner = T;

	fn introspect<U>(&self, func: impl FnOnce(&Self::Inner) -> U) -> U {
		let temp = self.take();
		let return_value = func(&temp);
		self.set(temp);
		return_value
	}

	fn introspect_mut<U>(&self, mut func: impl FnOnce(&mut Self::Inner) -> U) -> U {
		let mut temp = self.take();
		let return_value = func(&mut temp);
		self.set(temp);
		return_value
	}
}

impl StackValue {
	/// Returns whether this value can be used as an lvalue
	fn assignable(&self) -> bool {
		match self {
			StackValue::Value(_) => false,
			StackValue::Reference(ReferenceValue { value, .. }) => {
				value.as_ref().introspect(|value| match value {
					VariableValue::Plain(plain_value) => true,
					VariableValue::Dynamic(dynamic_variable_value) => {
						dynamic_variable_value.is_mutable()
					}
				})
			}
		}
	}

	/// Assigns to this reference, assumes that [`Self::assignable`] returns true.
	fn assign(&self, new_value: VariableValue) {
		match self {
			StackValue::Value(_) => todo!("Error!"),
			StackValue::Reference(ReferenceValue { value, .. }) => {
				value.as_ref().replace(new_value);
			}
		}
	}

	/// Returns the internal value, cloning it if it's behind a reference
	fn to_value(self) -> VariableValue {
		match self {
			StackValue::Value(variable_value) => variable_value,
			StackValue::Reference(ReferenceValue { value, .. }) => {
				value.as_ref().introspect(|value| value.clone())
			}
		}
	}

	/// Returns whether the underlying value is a plain datatype that doesn't require clones
	fn is_plain(&self) -> bool {
		match self {
			StackValue::Value(variable_value) => match variable_value {
				VariableValue::Plain(plain_value) => true,
				VariableValue::Dynamic(dynamic_variable_value) => false,
			},
			StackValue::Reference(ReferenceValue { value, .. }) => {
				value.introspect(|value| match value {
					VariableValue::Plain(plain_value) => true,
					VariableValue::Dynamic(dynamic_variable_value) => false,
				})
			}
		}
	}

	fn index(&mut self, index_value: VariableValue) -> Option<ReferenceValue> {
		self.introspect_mut(|inner| match inner {
			VariableValue::Plain(plain_value) => None, // TODO: Error
			VariableValue::Dynamic(dynamic_variable_value) => {
				dynamic_variable_value.index(index_value)
			}
		})
	}
}

impl StackValue {
	fn introspect<U>(&self, func: impl FnOnce(&VariableValue) -> U) -> U {
		match self {
			StackValue::Value(variable_value) => func(variable_value),
			StackValue::Reference(reference_value) => reference_value.value.introspect(func),
		}
	}

	fn introspect_mut<U>(&mut self, mut func: impl FnOnce(&mut VariableValue) -> U) -> U {
		match self {
			StackValue::Value(variable_value) => func(variable_value),
			StackValue::Reference(reference_value) => reference_value.value.introspect_mut(func),
		}
	}
}

impl<T> From<T> for StackValue
where
	T: Into<VariableValue>,
{
	fn from(value: T) -> Self {
		StackValue::Value(value.into())
	}
}

impl From<ReferenceValue> for StackValue {
	fn from(reference: ReferenceValue) -> Self {
		StackValue::Reference(reference)
	}
}

impl From<i32> for VariableValue {
	fn from(value: i32) -> Self {
		VariableValue::Plain(PlainValue::Int(value))
	}
}

impl From<f32> for VariableValue {
	fn from(value: f32) -> Self {
		VariableValue::Plain(PlainValue::Float(value))
	}
}

impl From<bool> for VariableValue {
	fn from(value: bool) -> Self {
		VariableValue::Plain(PlainValue::Bool(value))
	}
}

struct VariableSlot {
	value: Rc<Cell<VariableValue>>,
	// Option is None for Dollar variables, TODO: Reconsider
	declaration_loc: Option<Range<SourceLocation>>,
}

impl VariableSlot {
	pub fn new() -> Self {
		VariableSlot {
			value: Rc::new(Cell::new(VariableValue::default())),
			declaration_loc: None, // TODO
		}
	}

	pub fn borrow(&self) -> ReferenceValue {
		ReferenceValue {
			counter: Rc::new(()),
			value: self.value.clone(),
		}
	}
}

enum ContextFrameType {
	TopLevel,
	Sequence,
	MacroArgument,
}

struct ContextFrame {
	variables: HashMap<String, VariableSlot>,
	context_loc: Option<Range<SourceLocation>>,
	context_type: ContextFrameType,
}

impl ContextFrame {
	/// Creates a ContextFrame for the top level.
	pub fn new_top_level(lines: Option<Range<SourceLocation>>) -> Self {
		ContextFrame {
			variables: HashMap::new(),
			context_loc: lines,
			context_type: ContextFrameType::TopLevel,
		}
	}

	/// Creates a ContextFrame for a sequence.
	pub fn new_sequence() -> Self {
		ContextFrame {
			variables: HashMap::new(),
			context_loc: None,
			context_type: ContextFrameType::Sequence,
		}
	}

	/// Creates a ContextFrame for a macro.
	pub fn new_macro() -> Self {
		ContextFrame {
			variables: HashMap::new(),
			context_loc: None,
			context_type: ContextFrameType::MacroArgument,
		}
	}
}

pub enum Instruction {
	// Literal values
	Push(VariableValue),
	PushBlank,
	// Value discarding
	/// Discard top value from stack
	DiscardValue,
	// Scope management
	/// Create a new scope
	EnterScope,
	/// Exit a scope
	ExitScope,
	/// Create a new dollar register
	PushDollar,
	/// Discard topmost dollar register
	PopDollar,
	// Value access
	/// Create a new variable, return nothing
	CreateVariable(String),
	/// Accesses variable and pushes its reference to stack
	AccessVariable(String),
	/// Accesses $n register and pushes the value to stack
	AccessMacroRegister(usize),
	/// Grab (L, R) (L from the top then R) from stack and perform assignment L = R, returning the new L value
	Assign,
	// Operations
	BinaryOperation(BinaryOperator),
	UnaryOperation(UnaryOperator),
	// Arrays
	/// Grab (A, I) (I from the top then A) from stack and calculate the indexed value A\[I\]
	Index,
}

pub enum Operation<'expr> {
	/// Either expands an [`Expression`] into sub-[`Operation`]s.
	Expand(&'expr Expression),
	/// An instruction to be executed
	Instruction(Instruction),
	/// A method to be called
	MethodCall(&'expr str, &'expr Option<ArgumentList>),
	/// A macro to poll for further operations
	MacroCallBack(Box<dyn Macro<'expr> + 'expr>),
}

impl<'a> From<Instruction> for Operation<'a> {
	fn from(value: Instruction) -> Self {
		Operation::Instruction(value)
	}
}

impl<'a> From<&'a Expression> for Operation<'a> {
	fn from(value: &'a Expression) -> Self {
		Operation::Expand(value)
	}
}

struct ContextStack(Vec<ContextFrame>);

impl ContextStack {
	pub fn pop(&mut self) {
		self.0.pop(); // TODO: Error handling
	}
}

struct OperationStack<'op>(Vec<Operation<'op>>);

impl<'op> OperationStack<'op> {
	fn push(&mut self, operation: impl Into<Operation<'op>>) {
		self.0.push(operation.into());
	}
}

struct ValueStack(Vec<StackValue>);

impl ValueStack {
	pub fn push(&mut self, value: impl Into<StackValue>) {
		self.0.push(value.into());
	}

	pub fn push_blank(&mut self) {
		self.0
			.push(StackValue::Value(VariableValue::Plain(PlainValue::Blank)));
	}

	pub fn pop(&mut self) -> Option<StackValue> {
		self.0.pop()
	}
}

struct DollarStack(Vec<VariableSlot>);

impl DollarStack {
	pub fn push_new(&mut self) {
		self.0.push(VariableSlot {
			value: Rc::new(Cell::new(VariableValue::default())),
			declaration_loc: None,
		}); // TODO: Error handling
	}

	pub fn pop(&mut self) {
		self.0.pop(); // TODO: Error handling
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
	dollar_stack: DollarStack,
	macro_library: HashMap<String, Box<MacroConstructor<'ast>>>,
}

impl<'ast> Interpreter<'ast> {
	pub fn new(
		script: &'ast Expression,
		macro_library: HashMap<String, Box<MacroConstructor<'ast>>>,
	) -> Self {
		let mut interpreter = Interpreter {
			script,
			context_stack: ContextStack(Vec::new()),
			operation_stack: OperationStack(Vec::new()),
			value_stack: ValueStack(Vec::new()),
			dollar_stack: DollarStack(Vec::new()),
			macro_library,
		};
		interpreter
			.context_stack
			.0
			.push(ContextFrame::new_top_level(Some(script.loc.clone())));
		interpreter.operation_stack.push(script);
		interpreter
	}

	pub fn halted(&self) -> bool {
		self.operation_stack.0.is_empty()
	}

	pub fn execute(&mut self, max_steps: usize) {
		let mut steps = 0;
		while !self.operation_stack.0.is_empty() && steps < max_steps {
			steps += 1;
			let Some(operation) = self.operation_stack.0.pop() else {
				break;
			};
			match operation {
				Operation::Expand(expresssion) => match expresssion.value.as_ref() {
					ExpressionContent::Sequence(sequence) => {
						// Remaining instructions go into the working stack in reverse order to be processed later
						self.operation_stack.push(Instruction::ExitScope);
						// The return value is either the tail expression or a Blank.
						if let Some(expression) = sequence.tail.borrow() {
							self.operation_stack.push(expression);
						} else {
							self.operation_stack.push(Instruction::PushBlank);
						}
						// Other expressions have their values discarded
						for expression in sequence.statements.iter().rev() {
							self.operation_stack.push(Instruction::DiscardValue);
							self.operation_stack.push(expression);
						}
						// Push a scope
						self.operation_stack.push(Instruction::EnterScope);
					}
					ExpressionContent::Assignment(lvalue, rvalue) => {
						self.operation_stack.push(Instruction::Assign);
						self.operation_stack.push(lvalue);
						self.operation_stack.push(rvalue);
					}
					ExpressionContent::IntLiteral(value) => {
						self.operation_stack.push(Instruction::Push(
							(*value).try_into().unwrap_or(i32::MAX).into(), // TODO: better handling
						));
					}
					ExpressionContent::FloatLiteral(value) => {
						self.operation_stack
							.push(Instruction::Push((*value).into()));
					}
					ExpressionContent::StringLiteral(value) => {
						self.operation_stack.push(Instruction::Push(todo!()));
					}
					ExpressionContent::Blank => {
						self.operation_stack.push(Instruction::PushBlank);
					}
					ExpressionContent::Identifier(identifier) => {
						if let Some(constructor) = self.macro_library.get_mut(identifier) {
							if let Some(macro_data) = constructor(None) {
								self.operation_stack
									.push(Operation::MacroCallBack(macro_data));
								self.operation_stack.push(Instruction::PushBlank);
							} else {
								todo!()
							}
						} else {
							self.operation_stack
								.push(Instruction::AccessVariable(identifier.clone()));
						}
					}
					ExpressionContent::LetIdentifier(identifier) => {
						// TODO: Check for macro name collisions
						self.operation_stack
							.push(Instruction::AccessVariable(identifier.clone()));
						self.operation_stack
							.push(Instruction::CreateVariable(identifier.clone()));
					}
					ExpressionContent::MacroParameter(value) => self
						.operation_stack
						.push(Instruction::AccessMacroRegister(*value as usize)),
					ExpressionContent::MacroCall(macro_name, argument_list) => {
						if let Some(constructor) = self.macro_library.get_mut(macro_name) {
							if let Some(macro_data) = constructor(Some(argument_list)) {
								self.operation_stack
									.push(Operation::MacroCallBack(macro_data));
								self.operation_stack.push(Instruction::PushBlank);
							} else {
								todo!()
							}
						} else {
							todo!()
						}
					}
					ExpressionContent::MethodCall(expression, method_name, argument_list) => {
						self.operation_stack
							.push(Operation::MethodCall(method_name, argument_list));
						self.operation_stack.push(expression);
					}
					ExpressionContent::IndexAccess(array_value, index_expr) => {
						self.operation_stack.push(Instruction::Index);
						self.operation_stack.push(index_expr);
						self.operation_stack.push(array_value);
					}
					ExpressionContent::ArrayLiteral(values) => {
						self.operation_stack.push(Operation::MacroCallBack(Box::new(
							ArrayLiteralMacro::<'ast>::new(values),
						)));
						self.operation_stack.push(Instruction::PushBlank);
					}
					ExpressionContent::FillArrayLiteral(inner_expression, count_expression) => {
						self.operation_stack.push(Operation::MacroCallBack(Box::new(
							ArrayFillLiteralMacro::<'ast>::new(inner_expression, count_expression),
						)));
						self.operation_stack.push(Instruction::PushBlank);
					}
					ExpressionContent::Lambda(bindings, expression) => {
						self.operation_stack.push(expression);
						for (i, variable) in bindings.iter().enumerate() {
							// Discard the implicit-return value of the assignment
							self.operation_stack.push(Instruction::DiscardValue);
							// Assign
							self.operation_stack.push(Instruction::Assign);
							// Access it to get an lvalue
							self.operation_stack
								.push(Instruction::AccessVariable(variable.clone()));
							// Access a macro register to get the appropriate rvalue
							self.operation_stack
								.push(Instruction::AccessMacroRegister(i));
							// Make a new variable (let)
							self.operation_stack
								.push(Instruction::CreateVariable(variable.clone()));
						}
					}
					ExpressionContent::Operation(operation_expression) => {
						match operation_expression {
							OperationExpression::Unary(unary_operator, expression) => {
								self.operation_stack
									.push(Instruction::UnaryOperation(*unary_operator));
								self.operation_stack.push(expression);
							}
							OperationExpression::Binary(binary_operator, left, right) => {
								self.operation_stack
									.push(Instruction::BinaryOperation(*binary_operator));
								self.operation_stack.push(right);
								self.operation_stack.push(left);
							}
						}
					}
				},
				Operation::Instruction(instruction) => {
					match instruction {
						Instruction::PushBlank => self.value_stack.push_blank(),
						Instruction::Push(value) => self.value_stack.push(value),
						Instruction::DiscardValue => {
							self.value_stack.pop();
						} // TODO: Error handling
						Instruction::EnterScope => {
							self.context_stack.0.push(ContextFrame::new_sequence())
						}
						Instruction::ExitScope => self.context_stack.pop(), // TODO: Error handling
						Instruction::PushDollar => self.dollar_stack.push_new(),
						Instruction::PopDollar => self.dollar_stack.pop(),
						Instruction::CreateVariable(name) => {
							let frame = self
								.context_stack
								.0
								.last_mut()
								.expect("Context Stack should never be empty"); // TODO: Error handling
							if frame.variables.contains_key(&name) {
								todo!()
							}
							frame.variables.insert(name, VariableSlot::new());
						}
						Instruction::AccessVariable(name) => {
							let mut found = false;
							for context in self.context_stack.0.iter().rev() {
								if let Some(value) = context.variables.get(&name) {
									found = true;
									self.value_stack.push(StackValue::Reference(value.borrow())); // TODO: Error handling
									break;
								}
							}
							if !found {
								todo!(
									"error handling not implemented: variable {} not found",
									name
								)
							}
						}
						Instruction::AccessMacroRegister(index) => {
							self.value_stack.push(StackValue::Reference({
								if index >= self.dollar_stack.0.len() {
									todo!("Errors!");
								}
								self.dollar_stack.0[self.dollar_stack.0.len() - 1 - index].borrow()
							}))
						}
						Instruction::Assign => {
							let lvalue = self.value_stack.pop().unwrap();
							let rvalue = self.value_stack.pop().unwrap();
							if !lvalue.assignable() {
								todo!("Assignment Error!");
							}
							lvalue.assign(rvalue.to_value());
							self.value_stack.push(lvalue);
						}
						Instruction::BinaryOperation(binary_operator) => {
							// TODO: errors
							let right = self.value_stack.pop().unwrap();
							let left = self.value_stack.pop().unwrap();
							if !left.is_plain() || !right.is_plain() {
								todo!()
							}
							let left = match left.to_value() {
								VariableValue::Plain(plain_value) => plain_value,
								VariableValue::Dynamic(dynamic_variable_value) => unreachable!(),
							};
							let right = match right.to_value() {
								VariableValue::Plain(plain_value) => plain_value,
								VariableValue::Dynamic(dynamic_variable_value) => unreachable!(),
							};
							use BinaryOperator::*;
							use PlainValue::*;
							#[rustfmt::skip]
							let result = match (binary_operator, left, right) {
								(BinaryPlus, Int(a), Int(b)) => Some(Int(a + b)),
								(BinaryPlus, Int(a), Float(b)) => Some(Float((a as f32) + b)),
								(BinaryPlus, Float(a), Int(b)) => Some(Float(a + (b as f32))),
								(BinaryPlus, Float(a), Float(b)) => Some(Float(a + b)),
								(BinaryPlus, _, _) => None,
								(BinaryMinus, Int(a), Int(b)) => Some(Int(a - b)),
								(BinaryMinus, Int(a), Float(b)) => Some(Float((a as f32) - b)),
								(BinaryMinus, Float(a), Int(b)) => Some(Float(a - (b as f32))),
								(BinaryMinus, Float(a), Float(b)) => Some(Float(a - b)),
								(BinaryMinus, _, _) => None,
								(Multiply, Int(a), Int(b)) => Some(Int(a * b)),
								(Multiply, Int(a), Float(b)) => Some(Float((a as f32) * b)),
								(Multiply, Float(a), Int(b)) => Some(Float(a * (b as f32))),
								(Multiply, Float(a), Float(b)) => Some(Float(a * b)),
								(Multiply, _, _) => None,
								// TODO: Handle division by 0
								(Divide, Int(a), Int(b)) => Some(Float((a as f32) / (b as f32))),
								(Divide, Int(a), Float(b)) => Some(Float((a as f32) / b)),
								(Divide, Float(a), Int(b)) => Some(Float(a / (b as f32))),
								(Divide, Float(a), Float(b)) => Some(Float(a / b)),
								(Divide, _, _) => None,
								(IntegerDivide, Int(a), Int(b)) => todo!(),
								(IntegerDivide, Int(a), Float(b)) => todo!(),
								(IntegerDivide, Float(a), Int(b)) => todo!(),
								(IntegerDivide, Float(a), Float(b)) => todo!(),
								(IntegerDivide, _, _) => None,
								(Modulo, Int(a), Int(b)) => todo!(),
								(Modulo, Int(a), Float(b)) => todo!(),
								(Modulo, Float(a), Int(b)) => todo!(),
								(Modulo, Float(a), Float(b)) => todo!(),
								(Modulo, _, _) => None,
								(Power, Int(a), Int(b)) => todo!(),
								(Power, Int(a), Float(b)) => todo!(),
								(Power, Float(a), Int(b)) => todo!(),
								(Power, Float(a), Float(b)) => todo!(),
								(Power, _, _) => None,
								(CompareLess, Int(a), Int(b)) => Some(Bool(a < b)),
								(CompareLess, Int(a), Float(b)) => Some(Bool((a as f32) < b)),
								(CompareLess, Float(a), Int(b)) => Some(Bool(a < (b as f32))),
								(CompareLess, Float(a), Float(b)) => Some(Bool(a < b)),
								(CompareLess, _, _) => None,
								(CompareGreater, Int(a), Int(b)) => Some(Bool(a > b)),
								(CompareGreater, Int(a), Float(b)) => Some(Bool((a as f32) > b)),
								(CompareGreater, Float(a), Int(b)) => Some(Bool(a > (b as f32))),
								(CompareGreater, Float(a), Float(b)) => Some(Bool(a > b)),
								(CompareGreater, _, _) => None,
								(CompareLessEqual, Int(a), Int(b)) => Some(Bool(a <= b)),
								(CompareLessEqual, Int(a), Float(b)) => Some(Bool((a as f32) <= b)),
								(CompareLessEqual, Float(a), Int(b)) => Some(Bool(a <= (b as f32))),
								(CompareLessEqual, Float(a), Float(b)) => Some(Bool(a <= b)),
								(CompareLessEqual, _, _) => None,
								(CompareGreaterEqual, Int(a), Int(b)) => Some(Bool(a >= b)),
								(CompareGreaterEqual, Int(a), Float(b)) => Some(Bool((a as f32) >= b)),
								(CompareGreaterEqual, Float(a), Int(b)) => Some(Bool(a >= (b as f32))),
								(CompareGreaterEqual, Float(a), Float(b)) => Some(Bool(a >= b)),
								(CompareGreaterEqual, _, _) => None,
								(CompareEqual, Int(a), Int(b)) => Some(Bool(a == b)),
								(CompareEqual, Int(a), Float(b)) => Some(Bool((a as f32) == b)),
								(CompareEqual, Float(a), Int(b)) => Some(Bool(a == (b as f32))),
								(CompareEqual, Float(a), Float(b)) => Some(Bool(a == b)),
								(CompareEqual, Float(_), _) => Some(Bool(false)),
								(CompareEqual, _, Float(_)) => Some(Bool(false)),
								(CompareEqual, Int(_), _) => Some(Bool(false)),
								(CompareEqual, _, Int(_)) => Some(Bool(false)),
								(CompareEqual, Bool(a), Bool(b)) => Some(Bool(a == b)),
								(CompareEqual, Bool(_), _) => Some(Bool(false)),
								(CompareEqual, _, Bool(_)) => Some(Bool(false)),
								(CompareEqual, Blank, Blank) => Some(Bool(true)),
								(CompareEqual, Blank, _) => Some(Bool(false)),
								(CompareEqual, _, Blank) => Some(Bool(false)),
								(CompareNotEqual, Int(a), Int(b)) => Some(Bool(a != b)),
								(CompareNotEqual, Int(a), Float(b)) => Some(Bool((a as f32) != b)),
								(CompareNotEqual, Float(a), Int(b)) => Some(Bool(a != (b as f32))),
								(CompareNotEqual, Float(a), Float(b)) => Some(Bool(a != b)),
								(CompareNotEqual, Float(_), _) => Some(Bool(true)),
								(CompareNotEqual, _, Float(_)) => Some(Bool(true)),
								(CompareNotEqual, Int(_), _) => Some(Bool(true)),
								(CompareNotEqual, _, Int(_)) => Some(Bool(true)),
								(CompareNotEqual, Bool(a), Bool(b)) => Some(Bool(a != b)),
								(CompareNotEqual, Bool(_), _) => Some(Bool(true)),
								(CompareNotEqual, _, Bool(_)) => Some(Bool(true)),
								(CompareNotEqual, Blank, Blank) => Some(Bool(false)),
								(CompareNotEqual, Blank, _) => Some(Bool(true)),
								(CompareNotEqual, _, Blank) => Some(Bool(true)),
								(And, Bool(a), Bool(b)) => Some(Bool(a && b)),
								(And, _, _) => None,
								(Or, Bool(a), Bool(b)) => Some(Bool(a || b)),
								(Or, _, _) => None,
							};
							match result {
								Some(value) => self.value_stack.push(VariableValue::Plain(value)),
								None => todo!("Error handling yay"),
							}
						}
						Instruction::UnaryOperation(unary_operator) => {
							// TODO: Error handling for none.
							let input = self.value_stack.pop().unwrap();
							let result = match unary_operator {
								super::ast::UnaryOperator::UnaryPlus => {
									// To avoid unncecesary copies
									if !input.is_plain() {
										todo!("Wrong type")
									}
									let value = input.to_value();
									match value {
										VariableValue::Plain(PlainValue::Blank) => todo!("Error"),
										VariableValue::Plain(PlainValue::Int(_)) => value,
										VariableValue::Plain(PlainValue::Float(_)) => value,
										VariableValue::Plain(PlainValue::Bool(_)) => todo!("Error"),
										VariableValue::Dynamic(dynamic_variable_value) => {
											todo!("Error")
										}
									}
								}
								super::ast::UnaryOperator::UnaryMinus => {
									// To avoid unncecesary copies
									if !input.is_plain() {
										todo!("Wrong type")
									}
									match input.to_value() {
										VariableValue::Plain(PlainValue::Blank) => todo!("Error"),
										VariableValue::Plain(PlainValue::Int(value)) => {
											(-value).into()
										}
										VariableValue::Plain(PlainValue::Float(value)) => {
											(-value).into()
										}
										VariableValue::Plain(PlainValue::Bool(_)) => todo!("Error"),
										VariableValue::Dynamic(dynamic_variable_value) => {
											todo!("Error")
										}
									}
								}
								super::ast::UnaryOperator::Not => {
									// To avoid unncecesary copies
									if !input.is_plain() {
										todo!("Wrong type")
									}
									match input.to_value() {
										VariableValue::Plain(PlainValue::Blank) => todo!("Error"),
										VariableValue::Plain(PlainValue::Int(_)) => todo!("Error"),
										VariableValue::Plain(PlainValue::Float(_)) => {
											todo!("Error")
										}
										VariableValue::Plain(PlainValue::Bool(value)) => {
											(!value).into()
										}
										VariableValue::Dynamic(dynamic_variable_value) => {
											todo!("Error")
										}
									}
								}
							};
							self.value_stack.push(result);
						}
						Instruction::Index => {
							let index = self.value_stack.pop().unwrap().to_value();
							let mut indexed_value = self.value_stack.pop().unwrap();
							match indexed_value.index(index) {
								Some(value) => self.value_stack.push(value),
								None => todo!(),
							}
						}
						Instruction::Push(variable_value) => self.value_stack.push(variable_value),
					}
				}
				Operation::MethodCall(name, arguments) => {
					// TODO errors
					match self
						.value_stack
						.pop()
						.unwrap()
						.introspect_mut(|value| match value {
							VariableValue::Plain(plain_value) => match plain_value {
								PlainValue::Int(iterations) => match name {
									"for" => arguments
										.as_ref()
										.and_then(|a| a.positional_arguments.first())
										.map(|inner_expr| {
											Box::new(ForMacro::<'ast>::new(*iterations, inner_expr))
												as Box<dyn Macro>
										}),
									_ => None,
								},
								_ => None,
							},
							VariableValue::Dynamic(dynamic_variable_value) => {
								dynamic_variable_value.call_method(name, arguments)
							}
						}) {
						Some(macro_data) => {
							self.operation_stack
								.push(Operation::MacroCallBack(macro_data));
							self.operation_stack.push(Instruction::PushBlank);
						}
						None => {
							todo!()
						}
					}
				}
				Operation::MacroCallBack(mut macro_data) => {
					let input = self.value_stack.pop().unwrap(); // TODO
					match macro_data.poll(input) {
						MacroReturn::Operations(queue) => {
							self.operation_stack
								.push(Operation::MacroCallBack(macro_data));
							for operation in queue.into_iter().rev() {
								self.operation_stack.0.push(operation);
							}
						}
						MacroReturn::Return(value) => self.value_stack.push(value),
					}
				}
			}
		}
	}
}

mod epilang_types {
	use std::{cell::Cell, rc::Rc};

	use crate::epilang::ast::ArgumentList;

	use super::{
		epilang_macros::{ConstantMacro, ForMacro},
		DynamicVariableValue, Macro, PlainValue, ReferenceValue, VariableValue,
	};

	pub struct ArrayType {
		// This could be done better with some sort of subrc construct, but that would require unsafe and I won't bother.
		values: Vec<Rc<Cell<VariableValue>>>,
		/// [`Rc`] to keep track of whether there are external pointers to the insides of this object.
		borrow_counter: Rc<()>,
	}

	impl ArrayType {
		pub fn new(values: Vec<VariableValue>) -> Self {
			ArrayType {
				values: values
					.into_iter()
					.map(|val| Rc::new(Cell::new(val)))
					.collect(),
				borrow_counter: Rc::new(()),
			}
		}
	}

	impl DynamicVariableValue for ArrayType {
		fn index(&mut self, index: VariableValue) -> Option<ReferenceValue> {
			if self.values.is_empty() {
				return None; // TODO: Error handling
			}
			let index = match index {
				VariableValue::Plain(PlainValue::Int(value)) => {
					((value as isize).rem_euclid(self.values.len() as isize) as usize)
				}
				_ => todo!("Error handling for wrong type array indexing not yet implemented"),
			};
			let value = &self.values[index];
			Some(ReferenceValue {
				counter: self.borrow_counter.clone(),
				value: self.values[index].clone(),
			})
		}

		fn is_mutable(&self) -> bool {
			// Check we're the only person holding a reference to the counter
			Rc::strong_count(&self.borrow_counter) == 1
		}

		fn clone(&self) -> Box<dyn DynamicVariableValue> {
			Box::new(ArrayType {
				values: self
					.values
					.iter()
					.map(|val| {
						let inner = (*val.as_ref()).take();
						let clone = inner.clone();
						val.as_ref().set(inner);
						Rc::new(Cell::new(clone))
					})
					.collect(),
				borrow_counter: Rc::new(()),
			})
		}

		fn call_method<'ast>(
			&mut self,
			name: &str,
			arguments: &'ast Option<ArgumentList>,
		) -> Option<Box<dyn Macro<'ast> + 'ast>> {
			match name {
				"for" => todo!(),
				"len" => Some(Box::new(ConstantMacro::new(
					(self.values.len() as i32).into(),
				))),
				_ => None,
			}
		}
	}
}

mod epilang_macros {
	use crate::epilang::{ast::Expression, interpreter::CellValueExt};

	use super::{
		epilang_types::ArrayType, Instruction, Macro, MacroReturn, Operation, PlainValue,
		StackValue, VariableValue,
	};

	/// An unary macro that works as an identity, but does debug printing on the passed in value.
	pub struct PrintMacro<'ast> {
		expr: &'ast Expression,
		evaluating: bool,
	}

	impl<'ast> PrintMacro<'ast> {
		pub fn new(expr: &'ast Expression) -> Self {
			PrintMacro {
				expr,
				evaluating: true,
			}
		}
	}

	impl<'ast> Macro<'ast> for PrintMacro<'ast> {
		fn poll(&mut self, input: StackValue) -> MacroReturn<'ast> {
			if self.evaluating {
				self.evaluating = false;
				MacroReturn::Operations(vec![self.expr.into()])
			} else {
				fn log_value(value: &VariableValue) {
					match value {
						VariableValue::Plain(plain_value) => match plain_value {
							PlainValue::Blank => println!("_Blank"),
							PlainValue::Bool(value) => println!("b{:?}", value),
							PlainValue::Int(value) => println!("i{:?}", value),
							PlainValue::Float(value) => println!("f{:?}", value),
						},
						VariableValue::Dynamic(dynamic_variable_value) => println!("Dynamic value"),
					}
				}
				match &input {
					StackValue::Value(variable_value) => log_value(variable_value),
					StackValue::Reference(reference_value) => {
						print!("Reference to: ");
						reference_value.value.as_ref().introspect(log_value);
					}
				}
				MacroReturn::Return(input)
			}
		}
	}

	/// A nullary macro that simply yields the VariableValue it was constructed with.
	pub struct ConstantMacro {
		value: VariableValue,
	}

	impl ConstantMacro {
		pub fn new(value: VariableValue) -> Self {
			ConstantMacro { value }
		}
	}

	impl<'ast> Macro<'ast> for ConstantMacro {
		fn poll(&mut self, input: StackValue) -> MacroReturn<'ast> {
			let mut return_value = VariableValue::default();
			std::mem::swap(&mut self.value, &mut return_value);
			MacroReturn::Return(return_value.into())
		}
	}

	enum ArrayLiteralMacroState {
		Init,
		EvaluateCounter,
		GenerateValues { counter: usize, maximum: usize },
	}

	/// Internal macro used to implement arrayfill literals.
	pub struct ArrayFillLiteralMacro<'ast> {
		values: Vec<VariableValue>,
		state: ArrayLiteralMacroState,
		inner_expression: &'ast Expression,
		counter_expression: &'ast Expression,
	}

	impl<'ast> ArrayFillLiteralMacro<'ast> {
		pub fn new(
			inner_expression: &'ast Expression,
			counter_expression: &'ast Expression,
		) -> Self {
			ArrayFillLiteralMacro {
				values: Vec::new(),
				state: ArrayLiteralMacroState::Init,
				inner_expression,
				counter_expression,
			}
		}
	}

	impl<'ast> Macro<'ast> for ArrayFillLiteralMacro<'ast> {
		fn poll(&mut self, input: StackValue) -> super::MacroReturn<'ast> {
			match self.state {
				ArrayLiteralMacroState::Init => {
					self.state = ArrayLiteralMacroState::EvaluateCounter;
					return MacroReturn::Operations(vec![self.counter_expression.into()]);
				}
				ArrayLiteralMacroState::EvaluateCounter => {
					match input.introspect(|inner| match inner {
						VariableValue::Plain(PlainValue::Int(value)) => {
							if *value == 0 {
								return Some(MacroReturn::Return(
									VariableValue::Dynamic(Box::new(ArrayType::new(Vec::new())))
										.into(),
								));
							}
							if *value < 0 {
								todo!("Error handling");
							}
							self.state = ArrayLiteralMacroState::GenerateValues {
								counter: 0,
								maximum: *value as usize,
							};
							None
						}
						VariableValue::Dynamic(dynamic_variable_value) => todo!("Array handling"),
						_ => todo!("Error handling"),
					}) {
						Some(return_value) => return return_value,
						None => {}
					}
				}
				ArrayLiteralMacroState::GenerateValues { .. } => {
					self.values.push(input.to_value());
				}
			};
			let ArrayLiteralMacroState::GenerateValues { counter, maximum } = &mut self.state
			else {
				todo!("This should be unreachable, probably.");
			};
			if counter >= maximum {
				let mut temp = Vec::new();
				std::mem::swap(&mut self.values, &mut temp);
				return MacroReturn::Return(
					VariableValue::Dynamic(Box::new(ArrayType::new(temp))).into(),
				);
			}
			let return_val = MacroReturn::Operations(vec![
				Instruction::EnterScope.into(),
				Instruction::PushDollar.into(),
				Instruction::Push((*counter as i32).into()).into(),
				Instruction::AccessMacroRegister(0).into(),
				Instruction::Assign.into(),
				Instruction::DiscardValue.into(),
				self.inner_expression.into(),
				Instruction::PopDollar.into(),
				Instruction::ExitScope.into(),
			]);
			*counter += 1;
			return_val
		}
	}

	/// Internal macro used to implement array literals
	pub struct ArrayLiteralMacro<'ast> {
		values: Vec<VariableValue>,
		counter: usize,
		inner_expressions: &'ast Vec<Expression>,
	}

	impl<'ast> ArrayLiteralMacro<'ast> {
		pub fn new(inner_expressions: &'ast Vec<Expression>) -> Self {
			ArrayLiteralMacro {
				values: Vec::with_capacity(inner_expressions.len()),
				counter: 0,
				inner_expressions,
			}
		}
	}

	impl<'ast> Macro<'ast> for ArrayLiteralMacro<'ast> {
		fn poll(&mut self, input: StackValue) -> super::MacroReturn<'ast> {
			if self.counter > 0 {
				self.values.push(input.to_value());
			}
			if self.counter >= self.inner_expressions.len() {
				let mut temp = Vec::new();
				std::mem::swap(&mut self.values, &mut temp);
				return MacroReturn::Return(
					VariableValue::Dynamic(Box::new(ArrayType::new(temp))).into(),
				);
			}
			let return_val = MacroReturn::Operations(vec![
				Instruction::EnterScope.into(),
				(&self.inner_expressions[self.counter]).into(),
				Instruction::ExitScope.into(),
			]);
			self.counter += 1;
			return_val
		}
	}

	pub struct ForMacro<'ast> {
		iterator: Box<dyn Iterator<Item = VariableValue>>,
		inner_expression: &'ast Expression,
	}

	impl<'ast> ForMacro<'ast> {
		pub fn new(iteration: i32, inner_expression: &'ast Expression) -> Self {
			let iteration = iteration.max(0);
			ForMacro {
				iterator: Box::new((0..iteration).map(VariableValue::from)),
				inner_expression,
			}
		}
	}

	impl<'ast> Macro<'ast> for ForMacro<'ast> {
		fn poll(&mut self, input: StackValue) -> super::MacroReturn<'ast> {
			if let Some(value) = self.iterator.next() {
				MacroReturn::Operations(vec![
					Instruction::EnterScope.into(),
					Instruction::PushDollar.into(),
					Instruction::Push(value).into(),
					Instruction::AccessMacroRegister(0).into(),
					Instruction::Assign.into(),
					Instruction::DiscardValue.into(),
					self.inner_expression.into(),
					Instruction::PopDollar.into(),
					Instruction::ExitScope.into(),
				])
			} else {
				MacroReturn::Return(VariableValue::Plain(PlainValue::Blank).into())
			}
		}
	}
}

mod test {
	use std::collections::HashMap;

	use crate::epilang::{
		ast::{ArgumentList, Expression, ExpressionContent, Sequence},
		lex::{tokenize, Token},
		parser::parse,
		SourceLocation,
	};

	use super::{
		epilang_macros::{ConstantMacro, PrintMacro},
		Interpreter, Macro, MacroConstructor, PlainValue,
	};

	const NOT_HALTED_ERROR: &'static str = "Interpreter has not finished running during the testcase! Consider increasing the step count or improving performance";

	fn macro_dictionary<'ast>() -> HashMap<String, Box<MacroConstructor<'ast>>> {
		let mut map: HashMap<
			String,
			Box<
				dyn FnMut(Option<&'ast ArgumentList>) -> Option<Box<dyn Macro<'ast> + 'ast>> + 'ast,
			>,
		> = HashMap::new();
		map.insert(
			"print".to_string(),
			Box::new(|args: Option<&'ast ArgumentList>| match args {
				Some(args) => {
					if let Some(expr) = args.positional_arguments.first() {
						Some(Box::new(PrintMacro::new(expr)))
					} else {
						None
					}
				}
				None => None,
			}),
		);
		map.insert(
			"true".to_string(),
			Box::new(|args: Option<&'ast ArgumentList>| {
				Some(Box::new(ConstantMacro::new(true.into())))
			}),
		);
		map.insert(
			"false".to_string(),
			Box::new(|args: Option<&'ast ArgumentList>| {
				Some(Box::new(ConstantMacro::new(false.into())))
			}),
		);
		map
	}

	fn get_tokens(text: &str) -> Vec<(Token, std::ops::Range<SourceLocation>)> {
		tokenize(text).collect::<Result<Vec<_>, _>>().unwrap()
	}

	fn get_ast(text: &str) -> Expression {
		Expression {
			loc: SourceLocation::new(0, 0)..SourceLocation::new(0, 1),
			value: Box::new(ExpressionContent::Sequence(
				parse(get_tokens(text)).unwrap(),
			)),
		}
	}

	#[test]
	fn test_print() {
		let dictionary = macro_dictionary();

		let ast = get_ast("print(0)");

		let mut interpreter = Interpreter::new(&ast, dictionary);

		interpreter.execute(100);
		assert!(interpreter.halted(), "{}", NOT_HALTED_ERROR);
	}

	#[test]
	fn test_unary() {
		let dictionary = macro_dictionary();

		let ast = get_ast(
			r"
print(1);
print(+1);
print(-1);
print(1.5);
print(+1.5);
print(-1.5);
print(true);
print(false);
print(!false);
print(_);
",
		);

		let mut interpreter = Interpreter::new(&ast, dictionary);

		interpreter.execute(100);
		assert!(interpreter.halted(), "{}", NOT_HALTED_ERROR);
	}

	#[test]
	fn test_let() {
		let dictionary = macro_dictionary();

		let ast = get_ast(
			r"
let a = 3;
print(a);
a = -a;
print(a);
a = 5.5;
print(a);
print(a);
let b = _;
print(a);
print(b);
a = b;
print(a);
print(b);
b = 420;
print(a);
print(b);
a = b;
a = false;
print(a);
print(b);
let d = let c = b = a = 123;
print(a);
print(b);
print(c);
print(d);
",
		);

		let mut interpreter = Interpreter::new(&ast, dictionary);

		interpreter.execute(1000);
		assert!(interpreter.halted(), "{}", NOT_HALTED_ERROR);
	}

	#[test]
	fn test_scope() {
		let dictionary = macro_dictionary();

		let ast = get_ast(
			r"
let a = 3;
print(a);
let b = {
			print(a);
			let a = 5;
			print(a);
			a
};
print(a);
print(b);
",
		);

		let mut interpreter = Interpreter::new(&ast, dictionary);

		interpreter.execute(100);
		assert!(interpreter.halted(), "{}", NOT_HALTED_ERROR);
	}

	#[test]
	fn test_array_fill() {
		let dictionary = macro_dictionary();

		let ast = get_ast(
			r"
let a = [_;3];
let b = [a;4];
let c = [$0;5];
print(a);
print(b);
print(c);
",
		);

		let mut interpreter = Interpreter::new(&ast, dictionary);

		interpreter.execute(200);
		assert!(interpreter.halted(), "{}", NOT_HALTED_ERROR);
	}

	#[test]
	fn test_array_literal() {
		let dictionary = macro_dictionary();

		let ast = get_ast(
			r"
let a = [0,1,2,3];
let b = [a,a,a];
print(a);
print(b);
",
		);

		let mut interpreter = Interpreter::new(&ast, dictionary);

		interpreter.execute(200);
		assert!(interpreter.halted(), "{}", NOT_HALTED_ERROR);
	}

	#[test]
	fn test_array_index() {
		let dictionary = macro_dictionary();

		let ast = get_ast(
			r"
let a = [_;3];
let b = [a;4];
let c = [$0;5];

print(a[0]);
print(b[0][0]);
print(c[0]);
print(c[1]);
print(c[2]);
print(c[3]);
print(c[4]);
print(c[5]);
print(c[6]);
print(c[-1]);

let d = [true, false];
print(d[0]);
print(d[1]);
print(d[2]);
",
		);

		let mut interpreter = Interpreter::new(&ast, dictionary);

		interpreter.execute(1000);
		assert!(interpreter.halted(), "{}", NOT_HALTED_ERROR);
	}

	#[test]
	fn test_array_mutability() {
		let dictionary = macro_dictionary();

		let ast = get_ast(
			r"
let a = [_;3];

a[0] = 1;
a[a[0]] = 2;
a[a[a[0]]] = 3;

let b = 0;
b = a[b];
print(b);
b = a[b];
print(b);
b = a[b];
print(b);
b = a[0];
b = 7;
print(a[0]);
print(a[1]);
print(a[2]);

let nested = [a; 4];

print(nested[0][0]);
print(nested[1][0]);
nested[1][0] = 76;
print(nested[0][0]);
print(nested[1][0]);
nested[0] = _;
print(nested[0]);
print(nested[1][0]);
",
		);

		let mut interpreter = Interpreter::new(&ast, dictionary);

		interpreter.execute(1000);
		assert!(interpreter.halted(), "{}", NOT_HALTED_ERROR);
	}

	#[test]
	fn test_nested_array_literal() {
		let dictionary = macro_dictionary();

		/// Note: This is designed to have the same output as [`test_lambda`]
		let ast = get_ast(
			r"
let a = [$0; 3];
print(a[0]);
print(a[1]);
print(a[2]);
print(_);
a = [[[$0,$1]; 3]; 4];
print(a[0][0][0]);
print(a[0][0][1]);
print(_);
print(a[0][1][0]);
print(a[0][1][1]);
print(_);
print(a[3][0][0]);
print(a[3][0][1]);
print(_);
print(a[3][2][0]);
print(a[3][2][1]);
",
		);

		let mut interpreter = Interpreter::new(&ast, dictionary);

		interpreter.execute(1000);
		assert!(interpreter.halted(), "{}", NOT_HALTED_ERROR);
	}

	#[test]
	fn test_lambda() {
		let dictionary = macro_dictionary();

		/// Note: This is designed to have the same output as [`test_nested_array_literal`]
		let ast = get_ast(
			r"
let a = [i => i; 3];
print(a[0]);
print(a[1]);
print(a[2]);
print(_);
a = [[(x,y) => [x,y]; 3]; 4];
print(a[0][0][0]);
print(a[0][0][1]);
print(_);
print(a[0][1][0]);
print(a[0][1][1]);
print(_);
print(a[3][0][0]);
print(a[3][0][1]);
print(_);
print(a[3][2][0]);
print(a[3][2][1]);
",
		);

		let mut interpreter = Interpreter::new(&ast, dictionary);

		interpreter.execute(1000);
		assert!(interpreter.halted(), "{}", NOT_HALTED_ERROR);
	}

	#[test]
	fn test_array_len() {
		let dictionary = macro_dictionary();

		let ast = get_ast(
			r"
let a = [_;3];

print(a.len());
print(a.len);
",
		);

		let mut interpreter = Interpreter::new(&ast, dictionary);

		interpreter.execute(100);
		assert!(interpreter.halted(), "{}", NOT_HALTED_ERROR);
	}

	#[test]
	fn test_array_len_advanced() {
		let dictionary = macro_dictionary();

		let ast = get_ast(
			r"
let a = [[_;$0];3];

print(a.len);
print(a[0].len);
print(a[1].len);
print(a[2].len);
",
		);

		let mut interpreter = Interpreter::new(&ast, dictionary);

		interpreter.execute(500);
		assert!(interpreter.halted(), "{}", NOT_HALTED_ERROR);
	}

	#[test]
	fn test_for() {
		let dictionary = macro_dictionary();

		let ast = get_ast(
			r"
let a = [2,3,5,7,11];

print(_);
a.len.for(print($0));

print(_);
a.len.for(print(a[$0]));
print(_);
a.len.for(i => print(a[i]));

",
		);

		let mut interpreter = Interpreter::new(&ast, dictionary);

		interpreter.execute(500);
		assert!(interpreter.halted(), "{}", NOT_HALTED_ERROR);
	}

	#[test]
	fn test_for_advanced() {
		let dictionary = macro_dictionary();

		let ast = get_ast(
			r"
let a = [[(x,y) => [x,y]; 3]; 4];

print(_);
4.for(3.for({
	print($0);
	print($1);
}));

print(_);
a.len.for(a[$0].len.for({
	print(a[$1][$0][0]);
	print(a[$1][$0][1]);
}));
",
		);

		let mut interpreter = Interpreter::new(&ast, dictionary);

		interpreter.execute(2000);
		assert!(interpreter.halted(), "{}", NOT_HALTED_ERROR);
	}

	#[test]
	fn test_math() {
		let dictionary = macro_dictionary();

		// TODO: Better tests and better coverage
		let ast = get_ast(
			r"
let N = 20;
let A = [1,2,3];
print(2 * N);
print(N + N);
print(A[2] * A[2]);
print(9);
print(A[2] == A[2]);
print(A[2] == 3);
print(20 - 50);
print(-30);
print(20 - 50.0);
print(20.0 - 50);
print(20 == 20.0);
print(20.0 != 50);
print(20 >= 20.0);
print(20.0 <= 50);
print(20 > 20.0);
print(20.0 < 10);

print(_);
print(true && true);
print(true && false);
print(false && true);
print(false && false);

print(_);
print(true || true);
print(true || false);
print(false || true);
print(false || false);
",
		);

		let mut interpreter = Interpreter::new(&ast, dictionary);

		interpreter.execute(500);
		assert!(interpreter.halted(), "{}", NOT_HALTED_ERROR);
	}

	#[test]
	fn test_erasthotenes() {
		let dictionary = macro_dictionary();

		let ast = get_ast(
			r"
let sqrt_N = 7;

let N = sqrt_N * sqrt_N;

let primes = [true; N];
primes[0] = false;
primes[1] = false;

(sqrt_N - 2).for({
	let divisor1 = $0 + 2;
	(N - 2).for({
		let product = divisor1 * ($0 + 2);
		primes[product] = primes[product] && product >= N;
	});
});

N.for(i => {
	print(i);
	print(primes[i]);
});
",
		);

		let mut interpreter = Interpreter::new(&ast, dictionary);

		interpreter.execute(20000);
		assert!(interpreter.halted(), "{}", NOT_HALTED_ERROR);
	}
}
