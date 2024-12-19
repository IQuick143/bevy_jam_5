use super::{ast::Expression, SourceLocation};
use bevy::utils::HashMap;
use std::{cell::RefCell, ops::Range, rc::Rc};

trait GetVariableType {
	type Type;
	fn get_type(&self) -> Self::Type;
}

enum VariableType<T> {
	Blank,
	Boolean,
	Integer,
	Float,
	String,
	Array,
	Custom(T),
}

enum VariableValue<T: GetVariableType> {
	Blank,
	Boolean(bool),
	Integer(i32),
	Float(f32),
	String(String),
	Array(Rc<RefCell<[VariableValue<T>]>>),
	Custom(T),
}

impl<T: GetVariableType> GetVariableType for VariableValue<T> {
	type Type = VariableType<T::Type>;
	fn get_type(&self) -> Self::Type {
		match self {
			Self::Blank => Self::Type::Blank,
			Self::Boolean(_) => Self::Type::Boolean,
			Self::Integer(_) => Self::Type::Integer,
			Self::Float(_) => Self::Type::Float,
			Self::String(_) => Self::Type::String,
			Self::Array(_) => Self::Type::Array,
			Self::Custom(x) => Self::Type::Custom(x.get_type()),
		}
	}
}

enum VariablePathSegment<'ast> {
	Method(&'ast str),
	Index(i32),
}

struct VariablePath<'ast> {
	variable: &'ast str,
	indirections: Vec<VariablePathSegment<'ast>>,
}

enum InterpreterValue<'ast, T: GetVariableType> {
	Unit,
	Monad(Option<VariableValue<T>>),
	Variable(VariableValue<T>),
	MethodName(&'ast str),
	Path(VariablePath<'ast>),
}

struct VariableSlot<T: GetVariableType> {
	value: Option<VariableValue<T>>,
	declaration_loc: Range<SourceLocation>,
}

enum ContextFrameType {
	Sequence,
	MacroArgument,
}

struct ContextFrame<T: GetVariableType> {
	macro_parameters: Option<Vec<VariableValue<T>>>,
	variables: HashMap<String, VariableSlot<T>>,
	context_loc: Range<SourceLocation>,
	context_type: ContextFrameType,
}

enum Operation<'ast, T: GetVariableType> {
	Eval(&'ast Expression),
	ToRvalue,
	PopValue,
	PushContext(ContextFrame<T>),
	PopContext,
	Collect(usize),
}

struct ContextStack<T: GetVariableType>(Vec<ContextFrame<T>>);
