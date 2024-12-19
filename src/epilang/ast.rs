use super::SourceLocation;
use std::ops::Range;

/// A program file or a brace-enclosed block.
/// Contains zero or more expressions separated with semicolon,
/// and optionally an expression at the tail that the whole block
/// will evaluate to
#[derive(Clone, PartialEq, Default, Debug)]
pub struct Sequence {
	pub statements: Vec<Expression>,
	pub tail: Option<Expression>,
}

/// Expression with information about its location in the source file
#[derive(Clone, PartialEq, Debug)]
pub struct Expression {
	/// Location of the expression in the source file
	pub loc: Range<SourceLocation>,
	/// The expression AST
	pub value: Box<ExpressionContent>,
}

/// Enumerates all varints of what is considered an expression
#[derive(Clone, Debug, PartialEq)]
pub enum ExpressionContent {
	/// Brace-enclosed block expression. Evaluates to its tail expression, if any
	/// ## Example
	/// ```plain
	/// {
	///   expr1;
	///   expr2;
	///   expr3
	/// }
	/// ```
	Sequence(Sequence),
	/// Assignment expression
	Assignment(Expression, Expression),
	/// Integer literal
	IntLiteral(u32),
	/// Floating-point number literal
	FloatLiteral(f32),
	/// String literal
	StringLiteral(String),
	/// Identifier. Can be a variable name or a call to a global macro without arguments.
	/// ## Example
	/// ```plain
	/// a  # Variable named 'a'
	/// vertex  # Conctructor of vertex, called without any parameters
	/// ```
	Identifier(String),
	/// Let expression.
	/// Creates a new local variable in the current scope
	/// (scope can be a brace-enclosed [`Sequence`] or a macro argument)
	/// and evaluates to an lvalue of that variable.
	/// The variable is created at time of evaluation of the expression.
	/// Earlier accesses to a variable of the same name in the same scope
	/// will be to the global variable (or a local one of a higher scope, if present).
	/// ## Example
	/// ```plain
	/// a = 0;  # Global variable declared in global scope
	/// { b = 1 };  # Also a global variable, no let expression
	/// { let a = 3 };  # Local variable 'a'
	/// {
	///   a = 2;  # Global variable accessed
	///   let a = 3;  # Local variable declared
	///   a = 4;  # Local variable accessed
	/// };
	/// {
	///   let b = 2;
	///   {
	///     let a = 3;  # Local variable created
	///     b = 3;  # Using the local 'b' from higher scope
	///   }
	///   a = 4;  # Global 'a' accessed
	/// };
	/// a = let c = let d = 4;  # Let expressions are lvalues, assignments to them can be chained
	/// let a;  # Error: local variable already exists at this scope
	/// ```
	LetIdentifier(String),
	/// Macro parameter literal.
	/// Can be used in the body of a macro call expression,
	/// where the appropriate value is substituted for it before evaluation.
	/// ## Example
	/// ```plain
	/// [7, 11, 9].map([$0 + 1, $1])
	/// ```
	/// evaluates to
	/// ```plain
	/// [[8, 0], [12, 1], [10, 3]]
	/// ```
	MacroParameter(u32),
	/// Blank expression.
	/// Literal of the blank type which can be implicitly cast to the default
	/// value of any compatible type
	/// ## Example
	/// ```plain
	/// vertex(_);  # Vertex expects an object or glyph, blank means null
	/// cycle([_, _, _]);  # Cycle expects a list of vertices, blank means default vertex
	/// cycle(4.map(_));  # Returning blank from a map also works
	/// ```
	Blank,
	/// Call to a global macro. Permits positional and named arguments.
	/// If no arguments are passed or one positional argument is passed
	/// and it is a brace-enclosed sequence, the parens may be omitted.
	/// ## Example
	/// ```plain
	/// circle(center, r: radius);
	/// do {
	///   let i = a * 2;
	///   arr[i] = a;
	/// };
	/// ```
	MacroCall(String, ArgumentList),
	/// Call to a method macro or field access.
	/// Permits positional and named arguments.
	/// If no arguments are passed or one positional argument is passed
	/// and it is a brace-enclosed sequence, the parens may be omitted.
	/// ## Example
	/// ```plain
	/// 2.map($);  # Method call
	/// 2.sin;  # Method call, without arguments
	/// 2.map { a[$] = 1; };  # Method call with a sequence macro parameter.
	/// ```
	MethodCall(Expression, String, Option<ArgumentList>),
	/// Access to an array element. The array cannot be empty.
	/// Index has modulo semantics: if it is negative or overflows
	/// the array size, it is wrapped to be in-range.
	/// ## Example
	/// ```plain
	/// arr = [1, 2, 3];
	/// arr[0] = 0;  # Accesses the first element
	/// arr[-1] = 0;  # Accesses the last element
	/// arr[4] = 0;  # Accesses the second element
	/// [][0];  # Error: indexing into empty array
	/// ```
	IndexAccess(Expression, Expression),
	/// Array literal defined by a list of the elements.
	/// ## Example
	/// ```plain
	/// [];  # Empty array
	/// [1, 2, 3];  # Array containing these elements, in that order
	/// ```
	ArrayLiteral(Vec<Expression>),
	/// Array literal defined by a generating expression and size.
	/// The size may be a non-negative integer or an array of non-negative integers
	/// that specify the dimensions of a multi-dimensional array.
	/// The generating expression can use macro arguments that will
	/// evaluate to the indiced of the element being generated.
	/// ## Example
	/// ```plain
	/// [1; 5] == [1, 1, 1, 1, 1];
	/// [1; [2, 3]] == [[1, 1, 1], [1, 1, 1]];
	/// [$0 + $1; [2, 3]] == [[0, 1, 2], [1, 2, 3]];
	/// ```
	FillArrayLiteral(Expression, Expression),
	/// Lambda expression. Syntactic sugar for a let expression.
	/// ## Example
	/// ```plain
	/// 4.map(i => i.sqrt)
	/// ```
	/// is equivalent to
	/// ```plain
	/// 4.map {
	///   let i = $;
	///   i.sqrt
	/// }
	/// ```
	Lambda(Vec<String>, Expression),
	Operation(OperationExpression),
}

#[derive(Clone, Debug, PartialEq)]
pub enum OperationExpression {
	UnaryPlus(Expression),
	UnaryMinus(Expression),
	Not(Expression),
	BinaryPlus(Expression, Expression),
	BinaryMinus(Expression, Expression),
	Multiply(Expression, Expression),
	Divide(Expression, Expression),
	IntegerDivide(Expression, Expression),
	Modulo(Expression, Expression),
	Power(Expression, Expression),
	CompareLess(Expression, Expression),
	CompareGreater(Expression, Expression),
	CompareLessEqual(Expression, Expression),
	CompareGreaterEqual(Expression, Expression),
	CompareEqual(Expression, Expression),
	CompareNotEqual(Expression, Expression),
	And(Expression, Expression),
	Or(Expression, Expression),
}

#[derive(Clone, PartialEq, Default, Debug)]
pub struct ArgumentList {
	pub positional_arguments: Vec<Expression>,
	pub named_arguments: Vec<NamedArgument>,
}

#[derive(Clone, PartialEq, Debug)]
pub struct NamedArgument {
	pub name: String,
	pub value: Expression,
}

impl Sequence {
	pub fn new() -> Self {
		Self::default()
	}

	pub fn push_tail(mut self, new_tail: Option<Expression>) -> Self {
		if let Some(tail) = self.tail {
			self.statements.push(tail);
		}
		self.tail = new_tail;
		self
	}
}

impl Expression {
	pub fn new(location: Range<SourceLocation>, value: ExpressionContent) -> Self {
		Self {
			loc: location,
			value: Box::new(value),
		}
	}
}

impl ArgumentList {
	pub fn new(positional_arguments: Vec<Expression>, named_arguments: Vec<NamedArgument>) -> Self {
		Self {
			positional_arguments,
			named_arguments,
		}
	}
}

impl NamedArgument {
	pub fn new(name: String, value: Expression) -> Self {
		Self { name, value }
	}
}
