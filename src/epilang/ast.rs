use super::SourceLocation;
use std::ops::Range;

/// Represents a compiled source file
#[derive(Clone, Debug)]
pub struct Module(pub(super) Block);

#[derive(Clone, Debug)]
pub struct Block(pub Vec<Statement>);

#[derive(Clone, Debug)]
pub enum Statement {
	Expression(ExpressionNode),
	Condition(ConditionStatement),
}

#[derive(Clone, Debug)]
pub struct ConditionStatement {
	pub cases: Vec<ConditionCase>,
	pub tail: Option<Block>,
}

#[derive(Clone, Debug)]
pub struct ConditionCase {
	pub condition: ExpressionNode,
	pub handler: Block,
}

#[derive(Clone, Copy, Debug)]
pub struct ConditionStatementView<'a> {
	pub cases: &'a [ConditionCase],
	pub tail: &'a Option<Block>,
}

#[derive(Clone, Debug)]
pub struct ExpressionNode {
	pub loc: Range<SourceLocation>,
	pub expression: Box<Expression>,
}

#[derive(Clone, Debug)]
pub enum Expression {
	BlankLiteral,
	IntLiteral(i32),
	FloatLiteral(f32),
	StringLiteral(String),
	VariableValue(String),
	Callback(Module),
	UnaryOperator(UnaryOperatorExpression),
	BinaryOperator(BinaryOperatorExpression),
	Assignment(AssignmentExpression),
	Call(CallExpression),
}

#[derive(Clone, Debug)]
pub struct UnaryOperatorExpression {
	pub operand: ExpressionNode,
	pub operator: UnaryOperator,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum UnaryOperator {
	Plus,
	Minus,
	Not,
}

impl std::fmt::Display for UnaryOperator {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::Plus => f.write_str("+"),
			Self::Minus => f.write_str("-"),
			Self::Not => f.write_str("!"),
		}
	}
}

#[derive(Clone, Debug)]
pub struct BinaryOperatorExpression {
	pub left: ExpressionNode,
	pub right: ExpressionNode,
	pub operator: BinaryOperator,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum BinaryOperator {
	Plus,
	Minus,
	Mul,
	Div,
	IntDiv,
	Modulo,
	Pow,
	And,
	Or,
	Equals,
	NotEquals,
	Less,
	LessEquals,
	Greater,
	GreaterEquals,
}

impl std::fmt::Display for BinaryOperator {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::Plus => f.write_str("+"),
			Self::Minus => f.write_str("-"),
			Self::Mul => f.write_str("*"),
			Self::Div => f.write_str("/"),
			Self::IntDiv => f.write_str("//"),
			Self::Modulo => f.write_str("%"),
			Self::Pow => f.write_str("**"),
			Self::And => f.write_str("&&"),
			Self::Or => f.write_str("||"),
			Self::Equals => f.write_str("=="),
			Self::NotEquals => f.write_str("!="),
			Self::Less => f.write_str("<"),
			Self::LessEquals => f.write_str("<="),
			Self::Greater => f.write_str(">"),
			Self::GreaterEquals => f.write_str(">="),
		}
	}
}

impl ConditionStatement {
	pub fn as_view(&self) -> ConditionStatementView<'_> {
		ConditionStatementView {
			cases: &self.cases,
			tail: &self.tail,
		}
	}
}

#[derive(Clone, Debug)]
pub struct AssignmentExpression {
	pub left: String,
	pub right: ExpressionNode,
}

#[derive(Clone, Debug)]
pub struct CallExpression {
	pub function: String,
	pub arguments: Vec<ArgumentItem>,
}

#[derive(Clone, Debug)]
pub enum ArgumentItem {
	Argument(ExpressionNode),
	Separator,
}

impl ExpressionNode {
	pub fn new(loc: Range<SourceLocation>, expression: Expression) -> Self {
		Self {
			loc,
			expression: Box::new(expression),
		}
	}
}

impl From<AssignmentExpression> for Expression {
	fn from(value: AssignmentExpression) -> Self {
		Self::Assignment(value)
	}
}

impl From<CallExpression> for Expression {
	fn from(value: CallExpression) -> Self {
		Self::Call(value)
	}
}

impl From<UnaryOperatorExpression> for Expression {
	fn from(value: UnaryOperatorExpression) -> Self {
		Self::UnaryOperator(value)
	}
}

impl From<BinaryOperatorExpression> for Expression {
	fn from(value: BinaryOperatorExpression) -> Self {
		Self::BinaryOperator(value)
	}
}
