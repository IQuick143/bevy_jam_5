#![allow(clippy::let_unit_value)]

use super::{ast::*, lex::*, SourceLocation};
use pomelo::pomelo;
use std::ops::Range;

pub fn parse(
	tokens: impl IntoIterator<Item = (Token, Range<SourceLocation>)>,
) -> Result<Module, ParseError> {
	let mut parser = pomelo_parser::Parser::new();
	for (token, loc) in tokens {
		parser.parse(token.into_pomelo_token(loc))?;
	}
	parser.end_of_input()
}

#[derive(Clone, PartialEq, Debug, Default)]
pub enum ParseError {
	#[default]
	Generic,
	UnexpectedToken(UnexpectedTokenError),
	UnexpectedEoi,
	StackOverflow,
}

#[derive(Clone, PartialEq, Debug)]
pub struct UnexpectedTokenError {
	pub token: Token,
	pub loc: Range<SourceLocation>,
}

macro_rules! add_operator_rules {
	(
		$macro:ident! { $($pom:tt)* }
		UnaryOperator { $($uname:ident $([$uprec:ident])?),* $(,)? }
		BinaryOperator { $($bname:ident),* $(,)? }
	) => {
		$macro! {
			$($pom)*
			$(
				expr ::= $uname(start_loc) expr(expr) $([$uprec])? {
					ExpressionNode::new(start_loc.start..expr.loc.end, UnaryOperatorExpression {
						operand: expr,
						operator: UnaryOperator::$uname,
					}.into())
				}
			)*
			$(
				expr ::= expr(left) $bname expr(right) {
					ExpressionNode::new(left.loc.start..right.loc.end, BinaryOperatorExpression {
						left,
						right,
						operator: BinaryOperator::$bname,
					}.into())
				}
			)*
		}
	};
}

add_operator_rules! {
	pomelo! {
		%module pomelo_parser;
		%include { use super::*; }

		%error ParseError;
		%stack_overflow { ParseError::StackOverflow }
		%syntax_error {
			match token {
				None => Err(ParseError::UnexpectedEoi),
				Some(token) => Err(ParseError::UnexpectedToken(token.into()))
			}
		}

		// Underlying types of terminals
		%type Identifier String;
		%type IntLiteral i32;
		%type FloatLiteral f32;
		%type StringLiteral String;

		// Extra parameter included with every terminal
		%extra_token Range<SourceLocation>;

		// Operator precedence
		%right Assign;
		%left And Or;
		%left Equals NotEquals;
		%left Less LessEquals Greater GreaterEquals;
		%left Plus Minus;
		%left Mul Div IntDiv Modulo;
		%right Pow;
		%nonassoc Not;

		%type program Module;
		%type block Block;
		program ::= block(block) { Module(block) };
		block ::= { Block(Vec::new()) }
		block ::= block(mut block) statement(new_statement) { block.0.push(new_statement); block }

		%type statement Statement;
		statement ::= expr(expr) Semicolon { Statement::Expression(expr) }
		statement ::= condition(cond) { Statement::Condition(cond) }
		%type condition ConditionStatement;
		%type condition_head Vec<ConditionCase>;
		%type condition_tail Block;
		condition ::= condition_head(cases) condition_tail?(tail) { ConditionStatement { cases, tail } }
		condition_head ::= If expr(condition) OpenBrace block(handler) CloseBrace {
			vec![ConditionCase { condition, handler }]
		}
		condition_head ::= condition_head(mut cases) Else If expr(condition) OpenBrace block(handler) CloseBrace {
			cases.push(ConditionCase { condition, handler });
			cases
		}
		condition_tail ::= Else OpenBrace block(tail) CloseBrace { tail }

		%type expr ExpressionNode;
		expr ::= OpenParen expr(expr) CloseParen { expr }
		expr ::= Blank(loc) { ExpressionNode::new(loc, Expression::BlankLiteral) }
		expr ::= IntLiteral((loc, value)) { ExpressionNode::new(loc, Expression::IntLiteral(value)) }
		expr ::= FloatLiteral((loc, value)) { ExpressionNode::new(loc, Expression::FloatLiteral(value)) }
		expr ::= StringLiteral((loc, value)) { ExpressionNode::new(loc, Expression::StringLiteral(value)) }
		expr ::= Identifier((loc, name)) { ExpressionNode::new(loc, Expression::VariableValue(name)) }

		expr ::= Fn(start_loc) OpenBrace block(block) CloseBrace(end_loc) {
			ExpressionNode::new(start_loc.start..end_loc.end, Expression::Callback(Module(block)))
		}

		expr ::= Identifier((start_loc, left)) Assign expr(right) {
			ExpressionNode::new(start_loc.start..right.loc.end, AssignmentExpression { left, right }.into())
		}

		expr ::= Identifier((start_loc, function)) OpenParen callargs(arguments) CloseParen(end_loc) {
			ExpressionNode::new(start_loc.start..end_loc.end, CallExpression { function, arguments }.into())
		}
		%type callargs Vec<ArgumentItem>;
		%type callargs_expr Vec<ArgumentItem>;
		%type callargs_comma Vec<ArgumentItem>;
		%type callargs_semi Vec<ArgumentItem>;
		callargs ::= { Vec::new() }
		callargs ::= callargs_expr(args) { args }
		callargs ::= callargs_comma(args) { args }
		callargs ::= callargs_semi(args) { args }
		callargs_expr ::= expr(expr) { vec![ArgumentItem::Argument(expr)] }
		callargs_expr ::= callargs_comma(mut args) expr(expr) { args.push(ArgumentItem::Argument(expr)); args }
		callargs_expr ::= callargs_semi(mut args) expr(expr) { args.push(ArgumentItem::Argument(expr)); args }
		callargs_comma ::= callargs_expr(args) Comma { args }
		callargs_semi ::= Semicolon { vec![ArgumentItem::Separator] }
		callargs_semi ::= callargs_expr(mut args) Semicolon { args.push(ArgumentItem::Separator); args }
		callargs_semi ::= callargs_semi(mut args) Semicolon { args.push(ArgumentItem::Separator); args }
	}
	UnaryOperator {
		Not,
		Plus [Not],
		Minus [Not],
	}
	BinaryOperator {
		Plus,
		Minus,
		Mul,
		Div,
		IntDiv,
		Modulo,
		Pow,
		Equals,
		NotEquals,
		Less,
		LessEquals,
		Greater,
		GreaterEquals,
		And,
		Or,
	}
}

impl std::error::Error for ParseError {}

impl std::fmt::Display for ParseError {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::Generic => f.write_str("Generic parser error"),
			Self::UnexpectedToken(e) => f.write_fmt(format_args!("{e}")),
			Self::UnexpectedEoi => f.write_str("Unexpected end of input"),
			Self::StackOverflow => f.write_str("Parser stack overflow"),
		}
	}
}

impl std::fmt::Display for UnexpectedTokenError {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		f.write_fmt(format_args!(
			"Unexpected token {:?} at {}..{}",
			self.token, self.loc.start, self.loc.end
		))
	}
}

macro_rules! pomelo_token_conversions {
	( $($variant:ident $(($param:ident))? ),* $(,)? ) => {
		impl Token {
			fn into_pomelo_token(self, loc: Range<SourceLocation>) -> pomelo_parser::Token {
				match self {
					$( Self::$variant $(($param))? => pomelo_parser::Token::$variant((loc $(, $param)?)) ),*
				}
			}
		}

		impl From<pomelo_parser::Token> for UnexpectedTokenError {
			fn from(value: pomelo_parser::Token) -> Self {
				match value {
					$(
						#[allow(unused_parens)]
						pomelo_parser::Token::$variant((loc $(, $param)?)) => Self {
							loc,
							token: Token::$variant $(($param))?
						}
					),*
				}
			}
		}
	};
}

pomelo_token_conversions! {
	Identifier(name),
	IntLiteral(value),
	FloatLiteral(value),
	StringLiteral(value),
	Blank,
	If,
	Else,
	Fn,
	Plus,
	Minus,
	Mul,
	Div,
	IntDiv,
	Modulo,
	Pow,
	Not,
	And,
	Or,
	Equals,
	NotEquals,
	Less,
	Greater,
	LessEquals,
	GreaterEquals,
	OpenParen,
	CloseParen,
	OpenBrace,
	CloseBrace,
	Assign,
	Comma,
	Semicolon,
}
