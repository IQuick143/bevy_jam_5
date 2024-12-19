// Clippy keeps complaining about some let statement in macro-generated code
#![allow(clippy::let_unit_value)]

use super::{ast::*, lex::*, SourceLocation};
use pomelo::pomelo;
use std::ops::Range;

pub fn parse(
	tokens: impl IntoIterator<Item = (Token, Range<SourceLocation>)>,
) -> Result<Sequence, ParserError> {
	let mut parser = pomelo_parser::Parser::new();
	for (token, loc) in tokens {
		parser.parse(token.into_pomelo_token(loc))?;
	}
	parser.end_of_input()
}

#[derive(Clone, PartialEq, Debug, Default)]
pub enum ParserError {
	#[default]
	Generic,
	UnexpectedToken(ParserTokenError),
	UnexpectedEoi,
	StackOverflow,
}

#[derive(Clone, PartialEq, Debug)]
pub struct ParserTokenError {
	pub token: Token,
	pub loc: Range<SourceLocation>,
}

impl std::error::Error for ParserError {}

impl std::fmt::Display for ParserError {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::Generic => f.write_str("Generic parser error"),
			Self::UnexpectedToken(e) => f.write_fmt(format_args!(
				"Unexpected token {:?} at {}..{}",
				e.token, e.loc.start, e.loc.end
			)),
			Self::UnexpectedEoi => f.write_str("Unexpected end of input"),
			Self::StackOverflow => f.write_str("Parser stack overflow"),
		}
	}
}

pomelo! {
	%module pomelo_parser;
	%include { use super::*; }

	%error ParserError;
	%stack_overflow { ParserError::StackOverflow }
	%syntax_error {
		match token {
			None => Err(ParserError::UnexpectedEoi),
			Some(token) => Err(ParserError::UnexpectedToken(token.into()))
		}
	}

	// Underlying types of non-terminals
	%type program Sequence;
	%type sequence Sequence;
	%type nonemptysequence Sequence;
	%type expr Expression;
	%type bracedseq Expression;
	%type parglist (Range<SourceLocation>, ArgumentList);
	%type arglist ArgumentList;
	%type seqarglist Vec<Expression>;
	%type namedarglist Vec<NamedArgument>;
	%type namedarg NamedArgument;
	%type capturelist (Range<SourceLocation>, Vec<String>);
	%type namelist Vec<String>;

	// Underlying types of terminals
	%type IntLiteral u32;
	%type IntLiteralDot u32;
	%type FloatLiteral f32;
	%type StringLiteral String;
	%type Identifier String;
	%type MacroParameter u32;

	// Extra parameter included with every terminal
	%extra_token Range<SourceLocation>;

	// Operator table, from lowest to highest precedence
	%right Lambda;
	%right Assign;
	%left Or;
	%left And;
	%left Equals NotEquals;
	%left Less Greater LessEquals GreaterEquals;
	%left Plus Minus;
	%left Asterisk Slash DoubleSlash Percent;
	%right Pow;
	%nonassoc Not; // Unary plus and minus also go here (their rules' precedence is set explicitly)
	%left Dot OpenParen OpenBracket OpenBrace;

	program ::= sequence;
	sequence ::= { Sequence::new() }
	sequence ::= nonemptysequence;
	nonemptysequence ::= expr(e) { Sequence::new().push_tail(Some(e)) }
	nonemptysequence ::= nonemptysequence(s) Semicolon expr?(e) { s.push_tail(e) }
	expr ::= IntLiteral((loc, val)) { Expression::new(loc, ExpressionContent::IntLiteral(val)) }
	expr ::= FloatLiteral((loc, val)) { Expression::new(loc, ExpressionContent::FloatLiteral(val)) }
	expr ::= StringLiteral((loc, val)) { Expression::new(loc, ExpressionContent::StringLiteral(val)) }
	expr ::= Identifier((loc, val)) { Expression::new(loc, ExpressionContent::Identifier(val)) }
	expr ::= MacroParameter((loc, val)) { Expression::new(loc, ExpressionContent::MacroParameter(val)) }
	expr ::= Let(start_loc) Identifier((end_loc, val)) {
		Expression::new(start_loc.start..end_loc.end, ExpressionContent::LetIdentifier(val))
	}
	expr ::= bracedseq;
	bracedseq ::= OpenBrace(start_loc) sequence(seq) CloseBrace(end_loc) {
		Expression::new(start_loc.start..end_loc.end, ExpressionContent::Sequence(seq))
	}
	expr ::= expr(left) Assign expr(right) {
		Expression::new(left.loc.start..right.loc.end, ExpressionContent::Assignment(left, right))
	}
	expr ::= Blank(loc) { Expression::new(loc, ExpressionContent::Blank) }
	expr ::= OpenParen(start_loc) expr(inner) CloseParen(end_loc) {
		Expression {
			loc: start_loc.start..end_loc.end,
			value: inner.value,
		}
	}
	expr ::= expr(left) Dot Identifier((field_loc, field)) parglist?(arglist) {
		Expression::new(
			left.loc.start..arglist.as_ref().map(|p| p.0.end).unwrap_or(field_loc.end),
			ExpressionContent::MethodCall(left, field, arglist.map(|p| p.1)),
		)
	}
	expr ::= IntLiteralDot((start_loc, left)) Identifier((field_loc, field)) parglist?(arglist) {
		let left_expr = Expression::new(start_loc.clone(), ExpressionContent::IntLiteral(left));
		Expression::new(
			start_loc.start..arglist.as_ref().map(|p| p.0.end).unwrap_or(field_loc.end),
			ExpressionContent::MethodCall(left_expr, field, arglist.map(|p| p.1)),
		)
	}
	expr ::= Identifier((start_loc, name)) parglist((end_loc, args)) {
		Expression::new(start_loc.start..end_loc.end, ExpressionContent::MacroCall(name, args))
	}
	parglist ::= OpenParen(start_loc) arglist(args) CloseParen(end_loc) {
		(start_loc.start..end_loc.end, args)
	}
	parglist ::= bracedseq(arg) {
		(arg.loc.clone(), ArgumentList::new(vec![arg], Vec::new()))
	}
	arglist ::= { Default::default() }
	arglist ::= namedarglist(args) Comma? { ArgumentList::new(Vec::new(), args) }
	arglist ::= seqarglist(args) Comma? { ArgumentList::new(args, Vec::new()) }
	arglist ::= seqarglist(positional) Comma namedarglist(named) Comma? {
		ArgumentList::new(positional, named)
	}
	namedarglist ::= namedarg(arg) { vec![arg] }
	namedarglist ::= namedarglist(mut args) Comma namedarg(arg) { args.push(arg); args }
	seqarglist ::= expr(arg) { vec![arg] }
	seqarglist ::= seqarglist(mut args) Comma expr(arg) { args.push(arg); args }
	namedarg ::= Identifier((_, name)) Colon expr(value) { NamedArgument { name, value } }
	expr ::= expr(array) OpenBracket expr(index) CloseBracket(end_loc) {
		Expression::new(array.loc.start..end_loc.end, ExpressionContent::IndexAccess(array, index))
	}
	expr ::= OpenBracket(start_loc) CloseBracket(end_loc) {
		Expression::new(start_loc.start..end_loc.end, ExpressionContent::ArrayLiteral(Vec::new()))
	}
	expr ::= OpenBracket(start_loc) seqarglist(values) CloseBracket(end_loc) {
		Expression::new(start_loc.start..end_loc.end, ExpressionContent::ArrayLiteral(values))
	}
	expr ::= OpenBracket(start_loc) expr(value) Semicolon expr(repeat) CloseBracket(end_loc) {
		Expression::new(start_loc.start..end_loc.end, ExpressionContent::FillArrayLiteral(value, repeat))
	}
	expr ::= capturelist((start_loc, caps)) Lambda expr(body) {
		Expression::new(start_loc.start..body.loc.end, ExpressionContent::Lambda(caps, body))
	}
	capturelist ::= Identifier((loc, name)) { (loc, vec![name]) }
	capturelist ::= OpenParen(start_loc) namelist(names) CloseParen(end_loc) {
		(start_loc.start..end_loc.end, names)
	}
	namelist ::= Identifier((_, first)) Comma Identifier((_, second)) { vec![first, second] }
	namelist ::= namelist(mut params) Comma Identifier((_, next)) { params.push(next); params }
	expr ::= Plus(start_loc) expr(operand) [Not] {
		Expression::new(start_loc.start..operand.loc.end, ExpressionContent::Operation(OperationExpression::UnaryPlus(operand)))
	}
	expr ::= Minus(start_loc) expr(operand) [Not] {
		Expression::new(start_loc.start..operand.loc.end, ExpressionContent::Operation(OperationExpression::UnaryMinus(operand)))
	}
	expr ::= Not(start_loc) expr(operand) {
		Expression::new(start_loc.start..operand.loc.end, ExpressionContent::Operation(OperationExpression::Not(operand)))
	}
	expr ::= expr(left) Plus expr(right) {
		Expression::new(left.loc.start..right.loc.end, ExpressionContent::Operation(OperationExpression::BinaryPlus(left, right)))
	}
	expr ::= expr(left) Minus expr(right) {
		Expression::new(left.loc.start..right.loc.end, ExpressionContent::Operation(OperationExpression::BinaryMinus(left, right)))
	}
	expr ::= expr(left) Asterisk expr(right) {
		Expression::new(left.loc.start..right.loc.end, ExpressionContent::Operation(OperationExpression::Multiply(left, right)))
	}
	expr ::= expr(left) Slash expr(right) {
		Expression::new(left.loc.start..right.loc.end, ExpressionContent::Operation(OperationExpression::Divide(left, right)))
	}
	expr ::= expr(left) DoubleSlash expr(right) {
		Expression::new(left.loc.start..right.loc.end, ExpressionContent::Operation(OperationExpression::IntegerDivide(left, right)))
	}
	expr ::= expr(left) Percent expr(right) {
		Expression::new(left.loc.start..right.loc.end, ExpressionContent::Operation(OperationExpression::Modulo(left, right)))
	}
	expr ::= expr(left) Pow expr(right) {
		Expression::new(left.loc.start..right.loc.end, ExpressionContent::Operation(OperationExpression::Power(left, right)))
	}
	expr ::= expr(left) And expr(right) {
		Expression::new(left.loc.start..right.loc.end, ExpressionContent::Operation(OperationExpression::And(left, right)))
	}
	expr ::= expr(left) Or expr(right) {
		Expression::new(left.loc.start..right.loc.end, ExpressionContent::Operation(OperationExpression::Or(left, right)))
	}
	expr ::= expr(left) Equals expr(right) {
		Expression::new(left.loc.start..right.loc.end, ExpressionContent::Operation(OperationExpression::CompareEqual(left, right)))
	}
	expr ::= expr(left) NotEquals expr(right) {
		Expression::new(left.loc.start..right.loc.end, ExpressionContent::Operation(OperationExpression::CompareNotEqual(left, right)))
	}
	expr ::= expr(left) Less expr(right) {
		Expression::new(left.loc.start..right.loc.end, ExpressionContent::Operation(OperationExpression::CompareLess(left, right)))
	}
	expr ::= expr(left) Greater expr(right) {
		Expression::new(left.loc.start..right.loc.end, ExpressionContent::Operation(OperationExpression::CompareGreater(left, right)))
	}
	expr ::= expr(left) LessEquals expr(right) {
		Expression::new(left.loc.start..right.loc.end, ExpressionContent::Operation(OperationExpression::CompareLessEqual(left, right)))
	}
	expr ::= expr(left) GreaterEquals expr(right) {
		Expression::new(left.loc.start..right.loc.end, ExpressionContent::Operation(OperationExpression::CompareGreaterEqual(left, right)))
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

		impl From<pomelo_parser::Token> for ParserTokenError {
			fn from(value: pomelo_parser::Token) -> Self {
				match value {
					$( pomelo_parser::Token::$variant((loc $(, $param)?)) => Self {
						loc,
						token: Token::$variant $(($param))?
					} ),*
				}
			}
		}
	};
}

pomelo_token_conversions! {
	Identifier(name),
	IntLiteral(value),
	IntLiteralDot(value),
	FloatLiteral(value),
	StringLiteral(value),
	MacroParameter(index),
	Blank,
	Let,
	Plus,
	Minus,
	Asterisk,
	Slash,
	DoubleSlash,
	Percent,
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
	OpenBracket,
	CloseBracket,
	OpenBrace,
	CloseBrace,
	Assign,
	Dot,
	Comma,
	Colon,
	Semicolon,
	Lambda,
}
