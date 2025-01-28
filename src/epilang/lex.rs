use super::SourceLocation;
use logos::Logos;
use std::ops::Range;

pub fn lex(
	source: &str,
) -> impl Iterator<Item = Result<(Token, Range<SourceLocation>), LexerError>> + '_ {
	LexerWrapper(Token::lexer(source))
}

#[derive(Clone, Debug)]
pub struct LexerError {
	pub error_code: LexerErrorCode,
	pub slice: String,
	pub loc: Range<SourceLocation>,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, Default)]
pub enum LexerErrorCode {
	#[default]
	Generic,
	IntParseFailure,
	FloatParseFailure,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, Default)]
pub struct LexerSourceLocation {
	line_index: usize,
	line_start_offset: usize,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum NeverTag {}

#[derive(Logos, Clone, PartialEq, Debug)]
#[logos(skip r"[ \t\r]")]
#[logos(extras = LexerSourceLocation)]
#[logos(error = LexerErrorCode)]
pub enum Token {
	#[regex(r"(#[^\n]*)?\n", |lex| {
		lex.extras.line_index += 1;
		lex.extras.line_start_offset = lex.span().end;
		logos::Filter::Skip
	})]
	Eol(NeverTag),

	#[regex(r"[a-zA-Z][a-zA-Z0-9_]*", |lex| lex.slice().to_owned())]
	Identifier(String),

	#[regex(r"\d+", |lex| lex.slice().parse())]
	#[regex(r"0[Xx][0-9a-fA-F]+", |lex| i32::from_str_radix(&lex.slice()[2..], 16))]
	IntLiteral(i32),

	#[regex(r"(\d*\.\d+|\d+\.)([eE][+\-]?\d+)?|\d+[eE][+\-]\d+", |lex| lex.slice().parse())]
	FloatLiteral(f32),

	#[regex(r"'[^'\n]*'", |lex| lex.slice()[1..lex.slice().len() - 1].to_owned())]
	StringLiteral(String),

	#[token("_")]
	Blank,
	#[token("if")]
	If,
	#[token("else")]
	Else,
	#[token("fn")]
	Fn,
	#[token("=")]
	Assign,
	#[token("(")]
	OpenParen,
	#[token(")")]
	CloseParen,
	#[token("{")]
	OpenBrace,
	#[token("}")]
	CloseBrace,
	#[token(",")]
	Comma,
	#[token(";")]
	Semicolon,
	#[token("+")]
	Plus,
	#[token("-")]
	Minus,
	#[token("*")]
	Mul,
	#[token("/")]
	Div,
	#[token("//")]
	IntDiv,
	#[token("%")]
	Modulo,
	#[token("**")]
	Pow,
	#[token("&&")]
	And,
	#[token("||")]
	Or,
	#[token("!")]
	Not,
	#[token("==")]
	Equals,
	#[token("!=")]
	NotEquals,
	#[token("<")]
	Less,
	#[token("<=")]
	LessEquals,
	#[token(">")]
	Greater,
	#[token(">=")]
	GreaterEquals,
}

struct LexerWrapper<'a>(logos::Lexer<'a, Token>);

impl LexerWrapper<'_> {
	fn get_source_location(&self) -> Range<SourceLocation> {
		let extras = self.0.extras;
		let span = self.0.span();
		let start = SourceLocation::new(extras.line_index, span.start - extras.line_start_offset);
		let end = SourceLocation::new(extras.line_index, span.end - extras.line_start_offset);
		start..end
	}
}

impl Iterator for LexerWrapper<'_> {
	type Item = Result<(Token, Range<SourceLocation>), LexerError>;
	fn next(&mut self) -> Option<Self::Item> {
		self.0.next().map(|r| match r {
			Ok(token) => Ok((token, self.get_source_location())),
			Err(err) => Err(LexerError {
				error_code: err,
				slice: self.0.slice().to_owned(),
				loc: self.get_source_location(),
			}),
		})
	}
}

impl From<std::num::ParseIntError> for LexerErrorCode {
	fn from(_: std::num::ParseIntError) -> Self {
		Self::IntParseFailure
	}
}

impl From<std::num::ParseFloatError> for LexerErrorCode {
	fn from(_: std::num::ParseFloatError) -> Self {
		Self::FloatParseFailure
	}
}

impl std::error::Error for LexerError {}

impl std::fmt::Display for LexerErrorCode {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::Generic => f.write_str("Generic lexer error"),
			Self::FloatParseFailure => f.write_str("Invalid float literal"),
			Self::IntParseFailure => f.write_str("Invalid integer literal"),
		}
	}
}

impl std::fmt::Display for LexerError {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		f.write_fmt(format_args!(
			"{} at {}..{} {:?}",
			self.error_code, self.loc.start, self.loc.end, self.slice
		))
	}
}
