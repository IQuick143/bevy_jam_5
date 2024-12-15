use super::SourceLocation;
use logos::{Filter, Lexer, Logos};
use std::ops::Range;

pub fn tokenize(
	text: &str,
) -> impl Iterator<Item = Result<(Token, Range<SourceLocation>), LexerError>> + '_ {
	LexerWrapper(Token::lexer(text))
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct LexerError {
	pub loc: Range<SourceLocation>,
	pub slice: String,
	pub error_code: LexerErrorCode,
}

#[derive(Clone, PartialEq, Eq, Debug, Default)]
pub enum LexerErrorCode {
	#[default]
	Generic,
	IntParseFailure,
	FloatParseFailure,
}

impl std::error::Error for LexerError {}

impl std::error::Error for LexerErrorCode {}

impl std::fmt::Display for LexerError {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		f.write_fmt(format_args!(
			"{} at {}..{} '{}'",
			self.error_code, self.loc.start, self.loc.end, self.slice
		))
	}
}

impl std::fmt::Display for LexerErrorCode {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::Generic => f.write_str("Generic lexer error"),
			Self::FloatParseFailure => f.write_str("Invalid float literal"),
			Self::IntParseFailure => f.write_str("Invalid integer literal"),
		}
	}
}

impl From<core::num::ParseIntError> for LexerErrorCode {
	fn from(value: core::num::ParseIntError) -> Self {
		Self::IntParseFailure
	}
}

impl From<core::num::ParseFloatError> for LexerErrorCode {
	fn from(value: core::num::ParseFloatError) -> Self {
		Self::FloatParseFailure
	}
}

struct LexerWrapper<'a>(Lexer<'a, Token>);

impl LexerWrapper<'_> {
	fn get_source_location(&self) -> Range<SourceLocation> {
		let extras = self.0.extras;
		let span = self.0.span();
		let start = SourceLocation {
			line: extras.current_line,
			column: span.start - extras.line_start_offset,
		};
		let end = SourceLocation {
			line: extras.current_line,
			column: span.end - extras.line_start_offset,
		};
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

#[derive(Clone, Copy, PartialEq, Eq, Debug, Default)]
pub struct LexerSourceLocation {
	current_line: usize,
	line_start_offset: usize,
}

/// Non-instantiable dummy type to prevent instantiation
/// of a [`Token::Eol`] token type
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum EolToken {}

#[derive(Logos, Clone, PartialEq, Debug)]
#[logos(skip r"[ \t\r]")]
#[logos(extras = LexerSourceLocation)]
#[logos(error = LexerErrorCode)]
pub enum Token {
	#[regex(r"(#[^\n]*)?\n", |lex| {
		lex.extras.current_line += 1;
		lex.extras.line_start_offset = lex.span().end;
		Filter::Skip
	})]
	Eol(EolToken),

	#[regex(r"[a-zA-Z]\w*", |lex| lex.slice().to_owned())]
	Identifier(String),

	#[regex(r"\$\d+", |lex| lex.slice()[1..].parse())]
	#[token("$", |_| 0)]
	MacroParameter(u32),

	#[regex(r"\d+", |lex| lex.slice().parse(), priority = 3)]
	#[regex(r"0[xX][\da-fA-F]+", |lex| u32::from_str_radix(&lex.slice()[2..], 16))]
	IntLiteral(u32),

	/// Special token that represents a [`Token::IntLiteral`]
	/// followed by a [`Token::Dot`].
	///
	/// This token serves to provide a terminating state for this sequence,
	/// since Logos forbids backtracking, so this sequence would be interpreted
	/// as a [`Token::FloatLiteral`] and then fail
	#[regex(r"\d+\.", |lex| lex.slice().strip_suffix('.').unwrap().parse())]
	IntLiteralDot(u32),

	#[regex(r"(\d+|\d*\.\d+)([eE][+\-]?\d+)?", |lex| lex.slice().parse())]
	FloatLiteral(f32),

	#[regex(r"'[^'\n]'", |lex| lex.slice().to_owned())]
	StringLiteral(String),

	#[token("+")]
	Plus,

	#[token("-")]
	Minus,

	#[token("*")]
	Asterisk,

	#[token("/")]
	Slash,

	#[token("//")]
	DoubleSlash,

	#[token("%")]
	Percent,

	#[token("**")]
	Pow,

	#[token("!")]
	Not,

	#[token("&&")]
	And,

	#[token("||")]
	Or,

	#[token("<")]
	Less,

	#[token(">")]
	Greater,

	#[token("<=")]
	LessEquals,

	#[token(">=")]
	GreaterEquals,

	#[token("==")]
	Equals,

	#[token("!=")]
	NotEquals,

	#[token(".")]
	Dot,

	#[token("=")]
	Assign,

	#[token(",")]
	Comma,

	#[token(";")]
	Semicolon,

	#[token(":")]
	Colon,

	#[token("=>")]
	Lambda,

	#[token("(")]
	OpenParen,

	#[token(")")]
	CloseParen,

	#[token("[")]
	OpenBracket,

	#[token("]")]
	CloseBracket,

	#[token("{")]
	OpenBrace,

	#[token("}")]
	CloseBrace,

	#[token("_")]
	Blank,

	#[token("let")]
	Let,
}
