use super::SourceLocation;
use logos::Logos;
use std::ops::Range;

pub fn lex(
	source: &str,
) -> impl Iterator<Item = Result<(Token, Range<SourceLocation>), LexerError>> + '_ {
	LexerWrapper(Token::lexer(source))
}

#[derive(Clone, PartialEq, Eq, Debug)]
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
	InvalidEscapeCharacter(char),
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

	#[regex(r"'([^'\n]|'')*'", |lex| {
		parse_escape_sequences_single_quoted(&lex.slice()[1..lex.slice().len() - 1])
	})]
	#[regex(r#""([^"\n\\]|\\[^\n])*""#, |lex| {
		parse_escape_sequences_double_quoted(&lex.slice()[1..lex.slice().len() - 1])
	})]
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

/// Error that indicates an invalid escape sequence
/// in a double-quoted string literal
#[derive(Clone, PartialEq, Eq, Debug)]
struct InvalidEscapeCharacter(char);

impl From<InvalidEscapeCharacter> for LexerErrorCode {
	fn from(value: InvalidEscapeCharacter) -> Self {
		Self::InvalidEscapeCharacter(value.0)
	}
}

/// Parses escape sequences of a single-quoted string literal
/// and creates a real string representation of it
fn parse_escape_sequences_single_quoted(s: &str) -> String {
	let mut r = String::with_capacity(s.len());
	let mut skip_next = false;
	for c in s.chars() {
		if !skip_next {
			r.push(c);
			if c == '\'' {
				skip_next = true;
			}
		} else {
			skip_next = false;
		}
	}
	r
}

/// Parses escape sequences of a double-quoted string literal
/// and creates a real string representation of it
fn parse_escape_sequences_double_quoted(s: &str) -> Result<String, InvalidEscapeCharacter> {
	let mut r = String::with_capacity(s.len());
	let mut escape_sequence = false;
	for c in s.chars() {
		if !escape_sequence {
			if c == '\\' {
				escape_sequence = true;
			} else {
				r.push(c);
			}
		} else {
			match c {
				'\\' | '"' => r.push(c),
				'n' => r.push('\n'),
				't' => r.push('\t'),
				_ => return Err(InvalidEscapeCharacter(c)),
			}
			escape_sequence = false;
		}
	}
	Ok(r)
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
			Self::InvalidEscapeCharacter(c) => write!(f, "invalid escape sequence: '\\{c}'"),
		}
	}
}

impl std::fmt::Display for LexerError {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		f.write_fmt(format_args!(
			"{} at {} {:?}",
			self.error_code, self.loc.start, self.slice,
		))
	}
}
