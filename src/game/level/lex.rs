//! Primary parsing of level description files

use regex::Regex;

/// Line in the form `verb[modifier] values...`
#[derive(Clone, Debug)]
pub struct RawActionStatement<'a> {
	pub verb: &'a str,
	pub _modifier: Vec<&'a str>,
	pub values: Vec<&'a str>,
}

/// Line in the form `key=value`
#[derive(Clone, Copy, Debug)]
pub struct RawAssignmentStatement<'a> {
	pub key: &'a str,
	pub value: &'a str,
}

/// Primary parsed line of a level description file
#[derive(Clone, Debug)]
pub enum RawStatement<'a> {
	Action(RawActionStatement<'a>),
	Assignment(RawAssignmentStatement<'a>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum LexErrorCode {
	/// A line contains non-ascii characters
	NonAsciiLine,
	/// A line does not match either statement pattern
	MalformedStatement,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct LexError {
	pub code: LexErrorCode,
	pub line_number: usize,
}

pub fn parse(raw_data: &str) -> impl Iterator<Item = Result<(usize, RawStatement), LexError>> {
	let assignment_regex = Regex::new(r"^(?<KEY>[a-zA-Z0-9_]+)=(?<VALUE>.*)$")
		.expect("I expected to be able to write a valid regex.");
	let action_regex = Regex::new(r"^(?<VERB>\w+)(\[(?<MODIFIER>[\w\:]+)\])?(?<VALUES>.+)?$")
		.expect("I expected to be able to write a valid regex.");

	raw_data
		.split('\n')
		.map(&str::trim)
		.enumerate()
		.filter(|(_, line)| !(line.starts_with('#') || line.is_empty()))
		.map(move |(i, line)| {
			if !line.is_ascii() {
				Err(LexErrorCode::NonAsciiLine.at_line(i))
			} else if let Some(captures) = assignment_regex.captures(line) {
				let key = captures
					.name("KEY")
					.expect("KEY clause should always be present")
					.as_str();
				let value = captures
					.name("VALUE")
					.expect("VALUE clause should always be present")
					.as_str();
				Ok((
					i,
					RawStatement::Assignment(RawAssignmentStatement { key, value }),
				))
			} else if let Some(captures) = action_regex.captures(line) {
				Ok((
					i,
					RawStatement::Action(RawActionStatement {
						verb: captures
							.name("VERB")
							.expect("VERB clause should always be present")
							.as_str(),
						_modifier: captures
							.name("MODIFIER")
							.map(|x| x.as_str().split(':').collect())
							.unwrap_or_else(Vec::new),
						values: match captures.name("VALUES") {
							None => Vec::new(),
							Some(m) => m
								.as_str()
								.split_ascii_whitespace()
								.filter(|x| !x.is_empty())
								.collect(),
						},
					}),
				))
			} else {
				Err(LexErrorCode::MalformedStatement.at_line(i))
			}
		})
}

impl LexErrorCode {
	pub fn at_line(self, line_number: usize) -> LexError {
		LexError {
			code: self,
			line_number,
		}
	}
}

impl std::error::Error for LexError {}

impl std::fmt::Display for LexErrorCode {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::NonAsciiLine => write!(f, "Non-ascii characters found."),
			Self::MalformedStatement => {
				write!(f, "Line does not match the statement pattern.")
			}
		}
	}
}

impl std::fmt::Display for LexError {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "Line {}: {}", self.line_number + 1, self.code)
	}
}
