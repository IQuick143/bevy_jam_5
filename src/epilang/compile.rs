use super::{lex::*, parser::*, Module};

/// Compiles a source program into a module
pub fn compile(source: &str) -> Result<Module, CompileError> {
	let mut lex_error = None;
	let tokens = lex(source).map_while(|t| {
		match t {
			Ok(token) => Some(token),
			Err(err) => {
				// If lexer fails, stop feeding tokens to the parser
				// and put the erorr aside
				lex_error = Some(err);
				None
			}
		}
	});
	let module = parse(tokens);
	// First check that lexer did not fail
	// (if it does, parser usualy fails too, so evaluate the errors in that order)
	if let Some(err) = lex_error {
		return Err(err.into());
	}
	Ok(module?)
}

#[derive(Clone, Debug)]
pub struct CompileError(CompileErrorData);

#[derive(Clone, Debug)]
enum CompileErrorData {
	Lex(LexerError),
	Parse(ParseError),
}

impl From<LexerError> for CompileError {
	fn from(value: LexerError) -> Self {
		Self(CompileErrorData::Lex(value))
	}
}

impl From<ParseError> for CompileError {
	fn from(value: ParseError) -> Self {
		Self(CompileErrorData::Parse(value))
	}
}

impl std::error::Error for CompileError {
	fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
		match &self.0 {
			CompileErrorData::Lex(e) => Some(e),
			CompileErrorData::Parse(e) => Some(e),
		}
	}
}

impl std::fmt::Display for CompileError {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		std::fmt::Display::fmt(
			std::error::Error::source(&self).expect("All compile errors should have a source"),
			f,
		)
	}
}
