mod ast;
pub mod builtins;
mod compile;
pub mod interpreter;
mod lex;
mod parser;
#[cfg(test)]
mod test;
pub mod values;

pub use ast::Module;
pub use compile::{compile, CompileError};
pub use interpreter::{Interpreter, InterpreterEndState, InterpreterError};

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Default, Hash)]
pub struct SourceLocation {
	pub line: usize,
	pub column: usize,
}

impl SourceLocation {
	pub fn new(line: usize, column: usize) -> Self {
		Self { line, column }
	}
}

impl std::fmt::Display for SourceLocation {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		f.write_fmt(format_args!("{}:{}", self.line + 1, self.column + 1))
	}
}

impl std::fmt::Debug for SourceLocation {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{self}")
	}
}
