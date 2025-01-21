#![allow(unused)]
mod ast;
mod builtins;
mod compile;
mod interpreter;
mod lex;
mod parser;
#[cfg(test)]
mod test;
mod values;

pub use ast::{BinaryOperator, Module, UnaryOperator};
pub use builtins::*;
pub use compile::{compile, CompileError};
pub use interpreter::*;
pub use values::{DynVariableValue, VariableType, VariableValue};

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Default, Hash)]
pub struct SourceLocation {
	pub line: usize,
	pub column: usize,
}

impl std::fmt::Display for SourceLocation {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		f.write_fmt(format_args!("{}:{}", self.line + 1, self.column + 1))
	}
}
