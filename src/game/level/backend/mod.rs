//! Epilang backends for various level design aspects

pub mod builder;
pub mod domain;
pub mod level_pack;

use crate::epilang::CompileError;

pub const MAX_INTERPRETER_ITERATIONS: u32 = 2 << 20;

/// Error type returned by a full interpreter run
#[derive(Clone, PartialEq, Debug)]
pub enum Error<R: std::error::Error, F: std::error::Error> {
	/// Source code could not compile
	Compile(CompileError),
	/// The interpreter has timed out
	Timeout,
	/// An error occurred while the interpreter ran
	Runtime(R),
	/// An error occurred in the finalization procedure
	Finalize(F),
}

impl<R: std::error::Error, F: std::error::Error> std::error::Error for Error<R, F> {}

impl<R: std::error::Error, F: std::error::Error> std::fmt::Display for Error<R, F> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::Compile(e) => write!(f, "{e}"),
			Self::Timeout => f.write_str("interpreter timed out"),
			Self::Runtime(e) => write!(f, "{e}"),
			Self::Finalize(e) => write!(f, "{e}"),
		}
	}
}

impl<R: std::error::Error, F: std::error::Error> From<CompileError> for Error<R, F> {
	fn from(value: CompileError) -> Self {
		Self::Compile(value)
	}
}

/// Warning type emited by a full interpreter run
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Warning<R: std::error::Error, F: std::error::Error> {
	/// A warning emited while the interpreter ran
	Runtime(R),
	/// A warning emited by the finalization procedure
	Finalize(F),
}

impl<R: std::error::Error, F: std::error::Error> std::error::Error for Warning<R, F> {}

impl<R: std::error::Error, F: std::error::Error> std::fmt::Display for Warning<R, F> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::Runtime(e) => write!(f, "{e}"),
			Self::Finalize(e) => write!(f, "{e}"),
		}
	}
}
