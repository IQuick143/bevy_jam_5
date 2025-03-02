//! Epilang backend for [`LevelBuilder`]

mod finalize;
mod runtime;
#[cfg(test)]
mod test;

use super::{
	super::{builder::LevelBuilder, LevelData},
	domain::DomainType,
	MAX_INTERPRETER_ITERATIONS,
};
use crate::epilang::{interpreter::InterpreterWarning, *};

pub type Error =
	super::Error<InterpreterError<runtime::RuntimeError, DomainType>, finalize::FinalizeError>;
pub type Warning =
	super::Warning<InterpreterWarning<runtime::RuntimeWarning>, finalize::FinalizeWarning>;

pub fn run(
	module: &Module,
	mut warning_handler: impl FnMut(Warning),
) -> (Option<LevelData>, Option<Error>) {
	let mut interpreter = Interpreter::new(module, LevelBuilder::new());
	interpreter.variable_pool = builtins::default_builtin_variables();
	match interpreter.run(MAX_INTERPRETER_ITERATIONS) {
		InterpreterEndState::Timeout => return (None, Some(Error::Timeout)),
		InterpreterEndState::Halted(Ok(())) => {}
		InterpreterEndState::Halted(Err(err)) => return (None, Some(Error::Runtime(err))),
	}
	for warning in interpreter.get_warnings() {
		warning_handler(Warning::Runtime(warning));
	}
	let (level, err) = finalize::finalize(interpreter.backend, &interpreter.variable_pool, |w| {
		warning_handler(Warning::Finalize(w))
	});
	(level, err.map(Error::Finalize))
}

pub fn parse_and_run(
	level_file: &str,
	warning_handler: impl FnMut(Warning),
) -> (Option<LevelData>, Option<Error>) {
	match compile(level_file) {
		Ok(module) => run(&module, warning_handler),
		Err(err) => (None, Some(Error::Compile(err))),
	}
}
