use super::{asset::PlaybackSequenceBuilder, script::PlaybackPrimitive};
use crate::epilang::{builtins::NoDomainValue, interpreter::*};

impl InterpreterBackend for PlaybackSequenceBuilder {
	type Error = std::convert::Infallible;
	type Warning = std::convert::Infallible;
	type Value = NoDomainValue;

	fn call_function(
		&mut self,
		function_name: &str,
		args: ArgumentStream<Self::Value>,
		_warnings: WarningSink<Self::Warning>,
	) -> CallResult {
		match function_name {
			"wait" => self.call_wait(args),
			"turn" => self.call_turn(args),
			"load" => self.call_load(args),
			"select" => self.call_select(args),
			"select_turn" => self.call_select_turn(args),
			"end" => self.call_end(args),
			_ => Err(FunctionCallError::FunctionDoesNotExist),
		}
	}
}

type CallResult = Result<
	ReturnValue<'static, NoDomainValue>,
	FunctionCallError<std::convert::Infallible, NoDomainValue>,
>;

impl PlaybackSequenceBuilder {
	fn call_wait(&mut self, mut args: ArgumentStream<NoDomainValue>) -> CallResult {
		let seconds = args.read_single_as()?;
		self.current_sequence
			.push(PlaybackPrimitive::WaitSeconds(seconds));
		Ok(ReturnValue::void())
	}

	fn call_turn(&mut self, mut args: ArgumentStream<NoDomainValue>) -> CallResult {
		let target_cycle = args
			.read_as::<i32>()?
			.try_into()
			.map_err(|_| ArgumentError::InvalidValue)?;
		let amount = args.optional_read_as::<i32>().unwrap_or(1) as i64;
		args.read_end()?;
		self.current_sequence.push(PlaybackPrimitive::TurnCycle {
			target_cycle,
			amount,
		});
		Ok(ReturnValue::void())
	}

	fn call_select(&mut self, mut args: ArgumentStream<NoDomainValue>) -> CallResult {
		let target_cycle = args
			.optional_read_as::<i32>()
			.map(TryInto::try_into)
			.transpose()
			.map_err(|_| ArgumentError::InvalidValue)?;
		args.read_end()?;
		self.current_sequence
			.push(PlaybackPrimitive::SelectCycle(target_cycle));
		Ok(ReturnValue::void())
	}

	fn call_load(&mut self, mut args: ArgumentStream<NoDomainValue>) -> CallResult {
		let level_file = args.read_single_as::<&str>()?.to_owned();
		self.current_sequence
			.push(PlaybackPrimitive::EnterLevel(level_file));
		Ok(ReturnValue::void())
	}

	fn call_end(&mut self, args: ArgumentStream<NoDomainValue>) -> CallResult {
		args.read_end()?;
		self.flush_sequence();
		Ok(ReturnValue::void())
	}

	fn call_select_turn(&mut self, mut args: ArgumentStream<NoDomainValue>) -> CallResult {
		let target_cycle = args
			.read_as::<i32>()?
			.try_into()
			.map_err(|_| ArgumentError::InvalidValue)?;
		let amount = args.optional_read_as::<i32>().unwrap_or(1) as i64;
		args.read_end_or_separator()?;
		let delays = std::iter::from_fn(|| args.read_as_until_end_or_separator().transpose())
			.collect::<Result<Vec<_>, _>>()?;
		args.read_end()?;

		self.current_sequence
			.push(PlaybackPrimitive::SelectCycle(Some(target_cycle)));
		for seconds in delays {
			self.current_sequence
				.push(PlaybackPrimitive::WaitSeconds(seconds));
			self.current_sequence.push(PlaybackPrimitive::TurnCycle {
				target_cycle,
				amount,
			});
		}

		Ok(ReturnValue::void())
	}
}
