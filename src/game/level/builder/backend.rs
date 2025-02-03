use super::error::*;
use super::*;

use crate::epilang::{
	self,
	interpreter::{
		ArgumentError::{self, *},
		FunctionCallError::*,
		*,
	},
	values::*,
};
use itertools::Itertools as _;
use std::f32::consts::PI;

const MAX_INTERPRETER_ITERATIONS: u32 = 2000;

pub fn parse(
	level_file: &str,
	mut warning_handler: impl FnMut(LevelParsingWarning),
) -> Result<LevelData, LevelParsingError> {
	let module = epilang::compile(level_file)?;
	let mut interpreter = epilang::Interpreter::new(&module, LevelBuilder::new());
	interpreter.variable_pool = epilang::builtins::default_builtin_variables();
	match interpreter.run(MAX_INTERPRETER_ITERATIONS) {
		epilang::InterpreterEndState::Timeout => return Err(LevelParsingError::Timeout),
		epilang::InterpreterEndState::Halted(result) => result?,
	}
	for warning in interpreter.get_warnings() {
		warning_handler(LevelParsingWarning::RuntimeWarning(warning));
	}
	if let Some(level_name) = interpreter
		.variable_pool
		.load_as::<&str>("name")
		.transpose()?
	{
		interpreter.backend.set_level_name(level_name.to_owned())?;
	} else {
		warning_handler(LevelParsingWarning::LevelNameNotSet);
	}
	if let Some(level_hint) = interpreter
		.variable_pool
		.load_as::<&str>("hint")
		.transpose()?
	{
		interpreter.backend.set_level_hint(level_hint.to_owned())?;
	}
	Ok(interpreter.backend.build()?)
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct VertexId(pub usize);

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct CycleId(pub usize);

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum DomainValue {
	Vertex(VertexId),
	Cycle(CycleId),
	Object(ObjectData),
	Glyph(GlyphData),
	Color(LogicalColor),
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum DomainType {
	Vertex,
	Cycle,
	Object,
	Glyph,
	Color,
}

impl std::fmt::Display for DomainType {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::Vertex => f.write_str("vertex"),
			Self::Cycle => f.write_str("cycle"),
			Self::Object => f.write_str("object"),
			Self::Glyph => f.write_str("glyph"),
			Self::Color => f.write_str("color"),
		}
	}
}

impl epilang::values::DomainVariableValue for DomainValue {
	type Type = DomainType;

	fn get_type(&self) -> Self::Type {
		match self {
			Self::Vertex(_) => Self::Type::Vertex,
			Self::Cycle(_) => Self::Type::Cycle,
			Self::Object(_) => Self::Type::Object,
			Self::Glyph(_) => Self::Type::Glyph,
			Self::Color(_) => Self::Type::Color,
		}
	}
}

macro_rules! impl_try_into_for_domain_value {
	( $( $variant:ident ( $type:ty ) $({ $($extra_pat:pat => $extra_expr:expr),* $(,)? })? ),* $(,)? ) => {
		$(
			impl TryFrom<&VariableValue<'_, DomainValue>> for $type {
				type Error = ArgumentError<DomainType>;
				fn try_from(value: &VariableValue<DomainValue>) -> Result<Self, Self::Error> {
					use VariableValue::*;
					match value {
						Domain(DomainValue::$variant(x)) => Ok(*x),
						$($($extra_pat => $extra_expr,)*)?
						_ => Err(value.get_type().into_argument_error()),
					}
				}
			}

			impl TryFrom<VariableValue<'_, DomainValue>> for $type {
				type Error = ArgumentError<DomainType>;
				fn try_from(value: VariableValue<DomainValue>) -> Result<Self, Self::Error> {
					Self::try_from(&value)
				}
			}

			impl From<$type> for VariableValue<'_, DomainValue> {
				fn from(value: $type) -> Self {
					Self::Domain(DomainValue::$variant(value))
				}
			}
		)*
	};
}

impl_try_into_for_domain_value! {
	Vertex(VertexId),
	Cycle(CycleId),
	Object(ObjectData),
	Glyph(GlyphData),
	Color(LogicalColor) {
		Int(i) => (*i)
			.try_into()
			.map(LogicalColor::new)
			.map_err(|_| InvalidValue),
		String(s) => pictogram_name_to_id(s)
			.map(LogicalColor::pictogram)
			.map_err(|_| InvalidValue),
	}
}

impl InterpreterBackend for LevelBuilder {
	type Error = RuntimeError;
	type Warning = RuntimeWarning;
	type Value = DomainValue;

	fn call_function<'a>(
		&mut self,
		function_name: &str,
		args: ArgumentStream<'a, '_, Self::Value>,
		warnings: WarningSink<Self::Warning>,
	) -> Result<
		ReturnValue<'a, Self::Value>,
		FunctionCallError<Self::Error, <Self::Value as DomainVariableValue>::Type>,
	> {
		// Use the default built-in math functions first, only proceed if none match
		let builtin_function_result = epilang::builtins::DefaultInterpreterBackend::call_function(
			function_name,
			args.clone(),
		);
		if !matches!(builtin_function_result, Err(FunctionDoesNotExist)) {
			return Ok(builtin_function_result
				.map_err(|err| err.map_domain(Into::into))?
				.map_domain(|_| unreachable!()));
		}

		match function_name {
			"color" => Self::call_color(args),
			"pict" => Self::call_pict(args),
			"flag" => Self::call_flag(args),
			"player" => Self::call_player(args),
			"box" => Self::call_box(args),
			"button" => Self::call_button(args),
			"vertex" => self.call_vertex(args),
			"set_thing" => self.call_set_thing(args),
			"cycle" => self.call_cycle(args),
			"circle" => self.call_circle(args),
			"put_center" => self.call_put_center(args),
			"put_vertex" => self.call_put_vertex(args),
			"set_vertex_angle" => self.call_set_vertex_angle(args),
			"link" => self.call_link(false, args, warnings),
			"oneway" => self.call_link(true, args, warnings),
			"cycle_color_labels" => self.call_cycle_color_labels(args),
			_ => Err(FunctionDoesNotExist),
		}
	}
}

impl LevelBuilder {
	fn read_color_label_appearence(
		args: &mut ArgumentStream<DomainValue>,
	) -> Result<ButtonColorLabelAppearence, ArgumentError<DomainType>> {
		use VariableValue::*;

		let position = match args.read_until_end_or_separator() {
			None | Some(Blank) => ButtonColorLabelPosition::Inside,
			Some(Int(i)) => ButtonColorLabelPosition::AnglePlaced(PI * *i as f32 / 180.0),
			Some(Float(f)) => ButtonColorLabelPosition::AnglePlaced(PI * *f / 180.0),
			Some(String("left")) => ButtonColorLabelPosition::AnglePlaced(-PI / 2.0),
			Some(String("above")) => ButtonColorLabelPosition::AnglePlaced(0.0),
			Some(String("right")) => ButtonColorLabelPosition::AnglePlaced(PI / 2.0),
			Some(String("below")) => ButtonColorLabelPosition::AnglePlaced(PI),
			Some(String("rot")) => {
				let degrees: f32 = args.read_as()?;
				ButtonColorLabelPosition::AngleRotated(PI * degrees / 180.0)
			}
			Some(String(_)) => return Err(InvalidValue),
			Some(other) => return Err(TypeError(other.get_type())),
		};

		let has_arrow_tip = match args.read_as_or_blank_until_end_or_separator()? {
			None | Some("square") => false,
			Some("arrow") => true,
			Some(_) => return Err(InvalidValue),
		};

		Ok(ButtonColorLabelAppearence {
			position,
			has_arrow_tip,
		})
	}

	fn call_color(
		mut args: ArgumentStream<DomainValue>,
	) -> Result<ReturnValue<'static, DomainValue>, FunctionCallError<RuntimeError, DomainType>> {
		Ok(ReturnValue::pure(
			args.read_single_as::<LogicalColor>()?.into(),
		))
	}

	fn call_pict(
		mut args: ArgumentStream<DomainValue>,
	) -> Result<ReturnValue<'static, DomainValue>, FunctionCallError<RuntimeError, DomainType>> {
		let color_index = match args.read_single()? {
			VariableValue::Int(i) => (*i).try_into().map_err(|_| InvalidValue)?,
			VariableValue::String(s) => pictogram_name_to_id(s).map_err(|_| InvalidValue)?,
			other => return Err(TypeError(other.get_type()).into()),
		};
		Ok(ReturnValue::pure(
			LogicalColor::pictogram(color_index).into(),
		))
	}

	fn call_flag(
		args: ArgumentStream<DomainValue>,
	) -> Result<ReturnValue<'static, DomainValue>, FunctionCallError<RuntimeError, DomainType>> {
		args.read_end()?;
		Ok(ReturnValue::pure(GlyphData::Flag.into()))
	}

	fn call_player(
		args: ArgumentStream<DomainValue>,
	) -> Result<ReturnValue<'static, DomainValue>, FunctionCallError<RuntimeError, DomainType>> {
		args.read_end()?;
		Ok(ReturnValue::pure(ObjectData::Player.into()))
	}

	fn call_box(
		mut args: ArgumentStream<DomainValue>,
	) -> Result<ReturnValue<'static, DomainValue>, FunctionCallError<RuntimeError, DomainType>> {
		let color = args.read_as_or_blank_until_end_or_separator()?;
		args.read_end()?;
		Ok(ReturnValue::pure(ObjectData::Box(color).into()))
	}

	fn call_button(
		mut args: ArgumentStream<DomainValue>,
	) -> Result<ReturnValue<'static, DomainValue>, FunctionCallError<RuntimeError, DomainType>> {
		if let Some(color) = args.read_as_or_blank_until_end_or_separator()? {
			args.read_end_or_separator()?;
			let label_pos = Self::read_color_label_appearence(&mut args)?;
			args.read_end()?;
			Ok(ReturnValue::pure(
				GlyphData::Button(Some((color, label_pos))).into(),
			))
		} else {
			args.read_end()?;
			Ok(ReturnValue::pure(GlyphData::Button(None).into()))
		}
	}

	fn call_vertex(
		&mut self,
		mut args: ArgumentStream<DomainValue>,
	) -> Result<ReturnValue<'static, DomainValue>, FunctionCallError<RuntimeError, DomainType>> {
		use DomainValue::*;
		use VariableValue::*;

		let mut object = None;
		let mut glyph = None;

		let mut handle_argument = |arg: &VariableValue<DomainValue>| {
			match arg {
				Blank => {}
				Domain(Object(o)) if object.is_none() => object = Some(*o),
				Domain(Glyph(g)) if glyph.is_none() => glyph = Some(*g),
				_ => return Err(TypeError(arg.get_type())),
			}
			Ok(())
		};

		args.read_until_end_or_separator()
			.map(&mut handle_argument)
			.transpose()?;
		args.read_until_end_or_separator()
			.map(&mut handle_argument)
			.transpose()?;
		args.read_end()?;

		let vertex_id = self.add_vertex()?;
		if let Some(object) = object {
			self.set_object(vertex_id, object)?;
		}
		if let Some(glyph) = glyph {
			self.set_glyph(vertex_id, glyph)?;
		}
		Ok(ReturnValue::pure(VertexId(vertex_id).into()))
	}

	fn call_set_thing(
		&mut self,
		mut args: ArgumentStream<DomainValue>,
	) -> Result<ReturnValue<'static, DomainValue>, FunctionCallError<RuntimeError, DomainType>> {
		let mut target_vertices = Vec::new();
		while let Some(VertexId(vertex_id)) = args.read_as_until_end_or_separator()? {
			target_vertices.push(vertex_id);
		}
		args.read_separator()?;

		match args.read()? {
			VariableValue::Domain(DomainValue::Object(object)) => {
				for vertex in target_vertices {
					self.set_object(vertex, *object)?;
				}
			}
			VariableValue::Domain(DomainValue::Glyph(glyph)) => {
				for vertex in target_vertices {
					self.set_glyph(vertex, *glyph)?;
				}
			}
			other => return Err(TypeError(other.get_type()).into()),
		}

		args.read_end()?;
		Ok(ReturnValue::void())
	}

	fn call_cycle(
		&mut self,
		mut args: ArgumentStream<DomainValue>,
	) -> Result<ReturnValue<'static, DomainValue>, FunctionCallError<RuntimeError, DomainType>> {
		use DomainValue::*;
		use VariableValue::*;

		let turnability;
		if let Some(flag) = args.optional_read_as() {
			turnability = match flag {
				"manual" => CycleTurnability::WithPlayer,
				"auto" => CycleTurnability::Always,
				"still" => CycleTurnability::Never,
				_ => return Err(RuntimeError::InvalidFlag(flag.to_string()).into()),
			};
			args.read_end_or_separator()?;
		} else {
			turnability = CycleTurnability::default();
			args.optional_separator();
		}

		let mut vertices = Vec::new();
		while let Some(arg) = args.read_until_end_or_separator() {
			match arg {
				Blank => vertices.push(self.add_vertex()?),
				Domain(Object(object)) => {
					let vertex_id = self.add_vertex()?;
					self.set_object(vertex_id, *object)?;
					vertices.push(vertex_id);
				}
				Domain(Glyph(glyph)) => {
					let vertex_id = self.add_vertex()?;
					self.set_glyph(vertex_id, *glyph)?;
					vertices.push(vertex_id);
				}
				Domain(Vertex(VertexId(id))) => vertices.push(*id),
				other => return Err(TypeError(other.get_type()).into()),
			}
		}

		args.read_end()?;
		let cycle_id = self.add_cycle(turnability, vertices)?;
		Ok(ReturnValue::with_side_effect(CycleId(cycle_id).into()))
	}

	fn call_circle(
		&mut self,
		mut args: ArgumentStream<DomainValue>,
	) -> Result<ReturnValue<'static, DomainValue>, FunctionCallError<RuntimeError, DomainType>> {
		let CycleId(cycle_id) = args.read_as()?;
		args.read_separator()?;
		let x = args.read_as()?;
		let y = args.read_as()?;
		let r = args.read_as()?;
		args.read_end()?;
		self.place_cycle(cycle_id, Vec2::new(x, y), r, &[])?;
		Ok(ReturnValue::with_side_effect(CycleId(cycle_id).into()))
	}

	fn call_put_center(
		&mut self,
		mut args: ArgumentStream<DomainValue>,
	) -> Result<ReturnValue<'static, DomainValue>, FunctionCallError<RuntimeError, DomainType>> {
		let CycleId(cycle_id) = args.read_as()?;
		args.read_separator()?;
		if args.read_blank().is_ok() {
			args.read_end()?;
			self.place_cycle_center(cycle_id, None)?
		} else {
			let x = args.read_as()?;
			let y = args.read_as()?;
			args.read_end()?;
			self.place_cycle_center(cycle_id, Some(Vec2::new(x, y)))?
		}
		Ok(ReturnValue::with_side_effect(CycleId(cycle_id).into()))
	}

	fn call_set_vertex_angle(
		&mut self,
		mut args: ArgumentStream<DomainValue>,
	) -> Result<ReturnValue<'static, DomainValue>, FunctionCallError<RuntimeError, DomainType>> {
		let VertexId(vertex_id) = args.read_as()?;
		args.read_separator()?;
		let degrees: f32 = args.read_single_as()?;
		self.place_vertex_at_angle(vertex_id, degrees * PI / 180.0)?;
		Ok(ReturnValue::with_side_effect(VertexId(vertex_id).into()))
	}

	fn call_put_vertex(
		&mut self,
		mut args: ArgumentStream<DomainValue>,
	) -> Result<ReturnValue<'static, DomainValue>, FunctionCallError<RuntimeError, DomainType>> {
		let VertexId(vertex_id) = args.read_as()?;
		args.read_separator()?;
		let x = args.read_as()?;
		let y = args.read_as()?;
		args.read_end()?;
		self.place_vertex(vertex_id, Vec2::new(x, y))?;
		Ok(ReturnValue::with_side_effect(VertexId(vertex_id).into()))
	}

	fn call_link(
		&mut self,
		one_way: bool,
		mut args: ArgumentStream<DomainValue>,
		mut warnings: WarningSink<RuntimeWarning>,
	) -> Result<ReturnValue<'static, DomainValue>, FunctionCallError<RuntimeError, DomainType>> {
		let direction;
		if let Some(flag) = args.optional_read_as() {
			direction = match flag {
				"coincident" => LinkedCycleDirection::Coincident,
				"invert" => LinkedCycleDirection::Inverse,
				_ => return Err(RuntimeError::InvalidFlag(flag.to_string()).into()),
			};
			args.read_end_or_separator()?;
		} else {
			direction = LinkedCycleDirection::default();
			args.optional_separator();
		}

		let mut cycles = Vec::new();
		while let Some(CycleId(cycle_id)) = args.read_as_until_end_or_separator()? {
			cycles.push(cycle_id);
		}
		if cycles.len() < 2 {
			warnings.emit(RuntimeWarning::EmptyLink.into());
		}

		for (a, b) in cycles.into_iter().tuple_windows() {
			if one_way {
				self.one_way_link_cycles(a, b, direction)?;
			} else {
				self.link_cycles(a, b, direction)?;
			}
		}

		Ok(ReturnValue::void())
	}

	fn call_cycle_color_labels(
		&mut self,
		mut args: ArgumentStream<DomainValue>,
	) -> Result<ReturnValue<'static, DomainValue>, FunctionCallError<RuntimeError, DomainType>> {
		let CycleId(cycle_id) = args.read_as()?;
		args.read_separator()?;

		let positions = match args.read_as()? {
			"lr" => CycleBoundColorLabelPositionSet::LeftRight,
			"tb" => CycleBoundColorLabelPositionSet::AboveBelow,
			"quad" => CycleBoundColorLabelPositionSet::CardinalDirections,
			"any" => CycleBoundColorLabelPositionSet::AllDirections,
			"rot" => CycleBoundColorLabelPositionSet::AllDirectionsRotated,
			other => return Err(RuntimeError::InvalidFlag(other.to_string()).into()),
		};
		let place_outside_cycle = match args.read_as_until_end_or_separator()? {
			Some("out") | None => true,
			Some("in") => false,
			Some(other) => return Err(RuntimeError::InvalidFlag(other.to_string()).into()),
		};
		let has_arrow_tip = match args.read_as_until_end_or_separator()? {
			Some("square") | None => false,
			Some("arrow") => true,
			Some(other) => return Err(RuntimeError::InvalidFlag(other.to_string()).into()),
		};
		args.read_end()?;

		self.set_color_label_appearences_for_cycle(
			cycle_id,
			positions,
			place_outside_cycle,
			has_arrow_tip,
		)?;
		Ok(ReturnValue::void())
	}
}

fn pictogram_name_to_id(name: &str) -> Result<usize, ()> {
	match name {
		"desc" => Ok(0),                    // -,
		"asc" => Ok(1),                     //  |
		"x" | "cross" => Ok(2),             //  |
		"minus" | "hbar" => Ok(3),          //  |
		"beam" | "vbar" => Ok(4),           //  |
		"plus" => Ok(5),                    //  |  Lateral and diagonal beams
		"desc_neg" => Ok(6),                //  |
		"asc_neg" => Ok(7),                 //  |
		"x_neg" | "cross_neg" => Ok(8),     //  |
		"minus_neg" | "hbar_neg" => Ok(9),  //  |
		"beam_neg" | "vbar_neg" => Ok(10),  //  |
		"plus_neg" => Ok(11),               // -`
		"up" => Ok(12),                     // -,
		"down" => Ok(13),                   //  |
		"left" => Ok(14),                   //  |
		"right" => Ok(15),                  //  | Triangles that span full length of the box
		"up_neg" => Ok(16),                 //  |
		"down_neg" => Ok(17),               //  |
		"left_neg" => Ok(18),               //  |
		"right_neg" => Ok(19),              // -`
		"top" => Ok(20),                    // -,
		"bottom" | "bot" => Ok(21),         //  |
		"start" => Ok(22),                  //  |
		"end" => Ok(23),                    //  | Triangles that reach to the center of the box
		"top_neg" => Ok(24),                //  |
		"bottom_neg" | "bot_neg" => Ok(25), //  |
		"start_neg" => Ok(26),              //  |
		"end_neg" => Ok(27),                // -`
		"tb" => Ok(28),                     // -, Two opposing center-boud triangles
		"lr" | "rl" => Ok(29),              // -`
		"lt" => Ok(30),                     // -,
		"rt" => Ok(31),                     //  | Half of the box filled, diagonally (as a right triangle)
		"lb" => Ok(32),                     //  |
		"rb" => Ok(33),                     // -`
		"wide_diamond" => Ok(34),           // -, Wide diamond (square, not rhombus)
		"corners" => Ok(35),                // -`
		"star" => Ok(36),                   // Star pictogram
		"checker_asc" => Ok(37),            // -, Two opposite quadrants are filled
		"checker_desc" => Ok(38),           // -`
		"heart" => Ok(39),                  // -,
		"spade" => Ok(40),                  //  | Card suites
		"club" => Ok(41),                   //  |
		"diamond" => Ok(42),                // -`
		"one" => Ok(43),                    // -,
		"two" => Ok(44),                    //  |
		"three" => Ok(45),                  //  | Dice dots
		"four" => Ok(46),                   //  |
		"five" => Ok(47),                   //  |
		"six" => Ok(48),                    // -`
		"fill" => Ok(49),                   // Filled box
		_ => Err(()),
	}
}

#[derive(Clone, PartialEq, Debug)]
pub enum RuntimeError {
	ArithmeticOverflow,
	BuilderError(LevelBuilderError),
	InvalidFlag(String),
}

impl From<epilang::builtins::ArithmeticOverflowError> for RuntimeError {
	fn from(_: epilang::builtins::ArithmeticOverflowError) -> Self {
		Self::ArithmeticOverflow
	}
}

impl From<LevelBuilderError> for RuntimeError {
	fn from(value: LevelBuilderError) -> Self {
		Self::BuilderError(value)
	}
}

impl From<LevelBuilderError> for FunctionCallError<RuntimeError, DomainType> {
	fn from(value: LevelBuilderError) -> Self {
		RuntimeError::from(value).into()
	}
}

impl std::error::Error for RuntimeError {}

impl std::fmt::Display for RuntimeError {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::ArithmeticOverflow => f.write_str("arithmetic overflow"),
			Self::BuilderError(e) => e.fmt(f),
			Self::InvalidFlag(flag) => write!(f, "'{flag}' is not a valid flag for this function"),
		}
	}
}

#[derive(Debug, PartialEq)]
pub enum LevelParsingError {
	CompileError(epilang::CompileError),
	RuntimeError(epilang::InterpreterError<RuntimeError, DomainType>),
	Timeout,
	BuilderError(LevelBuilderError),
	SemanticVariableTypeError(String, VariableType<DomainType>),
}

impl From<LevelBuilderError> for LevelParsingError {
	fn from(value: LevelBuilderError) -> Self {
		Self::BuilderError(value)
	}
}

impl From<epilang::CompileError> for LevelParsingError {
	fn from(value: epilang::CompileError) -> Self {
		Self::CompileError(value)
	}
}

impl From<epilang::InterpreterError<RuntimeError, DomainType>> for LevelParsingError {
	fn from(value: epilang::InterpreterError<RuntimeError, DomainType>) -> Self {
		Self::RuntimeError(value)
	}
}

impl From<epilang::interpreter::LoadedVariableTypeError<DomainType>> for LevelParsingError {
	fn from(value: epilang::interpreter::LoadedVariableTypeError<DomainType>) -> Self {
		Self::SemanticVariableTypeError(value.variable_name, value.actual_type)
	}
}

impl std::error::Error for LevelParsingError {}

impl std::fmt::Display for LevelParsingError {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::CompileError(e) => e.fmt(f),
			Self::Timeout => f.write_str("epilang script execution timed out"),
			Self::RuntimeError(e) => e.fmt(f),
			Self::BuilderError(e) => write!(f, "while finishing level build: {e}"),
			Self::SemanticVariableTypeError(name, actual) => {
				write!(f, "exported variable {name} has invalid type {actual}")
			}
		}
	}
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum RuntimeWarning {
	/// A link function was called with less than two cycles,
	/// creating no links
	EmptyLink,
}

impl std::error::Error for RuntimeWarning {}

impl std::fmt::Display for RuntimeWarning {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::EmptyLink => f.write_str("Too few vertices to actually create a link"),
		}
	}
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum LevelParsingWarning {
	/// A warning triggered by the Epilang interpreter
	RuntimeWarning(InterpreterWarning<RuntimeWarning>),
	/// Level name has not been set
	LevelNameNotSet,
}

impl std::error::Error for LevelParsingWarning {}

impl std::fmt::Display for LevelParsingWarning {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::RuntimeWarning(w) => w.fmt(f),
			Self::LevelNameNotSet => f.write_str("no level name has been set"),
		}
	}
}

#[cfg(test)]
mod test {
	use super::{
		ButtonColorLabelAppearence, ButtonColorLabelPosition, FunctionCallError, InterpreterError,
		LevelBuilderError, LevelData, LevelParsingError, LogicError, LogicalColor,
		OverlappedLinkedCyclesError, RuntimeError,
	};
	use std::f32::consts::PI;

	fn parse(level_file: &str) -> Result<LevelData, LevelParsingError> {
		super::parse(level_file, |_| {})
	}

	macro_rules! assert_err_eq {
		($left:expr, $right:expr) => {
			let left = $left;
			let right = $right;
			let err = left.expect_err("Negative test sample parrsed without error!");
			let builder_error = match err {
				LevelParsingError::RuntimeError(InterpreterError::LogicError(err, _)) => match *err
				{
					LogicError::FunctionCall(
						FunctionCallError::Domain(RuntimeError::BuilderError(err)),
						_,
					) => err,
					_ => panic!("Negative test sample returned incorrect error!\n{err}"),
				},
				LevelParsingError::BuilderError(err) => err,
				_ => panic!("Negative test sample returned incorrect error!\n{err}"),
			};
			assert_eq!(builder_error, right);
		};
	}

	#[test]
	fn basic_test() {
		let data = r"
name = '?!#_AAA 648';
hint = 'This should parse correctly!';

# Here we declare all the vertex names
# The vertex function returns a value of type vertex
# Vertices have reference semantics (a copy of the variable will represent the same vertex)
x = vertex(box());

# Here we declare cycles
# First one declares a cycle, with a modifier specifying whether it needs a player
# Leaving the modifier out defaults to an automatic playerless cycle
# Next comes a list of vertices which lie on the circle in a clockwise order.
# They can be actual vertices, objects, glyphs, or blank values (underscores).
# For all types except a pre-existing vertex, the cycle function creates a new vertex at call time.
# Cycles also have reference semantics
cycle_a = cycle('manual'; box() vertex(player(), button()) _ x);
cycle_b = cycle('auto'; x _ flag() _);

# Oops, we forgot a cycle, let's add one
# This defaults to an automatic cycle
cycle_extra = cycle(q = vertex(), r = vertex(), s = vertex());

# Objects and glaphs can also be added subsequently
set_thing(q r; box());

# Cycles can be linked together
link(cycle_a cycle_extra);

# We have to position the cycles with x y radius data
circle(cycle_a; -100, 0.0, 100);
circle(cycle_b; +100, 0, 100);
circle(cycle_extra; -100, 100, sqrt(41));
";
		let level = parse(data).expect("Test sample did not parse correctly!");
		assert_eq!(level.name, "?!#_AAA 648");
	}

	#[test]
	fn test_structural_validation() {
		let test_cases = r"
# Linking a cycle to itself is already weird, but legal.
# The link, however, cannot be inverted.
a = cycle(_ _);
link('invert'; a a);

# Intersecting cycles cannot be linked.
a = cycle(x = vertex(), _);
b = cycle(x _);
link(a b);

# Two (declared) links between the same two cycles may exist as well,
# but they cannot be conflicting like this.
a = cycle(_ _);
b = cycle(_ _);
link(a b);
link('invert'; b a);

# Three cycles linked in a triangle.
# Again, this is legal, but the links must be compatible.
a = cycle(_ _);
b = cycle(_ _);
c = cycle(_ _);
link(a b c);
link('invert'; a c);

# Same with four cycles
a = cycle(_ _);
b = cycle(_ _);
c = cycle(_ _);
d = cycle(_ _);
link(a b c d);
link('invert'; a d);

# Cycles 1 and 3 share a vertex.
# They are not linked directly, but the links connect them transitively.
a = cycle(x = vertex(), _);
b = cycle(_ _);
c = cycle(_ x);
link(a b c);
";

		let expected_results = [
			LevelBuilderError::CycleLinkageConflict(0, 0),
			LevelBuilderError::OverlappedLinkedCycles(OverlappedLinkedCyclesError {
				source_cycle: 1,
				dest_cycle: 0,
				shared_vertex: 0,
			}),
			LevelBuilderError::CycleLinkageConflict(1, 0),
			LevelBuilderError::CycleLinkageConflict(0, 2),
			LevelBuilderError::CycleLinkageConflict(0, 3),
			LevelBuilderError::OverlappedLinkedCycles(OverlappedLinkedCyclesError {
				source_cycle: 2,
				dest_cycle: 0,
				shared_vertex: 0,
			}),
		];

		for (data, expected) in test_cases.split("\n\n").zip(expected_results) {
			assert_err_eq!(parse(data), expected);
		}
	}

	#[test]
	fn logical_colors_test() {
		let data = r"
# Default, with no call_color
vertex(box());

# Numeric colors are specified with numbers
vertex(box(42));

# Pictogram colors are specified by name
vertex(box('star'));

# Pictogram colors may also be specified by their id
vertex(box(pict(16)));

# Colorless object can be forced by explicit blank color value
vertex(box(_));
";

		let expected_colors = [
			None,
			Some(LogicalColor::new(42)),
			Some(LogicalColor::pictogram(36)),
			Some(LogicalColor::pictogram(16)),
			None,
		];

		let level = parse(data).expect("Test sample did not parse correctly!");
		for (vertex, expected_color) in level.vertices.iter().zip(expected_colors) {
			let Some(super::ObjectData::Box(call_color)) = vertex.object else {
				panic!("Vertex does not contain a box.");
			};
			assert_eq!(call_color, expected_color);
		}
	}

	#[test]
	fn color_labels_test() {
		let data = r"
col = color(0);

# Color labels can be placed to a set of
# predefined positions
a = vertex(button(col; 'left'));
b = vertex(button(col; 'right'));
c = vertex(button(col; 'above'));
d = vertex(button(col; 'below'));

# The default position is inside the button.
# This can also be forced manually
g = vertex(button(col; _));
h = vertex(button(col));

# Position can be specified manually as rotation
# (as clock angle in degrees)
i = vertex(button(col; 30));

# Adding the 'rot' flag means the label itself will be
# rotated, not just positioned
j = vertex(button(col; 'rot' 60));

# Finally, any option can be combined with a shape specifier
# to choose between square label (default) or an arrow-tipped one
k = vertex(button(col; 'above' 'square'));
l = vertex(button(col; _ 'arrow'));

m = vertex(button(col));
n = vertex(button(col));
o = vertex(button(col));
p = vertex(button(col));
q = vertex(button(col));
r = vertex(button(col));
s = vertex(button(col));
t = vertex(button(col));

x = cycle(m, n, o, p);
y = cycle(q, r, s, t);
circle(x; 0, 0, 100);
circle(y; 0, 0, 100);

set_vertex_angle(m; 0.5);
set_vertex_angle(q; 0.5);

# Labels can be positioned symmetrically around a cycle,
# with respect to where they are relative to the cycle center
cycle_color_labels(x; 'quad');

# Cycle-wide label placement may also be done inside the cycle
# and with arrow labels if desired
cycle_color_labels(y; 'lr' 'in' 'arrow');
";
		let expected_appearences = [
			ButtonColorLabelAppearence {
				position: ButtonColorLabelPosition::AnglePlaced(-PI / 2.0),
				has_arrow_tip: false,
			},
			ButtonColorLabelAppearence {
				position: ButtonColorLabelPosition::AnglePlaced(PI / 2.0),
				has_arrow_tip: false,
			},
			ButtonColorLabelAppearence {
				position: ButtonColorLabelPosition::AnglePlaced(0.0),
				has_arrow_tip: false,
			},
			ButtonColorLabelAppearence {
				position: ButtonColorLabelPosition::AnglePlaced(PI),
				has_arrow_tip: false,
			},
			ButtonColorLabelAppearence {
				position: ButtonColorLabelPosition::Inside,
				has_arrow_tip: false,
			},
			ButtonColorLabelAppearence {
				position: ButtonColorLabelPosition::Inside,
				has_arrow_tip: false,
			},
			ButtonColorLabelAppearence {
				position: ButtonColorLabelPosition::AnglePlaced(PI / 6.0),
				has_arrow_tip: false,
			},
			ButtonColorLabelAppearence {
				position: ButtonColorLabelPosition::AngleRotated(PI / 3.0),
				has_arrow_tip: false,
			},
			ButtonColorLabelAppearence {
				position: ButtonColorLabelPosition::AnglePlaced(0.0),
				has_arrow_tip: false,
			},
			ButtonColorLabelAppearence {
				position: ButtonColorLabelPosition::Inside,
				has_arrow_tip: true,
			},
			ButtonColorLabelAppearence {
				position: ButtonColorLabelPosition::AnglePlaced(0.0),
				has_arrow_tip: false,
			},
			ButtonColorLabelAppearence {
				position: ButtonColorLabelPosition::AnglePlaced(PI / 2.0),
				has_arrow_tip: false,
			},
			ButtonColorLabelAppearence {
				position: ButtonColorLabelPosition::AnglePlaced(PI),
				has_arrow_tip: false,
			},
			ButtonColorLabelAppearence {
				position: ButtonColorLabelPosition::AnglePlaced(PI * 1.5),
				has_arrow_tip: false,
			},
			ButtonColorLabelAppearence {
				position: ButtonColorLabelPosition::AnglePlaced(PI * 1.5),
				has_arrow_tip: true,
			},
			ButtonColorLabelAppearence {
				position: ButtonColorLabelPosition::AnglePlaced(PI * 1.5),
				has_arrow_tip: true,
			},
			ButtonColorLabelAppearence {
				position: ButtonColorLabelPosition::AnglePlaced(PI / 2.0),
				has_arrow_tip: true,
			},
			ButtonColorLabelAppearence {
				position: ButtonColorLabelPosition::AnglePlaced(PI / 2.0),
				has_arrow_tip: true,
			},
		];

		let level = parse(data).expect("Test sample did not parse correctly!");
		for (vertex, expected_appearence) in level.vertices.iter().zip(expected_appearences) {
			let Some(super::GlyphData::Button(Some((_, appearence)))) = vertex.glyph else {
				panic!("Vertex does not contain a colored button.");
			};
			assert_eq!(appearence, expected_appearence);
		}
	}

	#[test]
	fn one_way_parse_test() {
		let level_header = r"
v1 = vertex();
v2 = vertex();
v3 = vertex();
v4 = vertex();
v5 = vertex();
v6 = vertex();
vx = vertex();
vy = vertex();
y = cycle(vy);
c1 = cycle(v1);
c2 = cycle(v2);
c3 = cycle(v3);
c4 = cycle(v4);
c5 = cycle(v5);
c6 = cycle(v6);
x = cycle(vx);

link(c6 x);
link('invert'; c1 y);

circle(c1; 0 0 100);
circle(c2; 0 0 100);
circle(c3; 0 0 100);
circle(c4; 0 0 100);
circle(c5; 0 0 100);
circle(c6; 0 0 100);
circle(x; 0 0 100);
circle(y; 0 0 100);
";
		let linkages = r"
# CHAIN
oneway(c1 c2);
oneway(c2 c3);
oneway(c3 c4);
oneway(c4 c5);
oneway(c5 c6);

# CHAIN, inversely written
oneway(c5 c6);
oneway(c4 c5);
oneway(c3 c4);
oneway(c2 c3);
oneway(c1 c2);

# CHAIN, weird order
oneway(c3 c4);
oneway(c2 c3);
oneway(c4 c5);
oneway(c1 c2);
oneway(c5 c6);

# CHAIN, reverse
oneway(c2 c1);
oneway(c3 c2);
oneway(c4 c3);
oneway(c5 c4);
oneway(c6 c5);

# TREE
oneway(c1 c2);
oneway(c1 c3);
oneway(c2 c4);
oneway(c2 c5);
oneway(c3 c6);

# TREE, different order
oneway(c2 c4);
oneway(c2 c5);
oneway(c1 c2);
oneway(c1 c3);
oneway(c3 c6);

# DIAMOND
oneway(c1 c2);
oneway(c1 c3);
oneway(c2 c4);
oneway(c3 c4);

# DIAMOND, different order
oneway(c2 c4);
oneway(c3 c4);
oneway(c1 c2);
oneway(c1 c3);

# COMPLETE GRAPH
oneway(c1 c2);
oneway(c1 c3);
oneway(c1 c4);
oneway(c1 c5);
oneway(c1 c6);
oneway(c2 c3);
oneway(c2 c4);
oneway(c2 c5);
oneway(c2 c6);
oneway(c3 c4);
oneway(c3 c5);
oneway(c3 c6);
oneway(c4 c5);
oneway(c4 c6);
oneway(c5 c6);

# COMPLETE GRAPH, different order
oneway(c1 c2);
oneway(c2 c4);
oneway(c2 c5);
oneway(c1 c6);
oneway(c2 c6);
oneway(c4 c6);
oneway(c1 c3);
oneway(c3 c5);
oneway(c3 c4);
oneway(c5 c6);
oneway(c2 c3);
oneway(c3 c6);
oneway(c4 c5);
oneway(c1 c4);
oneway(c1 c5);

# COMPLETE GRAPH, DOUBLED LINKS
oneway(c1 c2);
oneway(c1 c3);
oneway(c1 c4);
oneway(c1 c5);
oneway(c1 c6);
oneway(c2 c3);
oneway(c2 c4);
oneway(c2 c5);
oneway(c2 c6);
oneway(c3 c4);
oneway(c3 c5);
oneway(c3 c6);
oneway(c4 c5);
oneway(c4 c6);
oneway(c5 c6);
oneway(c1 c2);
oneway(c2 c4);
oneway(c2 c5);
oneway(c1 c6);
oneway(c2 c6);
oneway(c4 c6);
oneway(c1 c3);
oneway(c3 c5);
oneway(c3 c4);
oneway(c5 c6);
oneway(c2 c3);
oneway(c3 c6);
oneway(c4 c5);
oneway(c1 c4);
oneway(c1 c5);
"
		.split("\n\n");

		for data in linkages {
			let level = format!("{}\n{}", level_header, data);
			let output = parse(&level);
			assert!(output.is_ok(), "{:?}", output);
		}
	}

	#[test]
	fn one_way_validation_test() {
		let level_header = r"
v1 = vertex();
v2 = vertex();
v3 = vertex();
v4 = vertex();
v5 = vertex();
v6 = vertex();
vx = vertex();
vy = vertex();
y = cycle(vy);
c1 = cycle(v1);
c2 = cycle(v2);
c3 = cycle(v3);
c4 = cycle(v4);
c5 = cycle(v5);
c6 = cycle(v6);
x = cycle(vx);

link(c6, x);
link('invert'; c1 y);

circle(c1; 0 0 100);
circle(c2; 0 0 100);
circle(c3; 0 0 100);
circle(c4; 0 0 100);
circle(c5; 0 0 100);
circle(c6; 0 0 100);
circle(x; 0 0 100);
circle(y; 0 0 100);
";
		let linkages = r"
# CHAIN with LOOP
oneway(c1 c2);
oneway(c2 c3);
oneway(c3 c4);
oneway(c4 c5);
oneway(c5 c6);
oneway(c6 c1);

# CHAIN with LINK LOOP
oneway(c1 c2);
oneway(c2 c3);
oneway(c3 c4);
oneway(c4 c5);
oneway(c5 c6);
link('invert'; c2 c5);

# OVERLAPPING LINK AND ONEWAY
oneway(c1 c2);
link(c1 c2);

# OVERLAPPING ONEWAYS
oneway(c1 c2);
oneway(c2 c1);

# TRIANGLE
oneway(c1 c2);
oneway(c3 c1);
oneway(c2 c3);
"
		.split("\n\n");

		for data in linkages {
			let level = format!("{}\n{}", level_header, data);
			assert_err_eq!(parse(&level), LevelBuilderError::OneWayLinkLoop);
		}
	}
}
