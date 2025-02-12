//! Runtime of the [`LevelBuilder`] backend
//!
//! Contains implementations of functions callable by Epilang
//! and errors and warnings they can emit

use super::super::{
	super::{
		builder::{error::LevelBuilderError, CycleBoundColorLabelPositionSet, LevelBuilder},
		*,
	},
	domain::*,
};
use crate::epilang::{
	builtins,
	interpreter::{
		ArgumentError::{self, *},
		FunctionCallError::*,
		*,
	},
	values::*,
};
use itertools::Itertools as _;
use std::f32::consts::PI;

impl InterpreterBackend for LevelBuilder {
	type Error = RuntimeError;
	type Warning = RuntimeWarning;
	type Value = DomainValue;

	fn call_function(
		&mut self,
		function_name: &str,
		args: ArgumentStream<Self::Value>,
		warnings: WarningSink<Self::Warning>,
	) -> CallResult {
		// Use the default built-in math functions first, only proceed if none match
		let builtin_function_result =
			builtins::DefaultInterpreterBackend::call_function(function_name, args.clone());
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
			"detector" => self.call_detector(args),
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

/// Shorthand for the common result type returned by function implementations
type CallResult =
	Result<ReturnValue<'static, DomainValue>, FunctionCallError<RuntimeError, DomainType>>;

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

	fn call_color(mut args: ArgumentStream<DomainValue>) -> CallResult {
		Ok(ReturnValue::pure(
			args.read_single_as::<LogicalColor>()?.into(),
		))
	}

	fn call_pict(mut args: ArgumentStream<DomainValue>) -> CallResult {
		let color_index = match args.read_single()? {
			VariableValue::Int(i) => (*i).try_into().map_err(|_| InvalidValue)?,
			VariableValue::String(s) => pictogram_color_name_to_id(s).map_err(|_| InvalidValue)?,
			other => return Err(TypeError(other.get_type()).into()),
		};
		Ok(ReturnValue::pure(
			LogicalColor::pictogram(color_index).into(),
		))
	}

	fn call_flag(args: ArgumentStream<DomainValue>) -> CallResult {
		args.read_end()?;
		Ok(ReturnValue::pure(GlyphData::Flag.into()))
	}

	fn call_player(args: ArgumentStream<DomainValue>) -> CallResult {
		args.read_end()?;
		Ok(ReturnValue::pure(ObjectData::Player.into()))
	}

	fn call_box(mut args: ArgumentStream<DomainValue>) -> CallResult {
		let color = args.read_as_or_blank_until_end_or_separator()?;
		args.read_end()?;
		Ok(ReturnValue::pure(ObjectData::Box(color).into()))
	}

	fn call_button(mut args: ArgumentStream<DomainValue>) -> CallResult {
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

	fn call_vertex(&mut self, mut args: ArgumentStream<DomainValue>) -> CallResult {
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

	fn call_detector(&mut self, args: ArgumentStream<DomainValue>) -> CallResult {
		args.read_end()?;
		Ok(ReturnValue::pure(DetectorId(self.add_detector()?).into()))
	}

	fn call_set_thing(&mut self, mut args: ArgumentStream<DomainValue>) -> CallResult {
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

	fn call_cycle(&mut self, mut args: ArgumentStream<DomainValue>) -> CallResult {
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
		// TODO: Detectors
		let cycle_id = self.add_cycle(turnability, vertices, Vec::new())?;
		Ok(ReturnValue::with_side_effect(CycleId(cycle_id).into()))
	}

	fn call_circle(&mut self, mut args: ArgumentStream<DomainValue>) -> CallResult {
		let CycleId(cycle_id) = args.read_as()?;
		args.read_separator()?;
		let x = args.read_as()?;
		let y = args.read_as()?;
		let r = args.read_as()?;
		args.read_end()?;
		self.place_cycle(cycle_id, Vec2::new(x, y), r, &[])?;
		Ok(ReturnValue::with_side_effect(CycleId(cycle_id).into()))
	}

	fn call_put_center(&mut self, mut args: ArgumentStream<DomainValue>) -> CallResult {
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

	fn call_set_vertex_angle(&mut self, mut args: ArgumentStream<DomainValue>) -> CallResult {
		let VertexId(vertex_id) = args.read_as()?;
		args.read_separator()?;
		let degrees: f32 = args.read_single_as()?;
		self.place_vertex_at_angle(vertex_id, degrees * PI / 180.0)?;
		Ok(ReturnValue::with_side_effect(VertexId(vertex_id).into()))
	}

	fn call_put_vertex(&mut self, mut args: ArgumentStream<DomainValue>) -> CallResult {
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
	) -> CallResult {
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
		let mut detector = None;
		// TODO: Better handling of wrong type inputs
		if one_way {
			// Handle the first input potentially being a detector
			if let Some(value) = args.read_until_end_or_separator() {
				match value {
					VariableValue::Domain(DomainValue::Cycle(CycleId(cycle_id))) => {
						cycles.push(*cycle_id)
					}
					VariableValue::Domain(DomainValue::Detector(DetectorId(detector_id))) => {
						detector = Some(*detector_id)
					}
					_ => {}
				}
			}
		}
		while let Some(CycleId(cycle_id)) = args.read_as_until_end_or_separator()? {
			cycles.push(cycle_id);
		}
		if cycles.len() < 2 && (cycles.len() < 1 || detector.is_none()) {
			warnings.emit(RuntimeWarning::EmptyLink.into());
		}

		// Handle the first input potentially being a detector
		if let Some(detector) = detector {
			if let Some(cycle) = cycles.first() {
				self.one_way_link_detector(detector, *cycle, direction)?;
			}
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

	fn call_cycle_color_labels(&mut self, mut args: ArgumentStream<DomainValue>) -> CallResult {
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

#[derive(Clone, PartialEq, Debug)]
pub enum RuntimeError {
	ArithmeticOverflow,
	BuilderError(LevelBuilderError),
	InvalidFlag(String),
}

impl From<builtins::ArithmeticOverflowError> for RuntimeError {
	fn from(_: builtins::ArithmeticOverflowError) -> Self {
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
