use super::Module;

pub trait DynVariableValue: std::fmt::Debug {
	fn get_type_name(&self) -> &'static str;
	fn dyn_clone(&self) -> Box<dyn DynVariableValue>;
}

impl Clone for Box<dyn DynVariableValue> {
	fn clone(&self) -> Self {
		self.dyn_clone()
	}
}

#[derive(Clone, Debug, Default)]
pub enum VariableValue<'m> {
	#[default]
	Blank,
	Bool(bool),
	Int(i32),
	Float(f32),
	String(&'m str),
	Callback(&'m Module),
	Dyn(Box<dyn DynVariableValue>),
}

macro_rules! impl_try_from_for_variable_value {
	( $($variant:ident ( $type:ty )),* $(,)? ) => {
		$(
			impl<'m> TryFrom<&VariableValue<'m>> for $type {
				type Error = VariableType;
				fn try_from(value: &VariableValue<'m>) -> Result<Self, Self::Error> {
					match value {
						VariableValue::$variant(x) => Ok(*x),
						_ => Err(value.get_type())
					}
				}
			}

			impl<'m> TryFrom<VariableValue<'m>> for $type {
				type Error = VariableType;
				fn try_from(value: VariableValue<'m>) -> Result<Self, Self::Error> {
					Self::try_from(&value)
				}
			}

			impl<'m> From<$type> for VariableValue<'m> {
				fn from(value: $type) -> Self {
					Self::$variant(value)
				}
			}
		)*
	};
}

impl_try_from_for_variable_value! {
	Bool(bool),
	Int(i32),
	Float(f32),
	String(&'m str),
	Callback(&'m Module),
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, Default)]
pub enum VariableType {
	#[default]
	Blank,
	Bool,
	Int,
	Float,
	String,
	Callback,
	Custom(&'static str),
}

impl std::fmt::Display for VariableType {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::Blank => f.write_str("_"),
			Self::Bool => f.write_str("bool"),
			Self::Int => f.write_str("int"),
			Self::Float => f.write_str("float"),
			Self::String => f.write_str("string"),
			Self::Callback => f.write_str("fn"),
			Self::Custom(name) => f.write_fmt(format_args!("'{name}'")),
		}
	}
}

pub enum NumericPair {
	Int(i32, i32),
	Float(f32, f32),
}

impl NumericPair {
	pub fn convert_from(left: &VariableValue, right: &VariableValue) -> Option<Self> {
		use VariableValue::{Float, Int};
		match (left, right) {
			(&Int(left), &Int(right)) => Some(Self::Int(left, right)),
			(&Int(left), &Float(right)) => Some(Self::Float(left as f32, right)),
			(&Float(left), &Int(right)) => Some(Self::Float(left, right as f32)),
			(&Float(left), &Float(right)) => Some(Self::Float(left, right)),
			_ => None,
		}
	}
}

impl VariableValue<'_> {
	pub fn get_type(&self) -> VariableType {
		match self {
			Self::Blank => VariableType::Blank,
			Self::Bool(_) => VariableType::Bool,
			Self::Int(_) => VariableType::Int,
			Self::Float(_) => VariableType::Float,
			Self::String(_) => VariableType::String,
			Self::Callback(_) => VariableType::Callback,
			Self::Dyn(value) => VariableType::Custom(value.get_type_name()),
		}
	}
}
