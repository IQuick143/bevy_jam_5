use super::Module;

pub trait DomainVariableValue: Clone + std::fmt::Debug {
	type Type: DomainVariableType;
	fn get_type(&self) -> Self::Type;
}

pub trait DomainVariableType: Clone + Copy + std::fmt::Debug + std::fmt::Display + 'static {}

impl<T: Clone + Copy + std::fmt::Debug + std::fmt::Display + 'static> DomainVariableType for T {}

#[derive(Clone, Debug, Default)]
pub enum VariableValue<'m, T: DomainVariableValue + 'm> {
	#[default]
	Blank,
	Bool(bool),
	Int(i32),
	Float(f32),
	String(&'m str),
	Callback(&'m Module),
	Domain(T),
}

macro_rules! impl_try_from_for_variable_value {
	( $($variant:ident ( $type:ty ) $({ $($extra_pat:pat => $extra_expr:expr),* $(,)? })? ),* $(,)? ) => {
		$(
			impl<'m, T: DomainVariableValue> TryFrom<&VariableValue<'m, T>> for $type {
				type Error = VariableType<T::Type>;
				fn try_from(value: &VariableValue<'m, T>) -> Result<Self, Self::Error> {
					use VariableValue::*;
					Ok(match value {
						$variant(x) => *x,
						$($($extra_pat => $extra_expr,)*)?
						_ => return Err(value.get_type()),
					})
				}
			}

			impl<'m, T: DomainVariableValue> TryFrom<VariableValue<'m, T>> for $type {
				type Error = VariableType<T::Type>;
				fn try_from(value: VariableValue<'m, T>) -> Result<Self, Self::Error> {
					Self::try_from(&value)
				}
			}

			impl<'m, T: DomainVariableValue> From<$type> for VariableValue<'m, T> {
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
	Float(f32) { Int(i) => *i as f32 },
	String(&'m str),
	Callback(&'m Module),
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, Default)]
pub enum VariableType<T: DomainVariableType> {
	#[default]
	Blank,
	Bool,
	Int,
	Float,
	String,
	Callback,
	Domain(T),
}

impl<T: DomainVariableType> std::fmt::Display for VariableType<T> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::Blank => f.write_str("_"),
			Self::Bool => f.write_str("bool"),
			Self::Int => f.write_str("int"),
			Self::Float => f.write_str("float"),
			Self::String => f.write_str("string"),
			Self::Callback => f.write_str("fn"),
			Self::Domain(t) => f.write_fmt(format_args!("domain:{t}")),
		}
	}
}

pub(super) enum NumericPair {
	Int(i32, i32),
	Float(f32, f32),
}

impl NumericPair {
	pub fn convert_from<T: DomainVariableValue>(
		left: &VariableValue<T>,
		right: &VariableValue<T>,
	) -> Option<Self> {
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

impl<T: DomainVariableValue> DomainVariableValue for VariableValue<'_, T> {
	type Type = VariableType<T::Type>;

	fn get_type(&self) -> Self::Type {
		match self {
			Self::Blank => Self::Type::Blank,
			Self::Bool(_) => Self::Type::Bool,
			Self::Int(_) => Self::Type::Int,
			Self::Float(_) => Self::Type::Float,
			Self::String(_) => Self::Type::String,
			Self::Callback(_) => Self::Type::Callback,
			Self::Domain(value) => Self::Type::Domain(value.get_type()),
		}
	}
}

impl<'a, T: DomainVariableValue + 'a> From<T> for VariableValue<'a, T> {
	fn from(value: T) -> Self {
		VariableValue::Domain(value)
	}
}

impl<T: DomainVariableType> From<T> for VariableType<T> {
	fn from(value: T) -> Self {
		VariableType::Domain(value)
	}
}

impl<'a, T: DomainVariableValue + 'a> VariableValue<'a, T> {
	pub fn map_domain<U: DomainVariableValue + 'a>(
		self,
		map: impl FnOnce(T) -> U,
	) -> VariableValue<'a, U> {
		match self {
			Self::Blank => VariableValue::Blank,
			Self::Bool(b) => VariableValue::Bool(b),
			Self::Int(i) => VariableValue::Int(i),
			Self::Float(f) => VariableValue::Float(f),
			Self::String(s) => VariableValue::String(s),
			Self::Callback(f) => VariableValue::Callback(f),
			Self::Domain(t) => VariableValue::Domain(map(t)),
		}
	}
}

impl<T: DomainVariableType> VariableType<T> {
	pub fn map_domain<U: DomainVariableType>(self, map: impl FnOnce(T) -> U) -> VariableType<U> {
		match self {
			Self::Blank => VariableType::Blank,
			Self::Bool => VariableType::Bool,
			Self::Int => VariableType::Int,
			Self::Float => VariableType::Float,
			Self::String => VariableType::String,
			Self::Callback => VariableType::Callback,
			Self::Domain(t) => VariableType::Domain(map(t)),
		}
	}
}
