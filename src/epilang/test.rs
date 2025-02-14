use super::{builtins::*, compile, interpreter::*, values::*, SourceLocation};

/// Testing interpreter backend that provides no functions
#[derive(Clone, Copy, Debug, Default)]
struct NoBackend;

impl InterpreterBackend for NoBackend {
	type Error = std::convert::Infallible;
	type Warning = std::convert::Infallible;
	type Value = NoDomainValue;
}

macro_rules! get_variable {
	( $interpreter:ident . $var:ident ) => {
		$interpreter.variable_pool.load(stringify!($var))
	};
}

/// Retrieves a variable from the variable pool of an interpreter.
/// Panics if the variable does not exist.
macro_rules! variable {
	( $interpreter:ident . $var:ident ) => {
		get_variable!($interpreter.$var).unwrap()
	};
}

macro_rules! assert_float_almost_eq {
	( $left:expr, $right:expr $(,)? ) => {
		assert_float_almost_eq!($left, $right, 0.0)
	};
	( $left:expr, $right:expr, $thres:expr $(,)? ) => {
		match (&$left, &$right, &$thres) {
			(VariableValue::Float(_left), _right, _thres) => {
				let dif = (*_left - *_right).abs();
				let tol = (_left.abs() + _right.abs()) * f32::EPSILON + *_thres;
				assert!(dif <= tol, "Expressions are not equal.\n\tlhs: {} which is {_left}\n\trhs: {} which is {_right}\n\ttolerance: {tol}", stringify!($left), stringify!($right));
			}
			(_left, _, _) => {
				panic!("Expression {} should have evaluated to floating-point, instead, it is {_left:?}", stringify!($left));
			}
		}
	};
}

#[test]
fn variable_manipulation() {
	let module = compile(
		r"
a = 1;
b = 2.4;
c = _;
d = e = f = b;
f = 'hello';
	",
	)
	.unwrap();
	let mut interpreter = Interpreter::new(&module, NoBackend);
	assert!(interpreter.run(1000).is_ok());
	assert!(matches!(variable!(interpreter.a), VariableValue::Int(1)));
	assert!(matches!(
		variable!(interpreter.b),
		VariableValue::Float(2.4)
	));
	assert!(matches!(variable!(interpreter.c), VariableValue::Blank));
	assert!(matches!(
		variable!(interpreter.d),
		VariableValue::Float(2.4)
	));
	assert!(matches!(
		variable!(interpreter.e),
		VariableValue::Float(2.4)
	));
	assert!(matches!(
		variable!(interpreter.f),
		VariableValue::String("hello")
	));
	assert!(interpreter.get_warnings().next().is_none());
}

#[test]
fn string_escape_sequences() {
	let module = compile(
		r#"
a = 'Hello''World';
b = 'Hello''''';
c = "\n\"\\\"\\\\";
	"#,
	)
	.unwrap();
	let mut interpreter = Interpreter::new(&module, NoBackend);
	assert!(interpreter.run(1000).is_ok());
	assert!(matches!(
		variable!(interpreter.a),
		VariableValue::String("Hello'World")
	));
	assert!(matches!(
		variable!(interpreter.b),
		VariableValue::String("Hello''")
	));
	assert!(matches!(
		variable!(interpreter.c),
		VariableValue::String("\n\"\\\"\\\\")
	));
}

#[test]
fn invalid_escape_sequences() {
	use super::lex::{LexerError, LexerErrorCode};
	assert_eq!(
		compile(r#"a = "\w";"#).unwrap_err(),
		LexerError {
			error_code: LexerErrorCode::InvalidEscapeCharacter('w'),
			slice: r#""\w""#.to_owned(),
			loc: SourceLocation::new(0, 4)..SourceLocation::new(0, 8),
		}
		.into()
	);
	assert_eq!(
		compile(
			r#"a = "abc\
	""#
		)
		.unwrap_err(),
		LexerError {
			error_code: LexerErrorCode::Generic,
			slice: r#""abc"#.to_owned(),
			loc: SourceLocation::new(0, 4)..SourceLocation::new(0, 8),
		}
		.into()
	);
}

#[test]
fn undeclared_variable() {
	let module = compile("a;").unwrap();
	let err = Interpreter::new(&module, NoBackend).run(1000).unwrap_err();
	let expected_loc = SourceLocation::new(0, 0)..SourceLocation::new(0, 1);
	assert_eq!(
		err,
		InterpreterError::LogicError(
			Box::new(LogicError::VariableDoesNotExist("a".to_owned())),
			expected_loc
		)
	);
}

#[test]
fn callbacks() {
	let module = compile(
		r"
a = fn {
	k = 42;
};
	",
	)
	.unwrap();
	let mut interpreter = Interpreter::new(&module, NoBackend);
	assert!(interpreter.run(1000).is_ok());
	assert!(interpreter.get_warnings().next().is_none());
	let inner_module = variable!(interpreter.a).try_into().unwrap();
	interpreter = Interpreter::new(inner_module, NoBackend);
	assert!(interpreter.run(1000).is_ok());
	assert!(matches!(variable!(interpreter.k), VariableValue::Int(42)));
	assert!(interpreter.get_warnings().next().is_none());
}

#[test]
fn functions() {
	struct MockBackend {
		invocation_count: u32,
	}

	impl InterpreterBackend for MockBackend {
		type Error = std::convert::Infallible;
		type Warning = std::convert::Infallible;
		type Value = NoDomainValue;

		fn call_function<'a>(
			&mut self,
			function_name: &str,
			mut args: ArgumentStream<'a, '_, NoDomainValue>,
			_: WarningSink<Self::Warning>,
		) -> Result<
			ReturnValue<'a, Self::Value>,
			FunctionCallError<Self::Error, <Self::Value as DomainVariableValue>::Type>,
		> {
			match self.invocation_count {
				0 => {
					assert_eq!(function_name, "myfun");
					assert_eq!(args.read_single_as(), Ok(0.0));
				}
				1 => {
					assert_eq!(function_name, "otherfun");
					assert_eq!(args.read_as(), Ok(42));
					assert_eq!(args.read_as(), Ok("hello"));
					assert!(args.read_separator().is_ok());
					assert!(args.read_blank().is_ok());
					assert!(args.read_end().is_ok());
				}
				2.. => panic!("Function was called more times than expected"),
			}
			self.invocation_count += 1;
			Ok(ReturnValue::pure(VariableValue::Bool(true)))
		}
	}

	let module = compile("a = myfun(0.0); b = otherfun(42, 'hello'; _);").unwrap();
	let mut interpreter = Interpreter::new(
		&module,
		MockBackend {
			invocation_count: 0,
		},
	);
	assert!(interpreter.run(1000).is_ok());
	assert!(matches!(
		variable!(interpreter.a),
		VariableValue::Bool(true)
	));
	assert!(matches!(
		variable!(interpreter.b),
		VariableValue::Bool(true)
	));
	assert!(interpreter.get_warnings().next().is_none());
	assert_eq!(interpreter.backend.invocation_count, 2);
}

#[test]
fn undeclared_function() {
	let module = compile("f();").unwrap();
	let err = Interpreter::new(&module, NoBackend).run(1000).unwrap_err();
	let expected_loc = SourceLocation::new(0, 0)..SourceLocation::new(0, 3);
	assert_eq!(
		err,
		InterpreterError::LogicError(
			Box::new(LogicError::FunctionCall(
				FunctionCallError::FunctionDoesNotExist,
				"f".to_owned()
			)),
			expected_loc
		)
	);
}

#[test]
fn builtins() {
	let module = compile(
		r"
sin0 = sin(0);
sinpi6 = sin(pi / 6);
sinpi2 = sin(pi / 2);
sinpi = sin(pi);
cos0 = cos(0);
cospi2 = cos(pi / 2);
cospi = cos(pi);
tan0 = tan(0);
tanpi4 = tan(pi / 4);
sqrt4 = sqrt(4);
sqrt2 = sqrt(2);
floor1 = floor(1);
floor16 = floor(1.6);
ceil1 = ceil(1);
ceil12 = ceil(1.2);
round12 = round(1.2);
round16 = round(1.6);
abs1 = abs(1);
absm4 = abs(-4);
absfm2 = abs(-2.5);
inti = int(1);
intf = int(2.5);
intbt = int(0 == 0);
intbf = int(1 == 0);
	",
	)
	.unwrap();
	let mut interpreter = Interpreter::new(&module, DefaultInterpreterBackend);
	interpreter.variable_pool = default_builtin_variables();
	assert!(interpreter.run(1000).is_ok());
	assert_float_almost_eq!(variable!(interpreter.sin0), 0.0);
	assert_float_almost_eq!(variable!(interpreter.sinpi6), 0.5);
	assert_float_almost_eq!(variable!(interpreter.sinpi2), 1.0);
	assert_float_almost_eq!(variable!(interpreter.sinpi), 0.0, 1e-6);
	assert_float_almost_eq!(variable!(interpreter.cos0), 1.0);
	assert_float_almost_eq!(variable!(interpreter.cospi2), 0.0, 1e-6);
	assert_float_almost_eq!(variable!(interpreter.cospi), -1.0);
	assert_float_almost_eq!(variable!(interpreter.tan0), 0.0);
	assert_float_almost_eq!(variable!(interpreter.tanpi4), 1.0);
	assert_float_almost_eq!(variable!(interpreter.sqrt4), 2.0);
	assert_float_almost_eq!(variable!(interpreter.sqrt2), std::f32::consts::SQRT_2);
	assert_float_almost_eq!(variable!(interpreter.floor1), 1.0);
	assert_float_almost_eq!(variable!(interpreter.floor16), 1.0);
	assert_float_almost_eq!(variable!(interpreter.ceil1), 1.0);
	assert_float_almost_eq!(variable!(interpreter.ceil12), 2.0);
	assert_float_almost_eq!(variable!(interpreter.round12), 1.0);
	assert_float_almost_eq!(variable!(interpreter.round16), 2.0);
	assert_float_almost_eq!(variable!(interpreter.absfm2), 2.5);
	assert!(matches!(variable!(interpreter.abs1), VariableValue::Int(1)));
	assert!(matches!(
		variable!(interpreter.absm4),
		VariableValue::Int(4)
	));
	assert!(matches!(variable!(interpreter.inti), VariableValue::Int(1)));
	assert!(matches!(variable!(interpreter.intf), VariableValue::Int(2)));
	assert!(matches!(
		variable!(interpreter.intbt),
		VariableValue::Int(1)
	));
	assert!(matches!(
		variable!(interpreter.intbf),
		VariableValue::Int(0)
	));
	assert!(interpreter.get_warnings().next().is_none());
}

#[test]
fn int_arithmetics() {
	let module = compile(
		r"
un_plus = +42;
un_minus = -23;
bin_plus = 1 + 1;
bin_minus = 2 - 3;
mul = 3 * 4;
div = 6 / 4;
int_div = 6 // 4;
modulo = 6 % 4;
pow = 5 ** 3;
	",
	)
	.unwrap();
	let mut interpreter = Interpreter::new(&module, NoBackend);
	assert!(interpreter.run(1000).is_ok());
	assert!(matches!(
		variable!(interpreter.un_plus),
		VariableValue::Int(42)
	));
	assert!(matches!(
		variable!(interpreter.un_minus),
		VariableValue::Int(-23)
	));
	assert!(matches!(
		variable!(interpreter.bin_plus),
		VariableValue::Int(2)
	));
	assert!(matches!(
		variable!(interpreter.bin_minus),
		VariableValue::Int(-1)
	));
	assert!(matches!(variable!(interpreter.mul), VariableValue::Int(12)));
	assert!(matches!(
		variable!(interpreter.div),
		VariableValue::Float(1.5)
	));
	assert!(matches!(
		variable!(interpreter.int_div),
		VariableValue::Int(1)
	));
	assert!(matches!(
		variable!(interpreter.modulo),
		VariableValue::Int(2)
	));
	assert!(matches!(
		variable!(interpreter.pow),
		VariableValue::Int(125)
	));
	assert!(interpreter.get_warnings().next().is_none());
}

#[test]
fn float_arithmetics() {
	let module = compile(
		r"
un_plus = +12.2;
un_minus = -32.6;
bin_plus = 123.0 + 112.5;
bin_minus = 12.4 - 23.1;
mul = 0.5 * 0.4;
div = 12.3 / 3.0;
pow = 2.25 ** 1.5;
	",
	)
	.unwrap();
	let mut interpreter = Interpreter::new(&module, NoBackend);
	assert!(interpreter.run(1000).is_ok());
	assert_float_almost_eq!(variable!(interpreter.un_plus), 12.2);
	assert_float_almost_eq!(variable!(interpreter.un_minus), -32.6);
	assert_float_almost_eq!(variable!(interpreter.bin_plus), 235.5);
	assert_float_almost_eq!(variable!(interpreter.bin_minus), -10.7);
	assert_float_almost_eq!(variable!(interpreter.mul), 0.2);
	assert_float_almost_eq!(variable!(interpreter.div), 4.1);
	assert_float_almost_eq!(variable!(interpreter.pow), 3.375);
	assert!(interpreter.get_warnings().next().is_none());
}

#[test]
fn mixed_arithmetics() {
	let module = compile(
		r"
plus_l = 42 + 112.5;
plus_r = 123.0 + 23;
minus_l = 3 - 23.04;
minus_r = 11.2 - 6;
mul_l = 2 * 0.4;
mul_r = 0.6 * 4;
div_l = 11 / 3.0;
div_r = 14.2 / 5;
pow_l = 9 ** 1.5;
pow_r = 1.5 ** 3;
	",
	)
	.unwrap();
	let mut interpreter = Interpreter::new(&module, NoBackend);
	assert!(interpreter.run(1000).is_ok());
	assert_float_almost_eq!(variable!(interpreter.plus_l), 154.5);
	assert_float_almost_eq!(variable!(interpreter.plus_r), 146.0);
	assert_float_almost_eq!(variable!(interpreter.minus_l), -20.04);
	assert_float_almost_eq!(variable!(interpreter.minus_r), 5.2);
	assert_float_almost_eq!(variable!(interpreter.mul_l), 0.8);
	assert_float_almost_eq!(variable!(interpreter.mul_r), 2.4);
	assert_float_almost_eq!(variable!(interpreter.div_l), 3.666666);
	assert_float_almost_eq!(variable!(interpreter.div_r), 2.84);
	assert_float_almost_eq!(variable!(interpreter.pow_l), 27.0);
	assert_float_almost_eq!(variable!(interpreter.pow_r), 3.375);
	assert!(interpreter.get_warnings().next().is_none());
}

#[test]
fn comparison() {
	let module = compile(
		r"
ltl = 1 < 2.0;
ltg = 1 < 0;
lte = 1.0 < 1;
lel = 1 <= 2;
leg = 1.0 <= 0.0;
lee = 1 <= 1;
gtl = 1.0 > 2;
gtg = 1 > 0.0;
gte = 1 > 1;
gel = 1 >= 2.0;
geg = 1.0 >= 0.0;
gee = 1 >= 1;
eql = 1.0 == 2;
eqg = 1 == 0.0;
eqe = 1 == 1.0;
nel = 1.0 != 2.0;
neg = 1 != 0;
nee = 1 != 1.0;
	",
	)
	.unwrap();
	let mut interpreter = Interpreter::new(&module, NoBackend);
	assert!(interpreter.run(1000).is_ok());
	assert!(matches!(
		variable!(interpreter.ltl),
		VariableValue::Bool(true)
	));
	assert!(matches!(
		variable!(interpreter.ltg),
		VariableValue::Bool(false)
	));
	assert!(matches!(
		variable!(interpreter.lte),
		VariableValue::Bool(false)
	));
	assert!(matches!(
		variable!(interpreter.lel),
		VariableValue::Bool(true)
	));
	assert!(matches!(
		variable!(interpreter.leg),
		VariableValue::Bool(false)
	));
	assert!(matches!(
		variable!(interpreter.lee),
		VariableValue::Bool(true)
	));
	assert!(matches!(
		variable!(interpreter.gtl),
		VariableValue::Bool(false)
	));
	assert!(matches!(
		variable!(interpreter.gtg),
		VariableValue::Bool(true)
	));
	assert!(matches!(
		variable!(interpreter.gte),
		VariableValue::Bool(false)
	));
	assert!(matches!(
		variable!(interpreter.gel),
		VariableValue::Bool(false)
	));
	assert!(matches!(
		variable!(interpreter.geg),
		VariableValue::Bool(true)
	));
	assert!(matches!(
		variable!(interpreter.gee),
		VariableValue::Bool(true)
	));
	assert!(matches!(
		variable!(interpreter.eql),
		VariableValue::Bool(false)
	));
	assert!(matches!(
		variable!(interpreter.eqg),
		VariableValue::Bool(false)
	));
	assert!(matches!(
		variable!(interpreter.eqe),
		VariableValue::Bool(true)
	));
	assert!(matches!(
		variable!(interpreter.nel),
		VariableValue::Bool(true)
	));
	assert!(matches!(
		variable!(interpreter.neg),
		VariableValue::Bool(true)
	));
	assert!(matches!(
		variable!(interpreter.nee),
		VariableValue::Bool(false)
	));
	assert!(interpreter.get_warnings().next().is_none());
}

#[test]
fn precedence() {
	// 99 % of people on Facebook cannot solve this, but my interpreter can
	let module = compile("a = -2 ** 2 * 5 + 2 / 2 ** 2 ** (1 + 1) * 4 - 1 / 3;").unwrap();
	let mut interpreter = Interpreter::new(&module, NoBackend);
	assert!(interpreter.run(1000).is_ok());
	assert!(matches!(
		variable!(interpreter.a),
		VariableValue::Float(20.166666)
	));
	assert!(interpreter.get_warnings().next().is_none());
}

#[test]
fn short_circuiting_logic() {
	let module = compile(
		r"
x =
(a = 1 == 1) &&
(b = 1 == 1) ||
(c = 1 == 1) &&
(d = 0 == 1) ||
(e = 0 == 1) &&
(f = 1 == 1);
	",
	)
	.unwrap();
	let mut interpreter = Interpreter::new(&module, NoBackend);
	assert!(interpreter.run(1000).is_ok());
	assert!(matches!(
		variable!(interpreter.a),
		VariableValue::Bool(true)
	));
	assert!(matches!(
		variable!(interpreter.b),
		VariableValue::Bool(true)
	));
	assert!(get_variable!(interpreter.c).is_none());
	assert!(matches!(
		variable!(interpreter.d),
		VariableValue::Bool(false)
	));
	assert!(matches!(
		variable!(interpreter.e),
		VariableValue::Bool(false)
	));
	assert!(get_variable!(interpreter.f).is_none());
	assert!(interpreter.get_warnings().next().is_none());
}

#[test]
fn conditionals() {
	let module = compile(
		r"
if 0 == 0 {
	true_one = 1;
}

if 1 == 0 {
	false_one = 1;
}

if 1 == 1 {
	true_two = 2;
} else if 0 == 0 {
	false_two = 2;
} else {
	false_two = 2;
}

if 0 == 1 {
	false_three = 3;
} else if 1 == 1 {
	true_three = 3;
} else {
	false_three = 3;
}

if 0 == 1 {
	false_four = 4;
} else if 1 == 2 {
	false_four = 4;
} else {
	true_four = 4;
}
	",
	)
	.unwrap();
	let mut interpreter = Interpreter::new(&module, NoBackend);
	assert!(interpreter.run(1000).is_ok());
	assert!(matches!(
		variable!(interpreter.true_one),
		VariableValue::Int(1)
	));
	assert!(matches!(
		variable!(interpreter.true_two),
		VariableValue::Int(2)
	));
	assert!(matches!(
		variable!(interpreter.true_three),
		VariableValue::Int(3)
	));
	assert!(matches!(
		variable!(interpreter.true_four),
		VariableValue::Int(4)
	));
	assert!(get_variable!(interpreter.false_one).is_none());
	assert!(get_variable!(interpreter.false_two).is_none());
	assert!(get_variable!(interpreter.false_three).is_none());
	assert!(get_variable!(interpreter.false_four).is_none());
	assert!(interpreter.get_warnings().next().is_none());
}

#[test]
fn conditional_type_errors() {
	let module = compile("if 'hi' {}").unwrap();
	let mut interpreter = Interpreter::new(&module, NoBackend);
	let expected_loc = SourceLocation::new(0, 3)..SourceLocation::new(0, 7);
	assert_eq!(
		interpreter.run(1000).unwrap_err(),
		InterpreterError::LogicError(
			Box::new(LogicError::IllegalConditionType(VariableType::String)),
			expected_loc
		)
	);
}

#[test]
fn negative_int_pow() {
	let module = compile("42 ** -2;").unwrap();
	let mut interpreter = Interpreter::new(&module, NoBackend);
	let expected_loc = SourceLocation::new(0, 0)..SourceLocation::new(0, 8);
	assert_eq!(
		interpreter.run(1000).unwrap_err(),
		InterpreterError::LogicError(Box::new(LogicError::NegativeIntPow), expected_loc)
	);
}

#[test]
fn abs_integer_overflow() {
	let module = compile("abs(-1 - 0x7fffffff);").unwrap();
	let mut interpreter = Interpreter::new(&module, DefaultInterpreterBackend);
	let err = interpreter.run(1000).unwrap_err();
	let expected_loc = SourceLocation::new(0, 0)..SourceLocation::new(0, 20);
	assert_eq!(
		err,
		InterpreterError::LogicError(
			Box::new(LogicError::FunctionCall(
				FunctionCallError::Domain(ArithmeticOverflowError),
				"abs".to_owned()
			)),
			expected_loc
		)
	);
}

#[test]
fn must_use_warning() {
	let module = compile(
		r"
1;
12.5;
'hello';
1 + 2.3;
pi;
abs(-3);
fn {};
1 <= 42;
0 == 1 && 0 == 0;
1 == 1 && 1 == 0;
0 == 1 || 0 == 0;
1 == 1 || 1 == 0;
	",
	)
	.unwrap();
	let mut interpreter = Interpreter::new(&module, DefaultInterpreterBackend);
	interpreter.variable_pool = default_builtin_variables();
	assert!(interpreter.run(1000).is_ok());
	let warnings = interpreter
		.get_warnings()
		.map(|w| w.warning_code)
		.collect::<Vec<_>>();
	assert_eq!(warnings, [const { WarningCode::DiscardedMustUse }; 12]);
}

#[test]
fn overwritte_builtin_warning() {
	let module = compile("a = 1;").unwrap();
	let mut interpreter = Interpreter::new(&module, NoBackend);
	interpreter.variable_pool = VariablePool::from_iter([("a", 42.into())]);
	assert!(interpreter.run(1000).is_ok());
	let warnings = interpreter
		.get_warnings()
		.map(|w| w.warning_code)
		.collect::<Vec<_>>();
	assert_eq!(warnings, [WarningCode::OverwrittenBuiltin("a".to_owned())]);
}

#[test]
fn comment_on_last_line() {
	let program = "# test comment";
	let module = compile(program).unwrap();
	assert!(module.0 .0.is_empty());
}
