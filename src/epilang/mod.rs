#![allow(unused)] // TODO: Use it somewhere and then drop this

mod ast;
mod interpreter;
mod lex;
mod parser;

/// Defines a position in a source file
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Default, Debug)]
pub struct SourceLocation {
	/// Line number (zero-based)
	pub line: usize,
	/// Column number within the line (zero-based)
	pub column: usize,
}

impl SourceLocation {
	fn new(line: usize, column: usize) -> Self {
		Self { line, column }
	}
}

impl std::fmt::Display for SourceLocation {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		f.write_fmt(format_args!("{}:{}", self.line, self.column))
	}
}

#[cfg(test)]
mod test {
	use super::{
		ast::*,
		lex::{tokenize, LexerError, LexerErrorCode, Token},
		parser::parse,
		SourceLocation,
	};

	fn get_lex_err(text: &str) -> LexerError {
		tokenize(text).collect::<Result<Vec<_>, _>>().unwrap_err()
	}

	fn get_tokens(text: &str) -> Vec<(Token, std::ops::Range<SourceLocation>)> {
		tokenize(text).collect::<Result<Vec<_>, _>>().unwrap()
	}

	fn get_ast(text: &str) -> Sequence {
		parse(get_tokens(text)).unwrap()
	}

	#[test]
	fn lexer_basic_test() {
		assert_eq!(
			get_tokens("let a = 42;"),
			vec![
				(
					Token::Let,
					SourceLocation::new(0, 0)..SourceLocation::new(0, 3)
				),
				(
					Token::Identifier("a".to_owned()),
					SourceLocation::new(0, 4)..SourceLocation::new(0, 5)
				),
				(
					Token::Assign,
					SourceLocation::new(0, 6)..SourceLocation::new(0, 7)
				),
				(
					Token::IntLiteral(42),
					SourceLocation::new(0, 8)..SourceLocation::new(0, 10)
				),
				(
					Token::Semicolon,
					SourceLocation::new(0, 10)..SourceLocation::new(0, 11)
				),
			]
		)
	}

	#[test]
	fn lexer_multiline_test() {
		let text = r"
4.map {
  let k = ks[$1];  # Get weight from prepared array
  $ * k
}
";
		let tokens = vec![
			(
				Token::IntLiteralDot(4),
				SourceLocation::new(1, 0)..SourceLocation::new(1, 2),
			),
			(
				Token::Identifier("map".to_owned()),
				SourceLocation::new(1, 2)..SourceLocation::new(1, 5),
			),
			(
				Token::OpenBrace,
				SourceLocation::new(1, 6)..SourceLocation::new(1, 7),
			),
			(
				Token::Let,
				SourceLocation::new(2, 2)..SourceLocation::new(2, 5),
			),
			(
				Token::Identifier("k".to_owned()),
				SourceLocation::new(2, 6)..SourceLocation::new(2, 7),
			),
			(
				Token::Assign,
				SourceLocation::new(2, 8)..SourceLocation::new(2, 9),
			),
			(
				Token::Identifier("ks".to_owned()),
				SourceLocation::new(2, 10)..SourceLocation::new(2, 12),
			),
			(
				Token::OpenBracket,
				SourceLocation::new(2, 12)..SourceLocation::new(2, 13),
			),
			(
				Token::MacroParameter(1),
				SourceLocation::new(2, 13)..SourceLocation::new(2, 15),
			),
			(
				Token::CloseBracket,
				SourceLocation::new(2, 15)..SourceLocation::new(2, 16),
			),
			(
				Token::Semicolon,
				SourceLocation::new(2, 16)..SourceLocation::new(2, 17),
			),
			(
				Token::MacroParameter(0),
				SourceLocation::new(3, 2)..SourceLocation::new(3, 3),
			),
			(
				Token::Asterisk,
				SourceLocation::new(3, 4)..SourceLocation::new(3, 5),
			),
			(
				Token::Identifier("k".to_owned()),
				SourceLocation::new(3, 6)..SourceLocation::new(3, 7),
			),
			(
				Token::CloseBrace,
				SourceLocation::new(4, 0)..SourceLocation::new(4, 1),
			),
		];

		assert_eq!(get_tokens(text), tokens);
	}

	#[test]
	fn lexer_illegal_character() {
		let err = get_lex_err("abc\u{12}ef");
		assert_eq!(
			err.loc,
			SourceLocation::new(0, 3)..SourceLocation::new(0, 4)
		);
		assert_eq!(err.slice, "\u{12}");
		assert_eq!(err.error_code, LexerErrorCode::Generic);
	}

	#[test]
	fn lexer_incomplete_token() {
		let err = get_lex_err("abc&1");
		assert_eq!(
			err.loc,
			SourceLocation::new(0, 3)..SourceLocation::new(0, 4)
		);
		assert_eq!(err.slice, "&");
		assert_eq!(err.error_code, LexerErrorCode::Generic);
	}

	#[test]
	fn lexer_int_literal_overflow() {
		let err = get_lex_err("9999999999999999");
		assert_eq!(
			err.loc,
			SourceLocation::new(0, 0)..SourceLocation::new(0, 16)
		);
		assert_eq!(err.slice, "9999999999999999");
		assert_eq!(err.error_code, LexerErrorCode::IntParseFailure);
	}

	#[test]
	fn simple_expression() {
		assert_eq!(
			get_ast("a = 4 + $0"),
			Sequence {
				statements: Vec::new(),
				tail: Some(Expression {
					loc: SourceLocation::new(0, 0)..SourceLocation::new(0, 10),
					value: Box::new(ExpressionContent::Assignment(
						Expression {
							loc: SourceLocation::new(0, 0)..SourceLocation::new(0, 1),
							value: Box::new(ExpressionContent::Identifier("a".to_owned())),
						},
						Expression {
							loc: SourceLocation::new(0, 4)..SourceLocation::new(0, 10),
							value: Box::new(ExpressionContent::Operation(
								OperationExpression::BinaryPlus(
									Expression {
										loc: SourceLocation::new(0, 4)..SourceLocation::new(0, 5),
										value: Box::new(ExpressionContent::IntLiteral(4)),
									},
									Expression {
										loc: SourceLocation::new(0, 8)..SourceLocation::new(0, 10),
										value: Box::new(ExpressionContent::MacroParameter(0)),
									}
								)
							))
						}
					))
				})
			}
		)
	}

	#[test]
	fn precedence_test() {
		assert_eq!(get_ast("a = x => z = 42 + -n * x ** 2 ** (n - 1) / 3 - x"), Sequence {
			statements: Vec::new(),
			tail: Some(Expression {
				loc: SourceLocation::new(0, 0)..SourceLocation::new(0, 48),
				value: Box::new(ExpressionContent::Assignment(
					Expression {
						loc: SourceLocation::new(0, 0)..SourceLocation::new(0, 1),
						value: Box::new(ExpressionContent::Identifier("a".to_owned())),
					},
					Expression {
						loc: SourceLocation::new(0, 4)..SourceLocation::new(0, 48),
						value: Box::new(ExpressionContent::Lambda(
							vec!["x".to_owned()],
							Expression {
								loc: SourceLocation::new(0, 9)..SourceLocation::new(0, 48),
								value: Box::new(ExpressionContent::Assignment(
									Expression {
										loc: SourceLocation::new(0, 9)..SourceLocation::new(0, 10),
										value: Box::new(ExpressionContent::Identifier("z".to_owned())),
									},
									Expression {
										loc: SourceLocation::new(0, 13)..SourceLocation::new(0, 48),
										value: Box::new(ExpressionContent::Operation(OperationExpression::BinaryMinus(
											Expression {
												loc: SourceLocation::new(0, 13)..SourceLocation::new(0, 44),
												value: Box::new(ExpressionContent::Operation(OperationExpression::BinaryPlus(
													Expression {
														loc: SourceLocation::new(0, 13)..SourceLocation::new(0, 15),
														value: Box::new(ExpressionContent::IntLiteral(42)),
													},
													Expression {
														loc: SourceLocation::new(0, 18)..SourceLocation::new(0, 44),
														value: Box::new(ExpressionContent::Operation(OperationExpression::Divide(
															Expression {
																loc: SourceLocation::new(0, 18)..SourceLocation::new(0, 40),
																value: Box::new(ExpressionContent::Operation(OperationExpression::Multiply(
																	Expression {
																		loc: SourceLocation::new(0, 18)..SourceLocation::new(0, 20),
																		value: Box::new(ExpressionContent::Operation(OperationExpression::UnaryMinus(Expression {
																			loc: SourceLocation::new(0, 19)..SourceLocation::new(0, 20),
																			value: Box::new(ExpressionContent::Identifier("n".to_owned())),
																		}))),
																	},
																	Expression {
																		loc: SourceLocation::new(0, 23)..SourceLocation::new(0, 40),
																		value: Box::new(ExpressionContent::Operation(OperationExpression::Power(
																			Expression {
																				loc: SourceLocation::new(0, 23)..SourceLocation::new(0, 24),
																				value: Box::new(ExpressionContent::Identifier("x".to_owned())),
																			},
																			Expression {
																				loc: SourceLocation::new(0, 28)..SourceLocation::new(0, 40),
																				value: Box::new(ExpressionContent::Operation(OperationExpression::Power(
																					Expression {
																						loc: SourceLocation::new(0, 28)..SourceLocation::new(0, 29),
																						value: Box::new(ExpressionContent::IntLiteral(2)),
																					},
																					Expression {
																						loc: SourceLocation::new(0, 33)..SourceLocation::new(0, 40),
																						value: Box::new(ExpressionContent::Operation(OperationExpression::BinaryMinus(
																							Expression {
																								loc: SourceLocation::new(0, 34)..SourceLocation::new(0, 35),
																								value: Box::new(ExpressionContent::Identifier("n".to_owned())),
																							},
																							Expression {
																								loc: SourceLocation::new(0, 38)..SourceLocation::new(0, 39),
																								value: Box::new(ExpressionContent::IntLiteral(1)),
																							},
																						)))
																					},
																				)))
																			},
																		)))
																	},
																)))
															},
															Expression {
																loc: SourceLocation::new(0, 43)..SourceLocation::new(0, 44),
																value: Box::new(ExpressionContent::IntLiteral(3)),
															},
														)))
													},
												)))
											},
											Expression {
												loc: SourceLocation::new(0, 47)..SourceLocation::new(0, 48),
												value: Box::new(ExpressionContent::Identifier("x".to_owned())),
											},
										)))
									}
								))
							}
						))
					}
				))
			})
		})
	}
}
