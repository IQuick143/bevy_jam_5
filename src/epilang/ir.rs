use std::{borrow::Borrow, i32};

use bevy::text::cosmic_text::ttf_parser::gsub::Sequence;

use super::ast::{self, Expression};

struct Program(Vec<Instruction>);

enum Instruction {
	// Literal values
	PushBlank,
	PushBool(bool),
	PushInt(i32),
	PushFloat(f32),
	PushString(String),
	// Value discarding
	DiscardValue,
	// Scope management
	EnterScope,
	ExitScope,
	EnterMacroScope,
	ExitMacroScope,
	PushDollar,
	PopDollar,
	// Value access
	CreateVariable(String),
	AccessVariable(String),
	AccessMacroRegister(usize),
	// Operations
	BinaryOperation(ast::BinaryOperator),
	UnaryOperation(ast::UnaryOperator),
	// Arrays
	Collect(usize),
	// Loops
	EnterLoop,
	LoopStartLabel {
		/// Jump delta to the end of the loop
		end_offset: isize,
		/// A unique id for this loop
		loop_label: u32,
	},
	LoopEndLabel {
		/// Jump delta to the start of the loop
		start_offset: isize,
		/// A unique id for this loop
		loop_label: u32,
	},
	ExitLoop,
}

enum Token<'expr> {
	Expr(&'expr Expression),
	Instruction(Instruction),
}

impl Program {
	/// Compiles a program from an AST into a sequence of instructions.
	pub fn compile(script: &Expression) -> Self {
		let mut working_stack: Vec<Token> = Vec::new();
		working_stack.push(script.into());

		let mut instructions = Vec::new();

		// Counter with the value of the next free loop label.
		let mut loop_id_counter: u32 = 0;

		while let Some(token) = working_stack.pop() {
			match token {
				Token::Instruction(instruction) => {
					instructions.push(instruction);
				}
				Token::Expr(expr) => {
					match expr.value.as_ref() {
						ast::ExpressionContent::Sequence(sequence) => {
							// Push a scope
							instructions.push(Instruction::EnterScope);
							// Remaining instructions go into the working stack in reverse order to be processed later
							working_stack.push(Instruction::ExitScope.into());
							// The return value is either the tail expression or a Blank.
							if let Some(expression) = sequence.tail.borrow() {
								working_stack.push(expression.into());
							} else {
								working_stack.push(Instruction::PushBlank.into());
							}
							// Other expressions have their values discarded
							for expression in sequence.statements.iter().rev() {
								working_stack.push(Instruction::DiscardValue.into());
								working_stack.push(expression.into());
							}
						}
						ast::ExpressionContent::Assignment(expression, expression1) => todo!(),
						ast::ExpressionContent::IntLiteral(value) => {
							instructions.push(Instruction::PushInt(
								(*value).try_into().unwrap_or(i32::MAX), // TODO: better handling
							));
						}
						ast::ExpressionContent::FloatLiteral(value) => {
							instructions.push(Instruction::PushFloat(*value));
						}
						ast::ExpressionContent::StringLiteral(value) => {
							instructions.push(Instruction::PushString(value.clone()));
						}
						ast::ExpressionContent::Blank => {
							instructions.push(Instruction::PushBlank);
						}
						ast::ExpressionContent::Identifier(identifier) => {
							instructions.push(Instruction::AccessVariable(identifier.clone()))
						}
						ast::ExpressionContent::LetIdentifier(identifier) => {
							instructions.push(Instruction::CreateVariable(identifier.clone()))
						}
						ast::ExpressionContent::MacroParameter(value) => {
							instructions.push(Instruction::AccessMacroRegister(*value as usize))
						}
						ast::ExpressionContent::MacroCall(_, argument_list) => todo!(),
						ast::ExpressionContent::MethodCall(expression, _, argument_list) => todo!(),
						ast::ExpressionContent::IndexAccess(expression, expression1) => todo!(),
						ast::ExpressionContent::ArrayLiteral(values) => {
							working_stack.push(Instruction::Collect(values.len()).into());
							for expression in values.iter().rev() {
								working_stack.push(expression.into());
							}
						}
						ast::ExpressionContent::FillArrayLiteral(
							inner_expression,
							count_expression,
						) => {
							working_stack.push(Instruction::EnterLoop.into());
							working_stack.push(count_expression.into());
						}
						ast::ExpressionContent::Lambda(vec, expression) => todo!(),
						ast::ExpressionContent::Operation(operation_expression) => {
							match operation_expression {
								ast::OperationExpression::Unary(unary_operator, expression) => {
									working_stack.push(expression.into());
									working_stack
										.push(Instruction::UnaryOperation(*unary_operator).into());
								}
								ast::OperationExpression::Binary(binary_operator, left, right) => {
									working_stack.push(right.into());
									working_stack.push(left.into());
									working_stack.push(
										Instruction::BinaryOperation(*binary_operator).into(),
									);
								}
							}
						}
					}
				}
			}
		}

		let mut program = Program(instructions);
		program.compute_offsets();
		program
	}

	fn compute_offsets(&mut self) {
		todo!();
	}
}

impl<'a> From<Instruction> for Token<'a> {
	fn from(value: Instruction) -> Self {
		Token::Instruction(value)
	}
}

impl<'a> From<&'a Expression> for Token<'a> {
	fn from(value: &'a Expression) -> Self {
		Token::Expr(value)
	}
}
