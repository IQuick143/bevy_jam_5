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
	/// Discard top value from stack
	DiscardValue,
	// Scope management
	/// Create a new scope
	EnterScope,
	/// Exit a scope
	ExitScope,
	/// Create a new scope for containing macro arguments
	EnterMacroScope,
	// Value access
	/// Create a new variable, return nothing
	CreateVariable(String),
	/// Accesses variable and pushes its reference to stack
	AccessVariable(String),
	/// Accesses $n register and pushes the value to stack
	AccessMacroRegister(usize),
	/// Grab [L, R] (L from the top then R) from stack and perform assignment L = R, returning the new L value
	Assign,
	// Operations
	BinaryOperation(ast::BinaryOperator),
	UnaryOperation(ast::UnaryOperator),
	// Arrays
	/// Push a special literal to stack
	CollectEnd,
	/// Collects all values from stack into an array until it hits either the end of the stack or a [`CollectEnd`] literal
	Collect,
	/// Grab [A, I] (I from the top then A) from stack and calculate the indexed value A[I]
	Index,
	// Loops
	/// Calls a given macro
	StartMacro(String),
	/// Denotes the start of the block segment a macro shall execute
	MacroInnerLabel {
		/// Jump delta to the end of the loop
		end_offset: isize,
		/// A unique id for this loop
		label_id: u32,
	},
	/// Denotes the end of the block segment a macro shall execute
	MacroInnerEndLabel {
		/// Jump delta to the start of the loop
		start_offset: isize,
		/// A unique id for this loop
		label_id: u32,
	},
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
		// Produces a fresh label ID
		let mut next_label = || {
			let label = loop_id_counter;
			loop_id_counter += 1;
			label
		};

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
						ast::ExpressionContent::Assignment(lvalue, rvalue) => {
							working_stack.push(Instruction::Assign.into());
							working_stack.push(lvalue.into());
							working_stack.push(rvalue.into());
						}
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
							instructions.push(Instruction::AccessVariable(identifier.clone()));
						}
						ast::ExpressionContent::LetIdentifier(identifier) => {
							instructions.push(Instruction::CreateVariable(identifier.clone()));
							instructions.push(Instruction::AccessVariable(identifier.clone()));
						}
						ast::ExpressionContent::MacroParameter(value) => {
							instructions.push(Instruction::AccessMacroRegister(*value as usize))
						}
						ast::ExpressionContent::MacroCall(macro_name, argument_list) => {
							let label = next_label();
							working_stack.push(
								Instruction::MacroInnerEndLabel {
									start_offset: 0,
									label_id: label,
								}
								.into(),
							);
							working_stack.push(
								Instruction::MacroInnerLabel {
									end_offset: 0,
									label_id: label,
								}
								.into(),
							);
							// Call the macro
							working_stack
								.push(Instruction::StartMacro(macro_name.to_string()).into());
							for argument in argument_list.named_arguments.iter() {
								working_stack.push(Instruction::Assign.into());
								working_stack.push((&argument.value).into());
								working_stack.push(
									Instruction::CreateVariable(argument.name.clone()).into(),
								);
							}
							working_stack.push(Instruction::EnterMacroScope.into()); // Macro variables go into a macro scope
						}
						ast::ExpressionContent::MethodCall(expression, _, argument_list) => todo!(),
						ast::ExpressionContent::IndexAccess(array_value, index_expr) => {
							working_stack.push(Instruction::Index.into());
							working_stack.push(index_expr.into());
							working_stack.push(array_value.into());
						}
						ast::ExpressionContent::ArrayLiteral(values) => {
							working_stack.push(Instruction::Collect.into());
							for expression in values.iter().rev() {
								working_stack.push(expression.into());
							}
							working_stack.push(Instruction::CollectEnd.into());
						}
						ast::ExpressionContent::FillArrayLiteral(
							inner_expression,
							count_expression,
						) => {
							let label = next_label();
							// Finally collect the array
							working_stack.push(Instruction::Collect.into());
							working_stack.push(
								Instruction::MacroInnerEndLabel {
									start_offset: 0,
									label_id: label,
								}
								.into(),
							);
							working_stack.push(inner_expression.into());
							working_stack.push(
								Instruction::MacroInnerLabel {
									end_offset: 0,
									label_id: label,
								}
								.into(),
							);
							// Call the macro
							working_stack.push(
								Instruction::StartMacro("builtin_FillArray".to_string()).into(),
							);
							working_stack.push(count_expression.into()); // Init the input variable for the macro
							working_stack.push(Instruction::EnterMacroScope.into()); // Macro variables go into a macro scope
																// Array value collection ends here
							working_stack.push(Instruction::CollectEnd.into());
						}
						ast::ExpressionContent::Lambda(bindings, expression) => {
							for (i, variable) in bindings.iter().enumerate() {
								// Make a new variable (let)
								instructions.push(Instruction::CreateVariable(variable.clone()));
								// Access it to get an lvalue
								instructions.push(Instruction::AccessVariable(variable.clone()));
								// Access a macro register to get the appropriate rvalue
								instructions.push(Instruction::AccessMacroRegister(i));
								// Assign
								instructions.push(Instruction::Assign);
								// Discard the implicit-return value of the assignment
								instructions.push(Instruction::DiscardValue);
							}
							working_stack.push(expression.into());
						}
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
