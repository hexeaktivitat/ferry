use std::collections::HashMap;

use miette::{Diagnostic, Result, SourceSpan};
use thiserror::Error;

use crate::{
    state::{FerryState, FerryValue},
    syntax::{
        walk_expr, Assign, Binary, Binding, Call, Expr, ExprVisitor, For, Function, Group, If, Lit,
        Loop, Unary, Variable,
    },
};

/// Intermediate Representation for FerryVM
/// Part of compilation process, intended to be a high-level assembly language
/// similar to LLVM-IR, cranelift-IR, etc.

#[derive(Error, Diagnostic, Debug)]
pub enum FerryIrError {}

// Type alias for addressing constants, etc.
// Used for things that live outside the stack

#[derive(Debug, Clone)]
pub enum FerryOpcode {
    // NOP: no operation
    Nop,
    // HALT: terminate application
    Halt,
    Return,
    // LOAD: loads designated value (push onto stack)
    Load(i64),
    // ADD: pops last 2 values, adds, pushes onto stack
    Add,
    Sub,
    Mul,
    Div,
}

// Into over From due to not being able to effeciently map u8 to fixed enum values
#[expect(clippy::from_over_into)]
impl Into<u8> for FerryOpcode {
    fn into(self) -> u8 {
        match self {
            FerryOpcode::Nop => 0x00,
            FerryOpcode::Load(_) => 0x01,
            FerryOpcode::Add => 0x02,
            FerryOpcode::Sub => 0x03,
            FerryOpcode::Mul => 0x04,
            FerryOpcode::Div => 0x05,
            FerryOpcode::Return => 0xfe,
            FerryOpcode::Halt => 0xff,
        }
    }
}

#[derive(Debug)]
pub struct FerryIr {
    // AST to be lowered to this IR
    ast: Vec<Expr>,
    constants: Vec<FerryValue>,
}

type FerryResult<T> = Result<T, FerryIrError>;

impl FerryIr {
    pub fn new(ast: Vec<Expr>) -> Self {
        Self {
            ast,
            constants: vec![],
        }
    }

    pub fn lower(&mut self) -> FerryResult<Vec<FerryOpcode>> {
        let mut program = vec![];
        let mut state = vec![];

        for expr in self.ast.clone().iter_mut() {
            match self.assemble_opcode(expr, &mut state) {
                Ok(mut instructions) => program.append(&mut instructions),
                Err(e) => println!("{e}"),
            }
        }

        program.push(FerryOpcode::Halt);
        Ok(program)
    }

    fn assemble_opcode(
        &mut self,
        expr: &mut Expr,
        state: &mut Vec<FerryValue>,
    ) -> FerryResult<Vec<FerryOpcode>> {
        walk_expr(&mut *self, expr, state)
    }
}

impl ExprVisitor<FerryResult<Vec<FerryOpcode>>, &mut Vec<FerryValue>> for &mut FerryIr {
    fn visit_literal(
        &mut self,
        literal: &mut Lit,
        state: &mut Vec<FerryValue>,
    ) -> FerryResult<Vec<FerryOpcode>> {
        match literal {
            // treat undefined as a 0 for now
            Lit::Undefined { token, expr_type } => Ok(vec![FerryOpcode::Load(0)]),
            Lit::Number {
                token,
                value,
                expr_type,
                span,
            } => Ok(vec![FerryOpcode::Load(*value)]),
            Lit::Str {
                token,
                value,
                expr_type,
                span,
            } => todo!(),
            Lit::Bool {
                token,
                value,
                expr_type,
                span,
            } => Ok(vec![FerryOpcode::Load(*value as i64)]),
            Lit::List {
                token,
                contents,
                expr_type,
                span,
            } => todo!(),
        }
    }

    fn visit_binary(
        &mut self,
        binary: &mut Binary,
        state: &mut Vec<FerryValue>,
    ) -> FerryResult<Vec<FerryOpcode>> {
        match binary.operator.get_token_type() {
            crate::token::TokenType::Operator(op) => match op {
                crate::token::Op::Add => {
                    let mut instructions = vec![];

                    let mut left = self.assemble_opcode(&mut binary.lhs, state)?;
                    let mut right = self.assemble_opcode(&mut binary.rhs, state)?;

                    instructions.append(&mut left);
                    instructions.append(&mut right);
                    instructions.append(&mut vec![FerryOpcode::Add]);

                    Ok(instructions)
                }
                crate::token::Op::Subtract => {
                    let mut instructions = vec![];

                    let mut left = self.assemble_opcode(&mut binary.lhs, state)?;
                    let mut right = self.assemble_opcode(&mut binary.rhs, state)?;

                    instructions.append(&mut left);
                    instructions.append(&mut right);
                    instructions.append(&mut vec![FerryOpcode::Sub]);

                    Ok(instructions)
                }
                crate::token::Op::Multiply => {
                    let mut instructions = vec![];

                    let mut left = self.assemble_opcode(&mut binary.lhs, state)?;
                    let mut right = self.assemble_opcode(&mut binary.rhs, state)?;

                    instructions.append(&mut left);
                    instructions.append(&mut right);
                    instructions.append(&mut vec![FerryOpcode::Mul]);

                    Ok(instructions)
                }
                crate::token::Op::Divide => {
                    let mut instructions = vec![];

                    let mut left = self.assemble_opcode(&mut binary.lhs, state)?;
                    let mut right = self.assemble_opcode(&mut binary.rhs, state)?;

                    instructions.append(&mut left);
                    instructions.append(&mut right);
                    instructions.append(&mut vec![FerryOpcode::Div]);

                    Ok(instructions)
                }
                crate::token::Op::Equals => todo!(),
                crate::token::Op::LessThan => todo!(),
                crate::token::Op::GreaterThan => todo!(),
                crate::token::Op::Equality => todo!(),
                crate::token::Op::LessEqual => todo!(),
                crate::token::Op::GreaterEqual => todo!(),
                crate::token::Op::GetI => todo!(),
                crate::token::Op::Cons => todo!(),
            },
            _ => Ok(vec![FerryOpcode::Nop]),
        }
    }

    fn visit_unary(
        &mut self,
        unary: &mut Unary,
        state: &mut Vec<FerryValue>,
    ) -> FerryResult<Vec<FerryOpcode>> {
        todo!()
    }

    fn visit_variable(
        &mut self,
        variable: &mut Variable,
        state: &mut Vec<FerryValue>,
    ) -> FerryResult<Vec<FerryOpcode>> {
        todo!()
    }

    fn visit_assign(
        &mut self,
        assign: &mut Assign,
        state: &mut Vec<FerryValue>,
    ) -> FerryResult<Vec<FerryOpcode>> {
        todo!()
    }

    fn visit_if_expr(
        &mut self,
        if_expr: &mut If,
        state: &mut Vec<FerryValue>,
    ) -> FerryResult<Vec<FerryOpcode>> {
        todo!()
    }

    fn visit_group(
        &mut self,
        group: &mut Group,
        state: &mut Vec<FerryValue>,
    ) -> FerryResult<Vec<FerryOpcode>> {
        todo!()
    }

    fn visit_binding(
        &mut self,
        binding: &mut Binding,
        state: &mut Vec<FerryValue>,
    ) -> FerryResult<Vec<FerryOpcode>> {
        todo!()
    }

    fn visit_loop(
        &mut self,
        loop_expr: &mut Loop,
        state: &mut Vec<FerryValue>,
    ) -> FerryResult<Vec<FerryOpcode>> {
        todo!()
    }

    fn visit_for(
        &mut self,
        for_expr: &mut For,
        state: &mut Vec<FerryValue>,
    ) -> FerryResult<Vec<FerryOpcode>> {
        todo!()
    }

    fn visit_function(
        &mut self,
        function: &mut Function,
        state: &mut Vec<FerryValue>,
    ) -> FerryResult<Vec<FerryOpcode>> {
        todo!()
    }

    fn visit_call(
        &mut self,
        call: &mut Call,
        state: &mut Vec<FerryValue>,
    ) -> FerryResult<Vec<FerryOpcode>> {
        todo!()
    }
}

mod tests {
    use super::*;
    use std::mem::size_of;
}
