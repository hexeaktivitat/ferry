use std::collections::HashMap;

use miette::{Diagnostic, Result, SourceSpan};
use thiserror::Error;

use crate::{
    interpreter::FerryInterpreter,
    state::{Convertable, FerryState, FerryValue},
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
pub type FerryAddr = u8;

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
    Alloc(FerryAddr, FerryValue),
}

// Into over From due to not being able to effeciently map u8 to fixed enum values
#[expect(clippy::from_over_into)]
impl Into<u8> for FerryOpcode {
    fn into(self) -> u8 {
        match self {
            FerryOpcode::Nop => 0x00,
            FerryOpcode::Load(_) => 0x01,
            FerryOpcode::Alloc(_, _) => 0x02,
            FerryOpcode::Add => 0x10,
            FerryOpcode::Sub => 0x11,
            FerryOpcode::Mul => 0x12,
            FerryOpcode::Div => 0x13,
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
    heap_ptr: FerryAddr,
}

type FerryResult<T> = Result<T, FerryIrError>;

impl FerryIr {
    pub fn new(ast: Vec<Expr>) -> Self {
        Self {
            ast,
            constants: vec![],
            heap_ptr: 0x00,
        }
    }

    pub fn lower(&mut self, state: &mut FerryState) -> FerryResult<Vec<FerryOpcode>> {
        let mut program = vec![];

        for expr in self.ast.clone().iter_mut() {
            match self.assemble_opcode(expr, state) {
                Ok(mut instructions) => program.append(&mut instructions),
                Err(e) => println!("{e}"),
            }
        }

        program.push(FerryOpcode::Return);

        Ok(program)
    }

    fn assemble_opcode(
        &mut self,
        expr: &mut Expr,
        state: &mut FerryState,
    ) -> FerryResult<Vec<FerryOpcode>> {
        walk_expr(&mut *self, expr, state)
    }
}

impl ExprVisitor<FerryResult<Vec<FerryOpcode>>, &mut FerryState> for &mut FerryIr {
    fn visit_literal(
        &mut self,
        literal: &mut Lit,
        state: &mut FerryState,
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
            } => {
                let ptr = self.heap_ptr;
                self.heap_ptr += 1;
                Ok(vec![
                    FerryOpcode::Alloc(ptr, FerryValue::Str(value.clone())),
                    FerryOpcode::Load(ptr as i64),
                ])
            }
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
            } => {
                let ptr = self.heap_ptr;
                self.heap_ptr += 1;
                let mut list_interpreter =
                    FerryInterpreter::new(vec![Expr::Literal(literal.clone())]);
                let values = list_interpreter.interpret(state).unwrap().unwrap();
                Ok(vec![
                    FerryOpcode::Alloc(ptr, values),
                    FerryOpcode::Load(ptr as i64),
                ])
            }
        }
    }

    fn visit_binary(
        &mut self,
        binary: &mut Binary,
        state: &mut FerryState,
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
        state: &mut FerryState,
    ) -> FerryResult<Vec<FerryOpcode>> {
        todo!()
    }

    fn visit_variable(
        &mut self,
        variable: &mut Variable,
        state: &mut FerryState,
    ) -> FerryResult<Vec<FerryOpcode>> {
        todo!()
    }

    fn visit_assign(
        &mut self,
        assign: &mut Assign,
        state: &mut FerryState,
    ) -> FerryResult<Vec<FerryOpcode>> {
        todo!()
    }

    fn visit_if_expr(
        &mut self,
        if_expr: &mut If,
        state: &mut FerryState,
    ) -> FerryResult<Vec<FerryOpcode>> {
        todo!()
    }

    fn visit_group(
        &mut self,
        group: &mut Group,
        state: &mut FerryState,
    ) -> FerryResult<Vec<FerryOpcode>> {
        todo!()
    }

    fn visit_binding(
        &mut self,
        binding: &mut Binding,
        state: &mut FerryState,
    ) -> FerryResult<Vec<FerryOpcode>> {
        todo!()
    }

    fn visit_loop(
        &mut self,
        loop_expr: &mut Loop,
        state: &mut FerryState,
    ) -> FerryResult<Vec<FerryOpcode>> {
        todo!()
    }

    fn visit_for(
        &mut self,
        for_expr: &mut For,
        state: &mut FerryState,
    ) -> FerryResult<Vec<FerryOpcode>> {
        todo!()
    }

    fn visit_function(
        &mut self,
        function: &mut Function,
        state: &mut FerryState,
    ) -> FerryResult<Vec<FerryOpcode>> {
        todo!()
    }

    fn visit_call(
        &mut self,
        call: &mut Call,
        state: &mut FerryState,
    ) -> FerryResult<Vec<FerryOpcode>> {
        todo!()
    }
}
