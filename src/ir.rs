use std::collections::HashMap;

use miette::{Diagnostic, Result, SourceSpan};
use thiserror::Error;

use crate::{
    state::{FerryState, FerryValue},
    syntax::{
        Assign, Binary, Binding, Call, Expr, ExprVisitor, For, Function, Group, If, Lit, Loop,
        Unary, Variable,
    },
};

/// Intermediate Representation for FerryVM
/// Part of compilation process, intended to be a high-level assembly language
/// similar to LLVM-IR, cranelift-IR, etc.

#[derive(Error, Diagnostic, Debug)]
pub enum FerryIrError {}

// Register addresses are 8-bit for the VM
// type FerryRegister = u8;
// type Integer = i16;

#[derive(Debug, Clone)]
pub enum FerryOpCode {
    Load(FerryValue),
    Add,
}

#[derive(Debug)]
pub struct FerryIr {
    // AST to be lowered to this IR
    ast: Vec<Expr>,
    // Symbol table
    symbols: FerryState,
    // Register table
}

type FerryResult<T> = Result<T, FerryIrError>;

impl FerryIr {
    pub fn new(ast: Vec<Expr>, symbols: FerryState) -> Self {
        Self { ast, symbols }
    }

    pub fn lower(&mut self) -> FerryResult<Vec<FerryOpCode>> {
        let result = vec![];

        Ok(result)
    }
}

impl ExprVisitor<FerryResult<FerryOpCode>, &mut FerryState> for FerryIr {
    fn visit_literal(
        &mut self,
        literal: &mut Lit,
        state: &mut FerryState,
    ) -> FerryResult<FerryOpCode> {
        match literal {
            Lit::Undefined {
                token: _,
                expr_type: _,
            } => Ok(FerryOpCode::Load(FerryValue::Unit)),
            Lit::Number {
                token: _,
                value,
                expr_type: _,
                span: _,
            } => Ok(FerryOpCode::Load(FerryValue::Number(*value))),
            Lit::Str {
                token: _,
                value,
                expr_type: _,
                span: _,
                // } => Ok(FerryOpCode::Load(FerryValue::Str(value.clone()))),
            } => todo!(),
            Lit::Bool {
                token: _,
                value,
                expr_type: _,
                span: _,
            } => Ok(FerryOpCode::Load(FerryValue::Boolean(*value))),
            Lit::List {
                token: _,
                contents,
                expr_type: _,
                span: _,
                // } => Ok(FerryOpCode::Load(FerryValue::List(contents.clone())))
            } => todo!(),
        }
    }

    fn visit_binary(
        &mut self,
        binary: &mut Binary,
        state: &mut FerryState,
    ) -> FerryResult<FerryOpCode> {
        todo!()
    }

    fn visit_unary(
        &mut self,
        unary: &mut Unary,
        state: &mut FerryState,
    ) -> FerryResult<FerryOpCode> {
        todo!()
    }

    fn visit_variable(
        &mut self,
        variable: &mut Variable,
        state: &mut FerryState,
    ) -> FerryResult<FerryOpCode> {
        todo!()
    }

    fn visit_assign(
        &mut self,
        assign: &mut Assign,
        state: &mut FerryState,
    ) -> FerryResult<FerryOpCode> {
        todo!()
    }

    fn visit_if_expr(
        &mut self,
        if_expr: &mut If,
        state: &mut FerryState,
    ) -> FerryResult<FerryOpCode> {
        todo!()
    }

    fn visit_group(
        &mut self,
        group: &mut Group,
        state: &mut FerryState,
    ) -> FerryResult<FerryOpCode> {
        todo!()
    }

    fn visit_binding(
        &mut self,
        binding: &mut Binding,
        state: &mut FerryState,
    ) -> FerryResult<FerryOpCode> {
        todo!()
    }

    fn visit_loop(
        &mut self,
        loop_expr: &mut Loop,
        state: &mut FerryState,
    ) -> FerryResult<FerryOpCode> {
        todo!()
    }

    fn visit_for(
        &mut self,
        for_expr: &mut For,
        state: &mut FerryState,
    ) -> FerryResult<FerryOpCode> {
        todo!()
    }

    fn visit_function(
        &mut self,
        function: &mut Function,
        state: &mut FerryState,
    ) -> FerryResult<FerryOpCode> {
        todo!()
    }

    fn visit_call(&mut self, call: &mut Call, state: &mut FerryState) -> FerryResult<FerryOpCode> {
        todo!()
    }
}

mod tests {
    // use super::*;
    // use std::mem::size_of;

    // / Checks that FerryOpCode is a 32-bit value
    // / This is not necessarily a requirement, but important to know
    // #[test]
    // fn check_opcode_size() {
    //     assert!(size_of::<FerryOpCode>() == 1);
    // }
}
