use std::collections::HashMap;

use miette::{Diagnostic, Result, SourceSpan};
use thiserror::Error;

use crate::{
    state::FerryState,
    syntax::{Expr, ExprVisitor},
};

/// Intermediate Representation for FerryVM
/// Part of compilation process, intended to be a high-level assembly language
/// similar to LLVM-IR, cranelift-IR, etc.

#[derive(Error, Diagnostic, Debug)]
pub enum FerryIrError {}

// Register addresses are 8-bit for the VM
type FerryRegister = u8;
type Integer = i16;

pub enum FerryOpCode {
    LoadLit {
        dest: FerryRegister,
        value: Integer,
    },
    Add {
        dest: FerryRegister,
        a: FerryRegister,
        b: FerryRegister,
    },
}

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
        let registers: HashMap<String, FerryRegister> = HashMap::new();
        let result = vec![];

        Ok(result)
    }
}

impl ExprVisitor<FerryResult<FerryOpCode>, &mut FerryState> for FerryIr {
    fn visit_literal(
        &mut self,
        literal: &mut crate::syntax::Lit,
        state: &mut FerryState,
    ) -> FerryResult<FerryOpCode> {
        todo!()
    }

    fn visit_binary(
        &mut self,
        binary: &mut crate::syntax::Binary,
        state: &mut FerryState,
    ) -> FerryResult<FerryOpCode> {
        todo!()
    }

    fn visit_unary(
        &mut self,
        unary: &mut crate::syntax::Unary,
        state: &mut FerryState,
    ) -> FerryResult<FerryOpCode> {
        todo!()
    }

    fn visit_variable(
        &mut self,
        variable: &mut crate::syntax::Variable,
        state: &mut FerryState,
    ) -> FerryResult<FerryOpCode> {
        todo!()
    }

    fn visit_assign(
        &mut self,
        assign: &mut crate::syntax::Assign,
        state: &mut FerryState,
    ) -> FerryResult<FerryOpCode> {
        todo!()
    }

    fn visit_if_expr(
        &mut self,
        if_expr: &mut crate::syntax::If,
        state: &mut FerryState,
    ) -> FerryResult<FerryOpCode> {
        todo!()
    }

    fn visit_group(
        &mut self,
        group: &mut crate::syntax::Group,
        state: &mut FerryState,
    ) -> FerryResult<FerryOpCode> {
        todo!()
    }

    fn visit_binding(
        &mut self,
        binding: &mut crate::syntax::Binding,
        state: &mut FerryState,
    ) -> FerryResult<FerryOpCode> {
        todo!()
    }

    fn visit_loop(
        &mut self,
        loop_expr: &mut crate::syntax::Loop,
        state: &mut FerryState,
    ) -> FerryResult<FerryOpCode> {
        todo!()
    }

    fn visit_for(
        &mut self,
        for_expr: &mut crate::syntax::For,
        state: &mut FerryState,
    ) -> FerryResult<FerryOpCode> {
        todo!()
    }

    fn visit_function(
        &mut self,
        function: &mut crate::syntax::Function,
        state: &mut FerryState,
    ) -> FerryResult<FerryOpCode> {
        todo!()
    }

    fn visit_call(
        &mut self,
        call: &mut crate::syntax::Call,
        state: &mut FerryState,
    ) -> FerryResult<FerryOpCode> {
        todo!()
    }
}

mod tests {
    use super::*;
    use std::mem::size_of;

    /// Checks that FerryOpCode is a 32-bit value
    /// This is not necessarily a requirement, but important to know
    #[test]
    fn check_opcode_size() {
        assert!(size_of::<FerryOpCode>() == 4);
    }
}
