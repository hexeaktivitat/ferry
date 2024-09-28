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

// Register values are stored on the stack
type FerryRegister = u8;
// Address values will be stored on the heap
type FerryAddress = u16;
// Literal values (derived from FerryValue)
type FerryLiteral = i16;

#[derive(Debug, Clone, Copy)]
pub enum FerryOpcode {
    /// NOP: no operation
    Nop,
    /// HALT: terminate application
    Halt,
    /// LOAD: Load value from addressed memory location
    Load {
        dest: FerryRegister,
        a: FerryAddress,
    },
    /// LOAD IMMEDIATE: Load a static, literally encoded value
    LoadI {
        dest: FerryRegister,
        v: FerryLiteral,
    },
    /// ADD: Add 2 registers together and store the result in a register
    Add {
        dest: FerryRegister,
        r1: FerryRegister,
        r2: FerryRegister,
    },
    Sub {
        dest: FerryRegister,
        r1: FerryRegister,
        r2: FerryRegister,
    },
}

// Into over From due to not being able to effeciently map u8 to fixed enum values
#[expect(clippy::from_over_into)]
impl Into<u8> for FerryOpcode {
    fn into(self) -> u8 {
        match self {
            FerryOpcode::Nop => 0x00,
            FerryOpcode::LoadI { .. } => 0x01,
            FerryOpcode::Load { .. } => 0x02,
            FerryOpcode::Add { .. } => 0x03,
            FerryOpcode::Sub { .. } => 0x04,
            FerryOpcode::Halt => 0xff,
        }
    }
}

#[derive(Debug)]
pub struct FerryIr {
    // AST to be lowered to this IR
    ast: Vec<Expr>,
    // Symbol table
    symbols: FerryState,
}

type FerryResult<T> = Result<T, FerryIrError>;

impl FerryIr {
    pub fn new(ast: Vec<Expr>, symbols: FerryState) -> Self {
        Self { ast, symbols }
    }

    pub fn lower(&mut self) -> FerryResult<Vec<FerryOpcode>> {
        let result = vec![];

        Ok(result)
    }
}

mod tests {
    use super::*;
    use std::mem::size_of;

    /// an instruction should be 32 bits
    #[test]
    fn test_instruction_size() {
        assert!(size_of::<FerryOpcode>() == 4);
    }
}
