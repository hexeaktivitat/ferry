use miette::{Diagnostic, Result, SourceSpan};
use thiserror::Error;

use crate::{
    state::FerryState,
    syntax::{walk_expr, Binary, Expr, ExprVisitor, Literal as SLit, Variable},
};

#[derive(Error, Diagnostic, Debug)]
pub enum FerryAsmError {}

type FerryResult<T> = Result<T, FerryAsmError>;
type FerryAsmResult<T> = Result<Vec<T>, Vec<FerryAsmError>>;

pub struct FerryRiscVAssembler {}

impl FerryRiscVAssembler {
    pub fn new() -> Self {
        Self {}
    }

    pub fn assemble(
        &mut self,
        source: Vec<Expr>,
        state: &mut FerryState,
    ) -> FerryAsmResult<Instruction> {
        // let mut results = Vec::new();
        let mut operations = Vec::new();

        for code in source.clone().iter_mut() {
            match self.generate_asm(code, &mut operations) {
                Ok(a) => operations.push(a),

                Err(e) => println!("o no"),
            }
        }

        Ok(operations)
    }

    fn generate_asm(
        &mut self,
        code: &mut Expr,
        state: &mut Vec<Instruction>,
    ) -> FerryResult<Instruction> {
        walk_expr(&mut *self, code, state)
    }
}

impl ExprVisitor<FerryResult<Instruction>, &mut Vec<Instruction>> for &mut FerryRiscVAssembler {
    fn visit_literal(
        &mut self,
        literal: &mut SLit,
        state: &mut Vec<Instruction>,
    ) -> FerryResult<Instruction> {
        match literal {
            SLit::Number { value, expr_type } => Ok(Instruction::Li {
                d: Register::A0,
                imm: value.clone() as i32,
            }),
        }
    }

    fn visit_binary(
        &mut self,
        binary: &mut Binary,
        state: &mut Vec<Instruction>,
    ) -> FerryResult<Instruction> {
        let left = self.generate_asm(&mut binary.lhs, state)?;
        state.push(left);
        state.push(Instruction::Sw {
            s1: Register::FP,
            s2: Register::A0,
            imm: 0,
        });
        let right = self.generate_asm(&mut binary.rhs, state)?;
        state.push(right);
        state.push(Instruction::Lw {
            d: Register::T0,
            s: Register::FP,
            imm: 0,
        });

        let result_instr = match binary.operator.get_type() {
            crate::token::TokenType::Operator(o) => match o {
                crate::token::Op::Add => Instruction::Add {
                    d: Register::A0,
                    s1: Register::T0,
                    s2: Register::A0,
                },
                crate::token::Op::Subtract => Instruction::Sub {
                    d: Register::A0,
                    s1: Register::T0,
                    s2: Register::A0,
                },
                crate::token::Op::Multiply => Instruction::Mul {
                    d: Register::A0,
                    s1: Register::T0,
                    s2: Register::A0,
                },
                // do NOT divide by zero!!!!!!!!!!!! you WILL regret this!!!!!!
                // division handling needs different care and more robust instructions
                crate::token::Op::Divide => Instruction::Div {
                    d: Register::A0,
                    s1: Register::T0,
                    s2: Register::A0,
                },
                crate::token::Op::RightArrow => todo!(),
                crate::token::Op::Equals => todo!(),
            },
            _ => unreachable!(),
        };
        Ok(result_instr)
    }

    fn visit_variable(
        &mut self,
        variable: &mut Variable,
        state: &mut Vec<Instruction>,
    ) -> FerryResult<Instruction> {
        Ok(Instruction::Lazy)
    }

    fn visit_assign(
        &mut self,
        assign: &mut crate::syntax::Assign,
        state: &mut Vec<Instruction>,
    ) -> FerryResult<Instruction> {
        Ok(Instruction::Lazy)
    }
}

/// `Instruction`
///
/// General nomenclature
/// d - Destination register for the opcode
/// s - Saved / Source register
/// imm - immediate value
#[derive(Clone, Debug)]
pub enum Instruction {
    Add {
        d: Register,
        s1: Register,
        s2: Register,
    },
    Addi {
        d: Register,
        s: Register,
        imm: i16,
    },
    Sw {
        s1: Register,
        s2: Register,
        imm: i16, // bit offset for operation
    },
    Lw {
        d: Register,
        s: Register,
        imm: i16,
    },
    Sub {
        d: Register,
        s1: Register,
        s2: Register,
    },
    Mul {
        d: Register,
        s1: Register,
        s2: Register,
    },
    Div {
        d: Register,
        s1: Register,
        s2: Register,
    },
    // Pseudo-instructions
    Li {
        d: Register,
        imm: i32, // 32-bit RISC set
    },
    // Dummy instruction for lazy
    Lazy,
}

#[derive(Clone, Debug)]
pub enum Register {
    R0, // Zero / x0
    T0, // Temporary / x5
    FP, // Frame pointer / x8
    A0, // Function arg / x10
}

impl std::fmt::Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Instruction::Add { d, s1, s2 } => write!(f, "add {}, {}, {}", d, s1, s2),
            Instruction::Addi { d, s, imm } => write!(f, "addi {}, {}, {}", d, s, imm),
            Instruction::Sw { s1, s2, imm } => write!(f, "sw {}, {}({})", s2, imm, s1),
            Instruction::Lw { d, s, imm } => write!(f, "lw {}, {}({})", d, imm, s),
            Instruction::Sub { d, s1, s2 } => write!(f, "sub {}, {}, {}", d, s1, s2),
            Instruction::Mul { d, s1, s2 } => write!(f, "mul {}, {}, {}", d, s1, s2),
            Instruction::Div { d, s1, s2 } => write!(f, "div {}, {}, {}", d, s1, s2),
            Instruction::Li { d, imm } => write!(f, "li {}, {}", d, imm),
            Instruction::Lazy => write!(f, "too lazy for this atm"),
        }
    }
}

impl std::fmt::Display for Register {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Register::R0 => f.write_str("x0"),
            Register::T0 => f.write_str("x5"),
            Register::FP => f.write_str("x8"),
            Register::A0 => f.write_str("x10"),
        }
    }
}
