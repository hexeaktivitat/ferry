use miette::{Diagnostic, Result, SourceSpan};
use thiserror::Error;

use crate::{
    state::FerryState,
    syntax::{
        walk_expr, Binary, Binding, Expr, ExprVisitor, Function, Group, If, Lit, Loop, Unary,
        Variable,
    },
};

#[derive(Error, Diagnostic, Debug)]
pub enum FerryAsmError {
    #[error("Unimplemented feature")]
    Unimplemented {
        #[help]
        advice: String,
        #[label]
        span: SourceSpan,
    },
}

type FerryResult<T> = Result<T, FerryAsmError>;
type FerryAsmResult<T> = Result<Vec<T>, Vec<FerryAsmError>>;

pub struct FerryRiscVAssembler {
    offset: i16,
}

impl FerryRiscVAssembler {
    pub fn new() -> Self {
        Self { offset: 0 }
    }

    pub fn assemble(
        &mut self,
        source: Vec<Expr>,
        _state: &mut FerryState,
    ) -> FerryAsmResult<Instruction> {
        let mut errors = Vec::new();
        let mut operations = Vec::new();

        for code in source.clone().iter_mut() {
            match self.generate_asm(code, &mut operations) {
                Ok(a) => operations.push(a),

                Err(e) => errors.push(e),
            }
        }

        if errors.is_empty() {
            Ok(operations)
        } else {
            Err(errors)
        }
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
        literal: &mut Lit,
        _state: &mut Vec<Instruction>,
    ) -> FerryResult<Instruction> {
        match literal {
            Lit::Number {
                value,
                expr_type: _,
                span: _,
                token: _,
            } => Ok(Instruction::Li {
                d: Register::A0,
                imm: *value as i32,
            }),
            // not a functional instruction!
            Lit::Str {
                value: _,
                expr_type: _,
                span: _,
                token: _,
            } => Ok(Instruction::Li {
                d: Register::A0,
                imm: 0,
            }),
            Lit::Bool {
                value,
                expr_type: _,
                span: _,
                token: _,
            } => {
                if *value {
                    Ok(Instruction::Li {
                        d: Register::A0,
                        imm: 1,
                    })
                } else {
                    Ok(Instruction::Li {
                        d: Register::A0,
                        imm: 0,
                    })
                }
            }
            Lit::Undefined {
                expr_type: _,
                token,
            } => Err(FerryAsmError::Unimplemented {
                advice: "unimplemented feature".into(),
                span: *token.get_span(),
            }),

            Lit::List {
                token,
                contents,
                expr_type,
                span,
            } => Err(FerryAsmError::Unimplemented {
                advice: "unimplemented feature".into(),
                span: *token.get_span(),
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
            imm: self.offset,
        });
        let right = self.generate_asm(&mut binary.rhs, state)?;
        state.push(right);
        state.push(Instruction::Lw {
            d: Register::T0,
            s: Register::FP,
            imm: self.offset,
        });

        let result_instr = match binary.operator.get_token_type() {
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
                // crate::token::Op::RightArrow => todo!(),
                crate::token::Op::Equals => todo!(),
                crate::token::Op::LessThan => todo!(),
                crate::token::Op::GreaterThan => todo!(),
                crate::token::Op::Equality => todo!(),
                crate::token::Op::LessEqual => todo!(),
                crate::token::Op::GreaterEqual => todo!(),
                crate::token::Op::GetI => todo!(),
                crate::token::Op::Cons => todo!(),
            },
            _ => unreachable!(),
        };
        Ok(result_instr)
    }

    fn visit_variable(
        &mut self,
        variable: &mut Variable,
        _state: &mut Vec<Instruction>,
    ) -> FerryResult<Instruction> {
        let globl = &variable.name;
        Ok(Instruction::LwGlobl {
            d: Register::A0,
            s: Register::Globl(globl.clone()),
        })
    }

    fn visit_assign(
        &mut self,
        assign: &mut crate::syntax::Assign,
        state: &mut Vec<Instruction>,
    ) -> FerryResult<Instruction> {
        let globl = assign.name.clone();
        let var = self.generate_asm(&mut assign.var, state)?;
        state.push(var);
        state.push(Instruction::Sw {
            s1: Register::FP,
            s2: Register::A0,
            imm: self.offset,
        });
        if let Some(v) = &mut assign.value {
            let value = self.generate_asm(v, state)?;
            state.push(value);
        }
        Ok(Instruction::SwGlobl {
            s1: Register::Globl(globl.clone()),
            s2: Register::T0,
            d: Register::A0,
        })
    }

    fn visit_if_expr(
        &mut self,
        if_expr: &mut If,
        _state: &mut Vec<Instruction>,
    ) -> FerryResult<Instruction> {
        Err(FerryAsmError::Unimplemented {
            advice: "unimplemented feature".into(),
            span: *if_expr.token.get_span(),
        })
    }

    fn visit_group(
        &mut self,
        group: &mut Group,
        _state: &mut Vec<Instruction>,
    ) -> FerryResult<Instruction> {
        Err(FerryAsmError::Unimplemented {
            advice: "unimplemented feature".into(),
            span: *group.token.get_span(),
        })
    }

    fn visit_binding(
        &mut self,
        binding: &mut Binding,
        _state: &mut Vec<Instruction>,
    ) -> FerryResult<Instruction> {
        Err(FerryAsmError::Unimplemented {
            advice: "unimplemented feature".into(),
            span: *binding.token.get_span(),
        })
    }

    fn visit_loop(
        &mut self,
        loop_expr: &mut Loop,
        _state: &mut Vec<Instruction>,
    ) -> FerryResult<Instruction> {
        Err(FerryAsmError::Unimplemented {
            advice: "unimplemented feature".into(),
            span: *loop_expr.token.get_span(),
        })
    }

    fn visit_unary(
        &mut self,
        unary: &mut Unary,
        state: &mut Vec<Instruction>,
    ) -> FerryResult<Instruction> {
        Err(FerryAsmError::Unimplemented {
            advice: "unimplemented feature".into(),
            span: *unary.operator.get_span(),
        })
    }

    fn visit_for(
        &mut self,
        for_expr: &mut crate::syntax::For,
        state: &mut Vec<Instruction>,
    ) -> FerryResult<Instruction> {
        Err(FerryAsmError::Unimplemented {
            advice: "unimplemented feature".into(),
            span: *for_expr.token.get_span(),
        })
    }

    fn visit_function(
        &mut self,
        function: &mut Function,
        state: &mut Vec<Instruction>,
    ) -> FerryResult<Instruction> {
        Err(FerryAsmError::Unimplemented {
            advice: "unimplemented feature".into(),
            span: *function.token.get_span(),
        })
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
    SwGlobl {
        s1: Register,
        s2: Register,
        d: Register,
    },
    Lw {
        d: Register,
        s: Register,
        imm: i16,
    },
    LwGlobl {
        d: Register,
        s: Register,
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
    Nop,
    // Dummy instruction for lazy
    Lazy,
}

#[derive(Clone, Debug)]
pub enum Register {
    Globl(String), // $-labeled global vals
    R0,            // Zero / x0
    T0,            // Temporary / x5
    FP,            // Frame pointer / x8
    A0,            // Function arg / x10
}

impl std::fmt::Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Instruction::Add { d, s1, s2 } => write!(f, "add {d}, {s1}, {s2}"),
            Instruction::Addi { d, s, imm } => write!(f, "addi {d}, {s}, {imm}"),
            Instruction::Sw { s1, s2, imm } => write!(f, "sw {s2}, {imm}({s1})"),
            Instruction::Lw { d, s, imm } => write!(f, "lw {d}, {imm}({s})"),
            Instruction::Sub { d, s1, s2 } => write!(f, "sub {d}, {s1}, {s2}"),
            Instruction::Mul { d, s1, s2 } => write!(f, "mul {d}, {s1}, {s2}"),
            Instruction::Div { d, s1, s2 } => write!(f, "div {d}, {s1}, {s2}"),
            Instruction::Li { d, imm } => write!(f, "li {d}, {imm}"),
            Instruction::Lazy => write!(f, "too lazy for this atm"),
            Instruction::SwGlobl { s1, s2, d } => write!(f, "sw {d}, {s1}, {s2}"),
            Instruction::LwGlobl { d, s } => write!(f, "lw {d} {s}"),
            Instruction::Nop => todo!(),
        }
    }
}

impl std::fmt::Display for Register {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Register::R0 => f.write_str("r0"),
            Register::T0 => f.write_str("t0"),
            Register::FP => f.write_str("fp"),
            Register::A0 => f.write_str("a0"),
            Register::Globl(g) => write!(f, "${g}"),
        }
    }
}
