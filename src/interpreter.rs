use miette::{Diagnostic, Result, SourceSpan};
use thiserror::Error;

use crate::{
    state::{FerryState, FerryValue},
    syntax::{walk_expr, Binary, Expr, ExprVisitor, Lit as SLit, Variable},
    token::{Op, TokenType as TT},
};

#[derive(Error, Diagnostic, Debug)]
pub enum FerryInterpreterError {
    #[error("Help?")]
    Temp {
        #[help]
        help: String,
        #[label]
        span: SourceSpan,
    },
    #[error("Invalid operation")]
    InvalidOperation {
        #[help]
        help: String,
        #[label]
        span: SourceSpan,
    },
}

type FerryResult<T> = Result<Option<T>, FerryInterpreterError>;

pub struct FerryInterpreter {
    syntax: Vec<Expr>,
}

impl FerryInterpreter {
    pub fn new(syntax: Vec<Expr>) -> Self {
        Self { syntax }
    }

    pub fn interpret(&mut self, state: &mut FerryState) -> FerryResult<FerryValue> {
        let mut ret = None;
        for mut code in self.syntax.clone().iter_mut() {
            ret = self.evaluate(&mut code, state)?;
        }

        Ok(ret)
    }

    fn evaluate(&mut self, code: &mut Expr, state: &mut FerryState) -> FerryResult<FerryValue> {
        walk_expr(&mut *self, code, state)
    }
}

impl ExprVisitor<FerryResult<FerryValue>, &mut FerryState> for &mut FerryInterpreter {
    fn visit_literal(
        &mut self,
        literal: &mut SLit,
        _state: &mut FerryState,
    ) -> FerryResult<FerryValue> {
        match literal {
            SLit::Number {
                value,
                expr_type: _,
                span: _,
            } => Ok(Some(FerryValue::Number(*value))),
            SLit::Str {
                value,
                expr_type: _,
                span: _,
            } => Ok(Some(FerryValue::Str(value.clone()))),
            SLit::Bool {
                value,
                expr_type: _,
                span: _,
            } => Ok(Some(FerryValue::Boolean(*value))),
            SLit::Undefined { expr_type: _ } => Ok(Some(FerryValue::Unit)),
        }
    }

    fn visit_binary(
        &mut self,
        binary: &mut Binary,
        state: &mut FerryState,
    ) -> FerryResult<FerryValue> {
        let left = self.evaluate(&mut binary.lhs, state)?;
        let right = self.evaluate(&mut binary.rhs, state)?;

        let op = &binary.operator;

        match &op.get_token_type() {
            TT::Operator(o) => match o {
                Op::Add => match (left, right) {
                    (Some(FerryValue::Number(l)), Some(FerryValue::Number(r))) => {
                        Ok(Some(FerryValue::Number(l + r)))
                    }
                    _ => unimplemented!(),
                },
                Op::Subtract => match (left, right) {
                    (Some(FerryValue::Number(l)), Some(FerryValue::Number(r))) => {
                        Ok(Some(FerryValue::Number(l - r)))
                    }
                    _ => unimplemented!(),
                },

                Op::Multiply => match (left, right) {
                    (Some(FerryValue::Number(l)), Some(FerryValue::Number(r))) => {
                        Ok(Some(FerryValue::Number(l * r)))
                    }
                    _ => unimplemented!(),
                },
                Op::Divide => match (left, right) {
                    (Some(FerryValue::Number(l)), Some(FerryValue::Number(r))) => {
                        Ok(Some(FerryValue::Number(l / r)))
                    }
                    _ => unimplemented!(),
                },
                // Op::RightArrow => todo!(),
                Op::Equals => todo!(),
            },
            _ => Ok(None),
        }
    }

    fn visit_variable(
        &mut self,
        variable: &mut Variable,
        state: &mut FerryState,
    ) -> FerryResult<FerryValue> {
        Ok(state.get_symbol_value(&variable.name))
    }

    fn visit_assign(
        &mut self,
        assign: &mut crate::syntax::Assign,
        state: &mut FerryState,
    ) -> FerryResult<FerryValue> {
        if let Some(v) = &mut assign.value {
            let value = self.evaluate(v, state).unwrap();
            state.add_symbol(&assign.name, value.clone());
            Ok(value)
        } else {
            Ok(Some(FerryValue::Number(0.)))
        }
    }

    fn visit_if_expr(
        &mut self,
        if_expr: &mut crate::syntax::If,
        state: &mut FerryState,
    ) -> FerryResult<FerryValue> {
        if let Some(conditional) = self.evaluate(&mut if_expr.condition, state)? {
            let value = if conditional.truthiness() {
                self.evaluate(&mut if_expr.then_expr, state)?
            } else {
                if let Some(else_expr) = &mut if_expr.else_expr {
                    self.evaluate(else_expr, state)?
                } else {
                    None
                }
            };
            return Ok(value);
        } else {
            Ok(None)
        }
    }
}

impl FerryValue {
    fn truthiness(&self) -> bool {
        match self {
            FerryValue::Number(n) => {
                if n <= &0. {
                    false
                } else {
                    true
                }
            }
            FerryValue::Str(s) => {
                if s.len() == 0 {
                    false
                } else {
                    true
                }
            }
            FerryValue::Boolean(b) => *b,
            FerryValue::Unit => false,
        }
    }
}
