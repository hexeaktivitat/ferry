use miette::{Diagnostic, Result, SourceSpan};
use thiserror::Error;

use crate::{
    state::{FerryState, FerryValue},
    syntax::{walk_expr, Binary, Expr, ExprVisitor, Literal as SLit, Variable},
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
        Ok(walk_expr(&mut *self, code, state))
    }
}

impl ExprVisitor<Option<FerryValue>, &mut FerryState> for &mut FerryInterpreter {
    fn visit_literal(&mut self, literal: &mut SLit, state: &mut FerryState) -> Option<FerryValue> {
        match literal {
            SLit::Number {
                value,
                expr_type,
                span,
            } => Some(FerryValue::Number(*value)),
            SLit::Str {
                value,
                expr_type,
                span,
            } => Some(FerryValue::Str(value.clone())),
            SLit::Bool {
                value,
                expr_type,
                span,
            } => todo!(),
        }
    }

    fn visit_binary(&mut self, binary: &mut Binary, state: &mut FerryState) -> Option<FerryValue> {
        let left = self.evaluate(&mut binary.lhs, state).unwrap();
        let right = self.evaluate(&mut binary.rhs, state).unwrap();

        let op = &binary.operator;

        match &op.get_type() {
            TT::Operator(o) => match o {
                Op::Add => match (left, right) {
                    (Some(FerryValue::Number(l)), Some(FerryValue::Number(r))) => {
                        Some(FerryValue::Number(l + r))
                    }
                    (None, None) => todo!(),
                    (None, Some(_)) => todo!(),
                    (Some(_), None) => todo!(),
                    _ => unimplemented!(),
                },
                Op::Subtract => match (left, right) {
                    (Some(FerryValue::Number(l)), Some(FerryValue::Number(r))) => {
                        Some(FerryValue::Number(l - r))
                    }
                    (None, None) => todo!(),
                    (None, Some(_)) => todo!(),
                    (Some(_), None) => todo!(),
                    _ => unimplemented!(),
                },

                Op::Multiply => match (left, right) {
                    (Some(FerryValue::Number(l)), Some(FerryValue::Number(r))) => {
                        Some(FerryValue::Number(l * r))
                    }
                    (None, None) => todo!(),
                    (None, Some(_)) => todo!(),
                    (Some(_), None) => todo!(),
                    _ => unimplemented!(),
                },
                Op::Divide => match (left, right) {
                    (Some(FerryValue::Number(l)), Some(FerryValue::Number(r))) => {
                        Some(FerryValue::Number(l / r))
                    }
                    (None, None) => todo!(),
                    (None, Some(_)) => todo!(),
                    (Some(_), None) => todo!(),
                    _ => unimplemented!(),
                },
                Op::RightArrow => todo!(),
                Op::Equals => todo!(),
            },
            _ => None,
        }
    }

    fn visit_variable(
        &mut self,
        variable: &mut Variable,
        state: &mut FerryState,
    ) -> Option<FerryValue> {
        state.get_symbol_value(&variable.name)
    }

    fn visit_assign(
        &mut self,
        assign: &mut crate::syntax::Assign,
        state: &mut FerryState,
    ) -> Option<FerryValue> {
        if let Some(v) = &mut assign.value {
            let value = self.evaluate(v, state).unwrap();
            state.add_symbol(&assign.name, value.clone());
            value
        } else {
            Some(FerryValue::Number(0.))
        }
    }
}
