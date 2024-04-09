use miette::{Diagnostic, Result, SourceSpan};
use thiserror::Error;

use crate::{
    state::FerryState,
    syntax::{walk_expr, Binary, Expr, ExprVisitor, FerryType, Literal as SLit, Variable},
};

#[derive(Error, Diagnostic, Debug)]
#[error("Type errors")]
pub enum FerryTypeError {
    #[error("asdf")]
    A {
        #[help]
        advice: String,
        #[label]
        span: SourceSpan,
    },
}

type FerryResult<T> = Result<T, FerryTypeError>;
type FerryTypecheckResult<T> = Result<Vec<T>, Vec<FerryTypeError>>;

#[derive(Debug)]
pub struct FerryTypechecker {
    syntax: Vec<Expr>,
}

impl FerryTypechecker {
    pub fn new(syntax: Vec<Expr>) -> Self {
        Self { syntax }
    }

    pub fn typecheck(&mut self, state: &mut FerryState) -> FerryTypecheckResult<Expr> {
        let mut result = Vec::new();
        let mut errors = Vec::new();

        for code in self.syntax.clone().iter_mut() {
            match self.check_types(code, state) {
                Ok(r) => result.push(r),
                Err(e) => errors.push(e),
            }
        }

        if errors.is_empty() {
            Ok(result)
        } else {
            Err(errors)
        }
    }

    fn check_types(&mut self, code: &mut Expr, state: &mut FerryState) -> FerryResult<Expr> {
        walk_expr(&mut *self, code, state)
    }
}

impl ExprVisitor<FerryResult<Expr>, &mut FerryState> for &mut FerryTypechecker {
    fn visit_literal(&mut self, literal: &mut SLit, state: &mut FerryState) -> FerryResult<Expr> {
        match literal {
            SLit::Number { value, expr_type } => Ok(Expr::Literal(SLit::Number {
                value: *value,
                expr_type: FerryType::Num,
            })),
        }
    }

    fn visit_binary(&mut self, binary: &mut Binary, state: &mut FerryState) -> FerryResult<Expr> {
        let left = self.check_types(&mut binary.lhs, state)?;
        let right = self.check_types(&mut binary.rhs, state)?;

        if left.get_type() == right.get_type() {
            let expr_type = left.get_type();
            Ok(Expr::Binary(Binary {
                lhs: Box::new(left.clone()),
                operator: binary.operator.clone(),
                rhs: Box::new(right),
                expr_type: expr_type.clone(),
            }))
        } else {
            Err(FerryTypeError::A {
                advice: "aa".into(),
                span: *binary.operator.get_span(),
            })
        }
    }

    fn visit_variable(
        &mut self,
        variable: &mut Variable,
        state: &mut FerryState,
    ) -> FerryResult<Expr> {
        todo!()
    }
}

trait TypeCheckable {
    fn get_type(&self) -> &FerryType;
}

impl TypeCheckable for Expr {
    fn get_type(&self) -> &FerryType {
        match self {
            Expr::Literal(l) => match l {
                SLit::Number { value, expr_type } => expr_type,
            },
            Expr::Binary(b) => &b.expr_type,
            Expr::Variable(v) => &v.expr_type,
        }
    }
}
