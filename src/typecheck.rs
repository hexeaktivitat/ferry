use miette::{Diagnostic, Result, SourceSpan};
use thiserror::Error;

use crate::{
    state::FerryState,
    syntax::{walk_expr, Assign, Binary, Expr, ExprVisitor, FerryType, Literal as SLit, Variable},
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
            SLit::Number {
                value,
                expr_type: _,
                span,
            } => Ok(Expr::Literal(SLit::Number {
                value: *value,
                expr_type: FerryType::Num,
                span: *span,
            })),
            SLit::Str {
                value,
                expr_type: _,
                span,
            } => Ok(Expr::Literal(SLit::Str {
                value: value.clone(),
                expr_type: FerryType::String,
                span: *span,
            })),
            SLit::Bool {
                value,
                expr_type: _,
                span,
            } => Ok(Expr::Literal(SLit::Bool {
                value: *value,
                expr_type: FerryType::Boolean,
                span: *span,
            })),
            SLit::Undefined { expr_type } => todo!(),
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
        // we expect that the variable has been declared in some scope prior to being referenced
        if let (Some(derived_type), assigned_type) =
            (state.get_symbol_value(&variable.name), &variable.expr_type)
        {
            if derived_type.verify_type(assigned_type) {
                Ok(Expr::Variable(variable.clone()))
            } else if assigned_type == &FerryType::Untyped {
                // type inference: if derived_type is valid, update the type
                Ok(Expr::Variable(Variable {
                    token: variable.token.clone(),
                    name: variable.name.clone(),
                    expr_type: derived_type.get_type().clone(),
                }))
            } else {
                Err(FerryTypeError::A {
                    advice: "mismatched type:".into(),
                    span: variable.token.get_span().clone(),
                })
            }
        } else {
            Err(FerryTypeError::A {
                advice: "variable of unknown type".into(),
                span: variable.token.get_span().clone(),
            })
        }
    }

    fn visit_assign(&mut self, assign: &mut Assign, state: &mut FerryState) -> FerryResult<Expr> {
        // type inference first
        if let Some(value) = &mut assign.value {
            if let Ok(value_check) = self.check_types(value, state) {
                return Ok(Expr::Assign(Assign {
                    var: assign.var.clone(),
                    name: assign.name.clone(),
                    value: Some(Box::new(value_check.clone())),
                    expr_type: value_check.get_type().clone(),
                    token: assign.token.clone(),
                }));
            }
        }
        Err(FerryTypeError::A {
            advice: "???".into(),
            span: (0, 1).into(),
        })
    }
}

pub trait TypeCheckable {
    fn get_type(&self) -> &FerryType;
}

impl TypeCheckable for Expr {
    fn get_type(&self) -> &FerryType {
        match self {
            Expr::Literal(l) => match l {
                SLit::Number {
                    value: _,
                    expr_type,
                    span: _,
                } => expr_type,
                SLit::Str {
                    value: _,
                    expr_type,
                    span: _,
                } => expr_type,
                SLit::Bool {
                    value: _,
                    expr_type,
                    span: _,
                } => expr_type,
                SLit::Undefined { expr_type } => todo!(),
            },
            Expr::Binary(b) => &b.expr_type,
            Expr::Variable(v) => &v.expr_type,
            Expr::Assign(a) => &a.expr_type,
        }
    }
}
