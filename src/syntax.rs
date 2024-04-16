use miette::SourceSpan;

use crate::{state::FerryValue, token::FerryToken};

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Literal(Literal),
    Binary(Binary),
    Variable(Variable),
    Assign(Assign),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Number {
        value: f64,
        expr_type: FerryType,
        span: SourceSpan,
    },
    Str {
        value: String,
        expr_type: FerryType,
        span: SourceSpan,
    },
    Bool {
        value: bool,
        expr_type: FerryType,
        span: SourceSpan,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct Binary {
    pub lhs: Box<Expr>,
    pub operator: FerryToken,
    pub rhs: Box<Expr>,
    pub expr_type: FerryType,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Assign {
    pub var: Box<Expr>,
    pub name: String,
    pub value: Option<Box<Expr>>,
    pub expr_type: FerryType,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Variable {
    pub token: FerryToken,
    pub name: String,
    pub expr_type: FerryType,
}

pub trait ExprVisitor<T, S> {
    fn visit_literal(&mut self, literal: &mut Literal, state: S) -> T;
    fn visit_binary(&mut self, binary: &mut Binary, state: S) -> T;
    fn visit_variable(&mut self, variable: &mut Variable, state: S) -> T;
    fn visit_assign(&mut self, assign: &mut Assign, state: S) -> T;
}

pub fn walk_expr<T, S>(mut visitor: impl ExprVisitor<T, S>, expr: &mut Expr, state: S) -> T {
    match expr {
        Expr::Literal(literal) => visitor.visit_literal(literal, state),
        Expr::Binary(binary) => visitor.visit_binary(binary, state),
        Expr::Variable(variable) => visitor.visit_variable(variable, state),
        Expr::Assign(assign) => visitor.visit_assign(assign, state),
    }
}

impl Expr {
    pub fn get_span(&self) -> Option<SourceSpan> {
        match self {
            Expr::Literal(l) => match l {
                Literal::Number {
                    value,
                    expr_type,
                    span,
                } => Some(*span),
                Literal::Str {
                    value,
                    expr_type,
                    span,
                } => Some(*span),
                Literal::Bool {
                    value,
                    expr_type,
                    span,
                } => Some(*span),
            },
            Expr::Binary(b) => Some(b.operator.get_span().clone()),
            Expr::Variable(v) => Some(v.token.get_span().clone()),
            Expr::Assign(a) => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum FerryType {
    Untyped,
    Num,
    String,
    Boolean,
}
