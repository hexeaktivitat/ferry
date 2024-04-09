use crate::{state::FerryValue, token::FerryToken};

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Literal(Literal),
    Binary(Binary),
    Variable(Variable),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Number { value: f64, expr_type: FerryType },
}

#[derive(Debug, Clone, PartialEq)]
pub struct Binary {
    pub lhs: Box<Expr>,
    pub operator: FerryToken,
    pub rhs: Box<Expr>,
    pub expr_type: FerryType,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Variable {
    pub token: FerryToken,
    pub name: String,
    pub value: Option<FerryValue>,
    pub expr_type: FerryType,
}

pub trait ExprVisitor<T, S> {
    fn visit_literal(&mut self, literal: &mut Literal, state: S) -> T;
    fn visit_binary(&mut self, binary: &mut Binary, state: S) -> T;
    fn visit_variable(&mut self, variable: &mut Variable, state: S) -> T;
}

pub fn walk_expr<T, S>(mut visitor: impl ExprVisitor<T, S>, expr: &mut Expr, state: S) -> T {
    match expr {
        Expr::Literal(literal) => visitor.visit_literal(literal, state),
        Expr::Binary(binary) => visitor.visit_binary(binary, state),
        Expr::Variable(variable) => visitor.visit_variable(variable, state),
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum FerryType {
    Untyped,
    Num,
    String,
}
