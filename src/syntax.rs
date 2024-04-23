use miette::SourceSpan;

use crate::token::{FerryToken, TokenType as TT};

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Literal(Literal),
    Binary(Binary),
    Variable(Variable),
    Assign(Assign),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Undefined {
        expr_type: FerryType,
    },
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

#[derive(Debug, Clone, PartialEq)]
pub enum FerryType {
    Untyped,
    Undefined,
    Num,
    String,
    Boolean,
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Literal(l) => match l {
                Literal::Number {
                    value,
                    expr_type: _,
                    span: _,
                } => write!(f, "{value}"),
                Literal::Str {
                    value,
                    expr_type: _,
                    span: _,
                } => write!(f, "{value}"),
                Literal::Bool {
                    value,
                    expr_type: _,
                    span: _,
                } => write!(f, "{value}"),
                Literal::Undefined { expr_type } => write!(f, "{expr_type}"),
            },
            Expr::Binary(b) => match b.operator.get_type() {
                TT::Operator(o) => match o {
                    crate::token::Op::Add => write!(f, "Add {} {}", b.lhs, b.rhs),
                    crate::token::Op::Subtract => write!(f, "Subtract {} {}", b.lhs, b.rhs),
                    crate::token::Op::Multiply => write!(f, "Multiply {} {}", b.lhs, b.rhs),
                    crate::token::Op::Divide => write!(f, "Divide {} {}", b.lhs, b.rhs),
                    crate::token::Op::Equals => write!(f, "{} equals {}", b.lhs, b.rhs),
                    crate::token::Op::RightArrow => todo!(),
                },
                _ => unreachable!(),
            },
            Expr::Variable(v) => write!(f, "{}", v.name),
            Expr::Assign(a) => {
                if a.value.is_some() {
                    write!(f, "{} is {}", a.var, a.value.clone().unwrap())
                } else {
                    write!(f, "{} is NULL", a.var)
                }
            }
        }
    }
}

impl std::fmt::Display for FerryType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FerryType::Untyped => write!(f, "Untyped"),
            FerryType::Undefined => write!(f, "Undefined"),
            FerryType::Num => write!(f, "Num"),
            FerryType::String => write!(f, "String"),
            FerryType::Boolean => write!(f, "Boolean"),
        }
    }
}
