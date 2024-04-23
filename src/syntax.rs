use miette::SourceSpan;

use crate::token::{FerryToken, TokenType as TT};

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Literal(Lit),
    Binary(Binary),
    Variable(Variable),
    Assign(Assign),
    If(If),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Lit {
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
    pub token: FerryToken,
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

#[derive(Debug, Clone, PartialEq)]
pub struct If {
    pub token: FerryToken,
    pub condition: Box<Expr>,
    pub then_expr: Box<Expr>,
    pub else_expr: Option<Box<Expr>>,
    pub expr_type: FerryType,
}

pub trait ExprVisitor<T, S> {
    fn visit_literal(&mut self, literal: &mut Lit, state: S) -> T;
    fn visit_binary(&mut self, binary: &mut Binary, state: S) -> T;
    fn visit_variable(&mut self, variable: &mut Variable, state: S) -> T;
    fn visit_assign(&mut self, assign: &mut Assign, state: S) -> T;
    fn visit_if_expr(&mut self, if_expr: &mut If, state: S) -> T;
}

pub fn walk_expr<T, S>(mut visitor: impl ExprVisitor<T, S>, expr: &mut Expr, state: S) -> T {
    match expr {
        Expr::Literal(literal) => visitor.visit_literal(literal, state),
        Expr::Binary(binary) => visitor.visit_binary(binary, state),
        Expr::Variable(variable) => visitor.visit_variable(variable, state),
        Expr::Assign(assign) => visitor.visit_assign(assign, state),
        Expr::If(if_expr) => visitor.visit_if_expr(if_expr, state),
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
                Lit::Number {
                    value,
                    expr_type: _,
                    span: _,
                } => write!(f, "{value}"),
                Lit::Str {
                    value,
                    expr_type: _,
                    span: _,
                } => write!(f, "{value}"),
                Lit::Bool {
                    value,
                    expr_type: _,
                    span: _,
                } => write!(f, "{value}"),
                Lit::Undefined { expr_type } => write!(f, "{expr_type}"),
            },
            Expr::Binary(b) => match b.operator.get_token_type() {
                TT::Operator(o) => match o {
                    crate::token::Op::Add => write!(f, "Add {} {}", b.lhs, b.rhs),
                    crate::token::Op::Subtract => write!(f, "Subtract {} {}", b.lhs, b.rhs),
                    crate::token::Op::Multiply => write!(f, "Multiply {} {}", b.lhs, b.rhs),
                    crate::token::Op::Divide => write!(f, "Divide {} {}", b.lhs, b.rhs),
                    crate::token::Op::Equals => write!(f, "{} equals {}", b.lhs, b.rhs),
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
            Expr::If(i) => {
                if i.else_expr.is_some() {
                    write!(
                        f,
                        "if {} then: {} else: {}",
                        i.condition,
                        i.then_expr,
                        i.else_expr.clone().unwrap()
                    )
                } else {
                    write!(f, "if {} then: {}", i.condition, i.then_expr)
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
