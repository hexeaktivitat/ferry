use miette::SourceSpan;

use crate::token::{FerryToken, TokenType as TT};
use crate::types::{FerryType, FerryTyping};

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Literal(Lit),
    Binary(Binary),
    Variable(Variable),
    Assign(Assign),
    If(If),
    Group(Group),
    Binding(Binding),
    Loop(Loop),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Lit {
    Undefined {
        token: FerryToken,
        expr_type: FerryTyping,
    },
    Number {
        token: FerryToken,
        value: f64,
        expr_type: FerryTyping,
        span: SourceSpan,
    },
    Str {
        token: FerryToken,
        value: String,
        expr_type: FerryTyping,
        span: SourceSpan,
    },
    Bool {
        token: FerryToken,
        value: bool,
        expr_type: FerryTyping,
        span: SourceSpan,
    },
    List {
        token: FerryToken,
        contents: Vec<Expr>,
        expr_type: FerryTyping,
        span: SourceSpan,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct Binary {
    pub lhs: Box<Expr>,
    pub operator: FerryToken,
    pub rhs: Box<Expr>,
    pub expr_type: FerryTyping,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Assign {
    pub token: FerryToken,
    pub var: Box<Expr>,
    pub name: String,
    pub value: Option<Box<Expr>>,
    pub expr_type: FerryTyping,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Variable {
    pub token: FerryToken,
    pub name: String,
    pub expr_type: FerryTyping,
}

#[derive(Debug, Clone, PartialEq)]
pub struct If {
    pub token: FerryToken,
    pub condition: Box<Expr>,
    pub then_expr: Box<Expr>,
    pub else_expr: Option<Box<Expr>>,
    pub expr_type: FerryTyping,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Group {
    pub token: FerryToken,
    pub contents: Box<Expr>,
    pub expr_type: FerryTyping,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Binding {
    pub token: FerryToken,
    pub name: String,
    pub assigned_type: Option<FerryType>,
    pub value: Option<Box<Expr>>,
    pub expr_type: FerryTyping,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Loop {
    pub token: FerryToken,
    pub condition: Option<Box<Expr>>,
    pub contents: Box<Expr>,
    pub expr_type: FerryTyping,
}

pub trait ExprVisitor<T, S> {
    fn visit_literal(&mut self, literal: &mut Lit, state: S) -> T;
    fn visit_binary(&mut self, binary: &mut Binary, state: S) -> T;
    fn visit_variable(&mut self, variable: &mut Variable, state: S) -> T;
    fn visit_assign(&mut self, assign: &mut Assign, state: S) -> T;
    fn visit_if_expr(&mut self, if_expr: &mut If, state: S) -> T;
    fn visit_group(&mut self, group: &mut Group, state: S) -> T;
    fn visit_binding(&mut self, binding: &mut Binding, state: S) -> T;
    fn visit_loop(&mut self, loop_expr: &mut Loop, state: S) -> T;
}

pub fn walk_expr<T, S>(mut visitor: impl ExprVisitor<T, S>, expr: &mut Expr, state: S) -> T {
    match expr {
        Expr::Literal(literal) => visitor.visit_literal(literal, state),
        Expr::Binary(binary) => visitor.visit_binary(binary, state),
        Expr::Variable(variable) => visitor.visit_variable(variable, state),
        Expr::Assign(assign) => visitor.visit_assign(assign, state),
        Expr::If(if_expr) => visitor.visit_if_expr(if_expr, state),
        Expr::Group(group) => visitor.visit_group(group, state),
        Expr::Binding(binding) => visitor.visit_binding(binding, state),
        Expr::Loop(loop_expr) => visitor.visit_loop(loop_expr, state),
    }
}

impl Expr {
    pub fn get_token(&self) -> &FerryToken {
        match self {
            Expr::Literal(l) => match l {
                Lit::Undefined {
                    token,
                    expr_type: _,
                } => token,
                Lit::Number {
                    token,
                    value: _,
                    expr_type: _,
                    span: _,
                } => token,
                Lit::Str {
                    token,
                    value: _,
                    expr_type: _,
                    span: _,
                } => token,
                Lit::Bool {
                    token,
                    value: _,
                    expr_type: _,
                    span: _,
                } => token,
                Lit::List {
                    token,
                    contents: _,
                    expr_type: _,
                    span: _,
                } => token,
            },
            Expr::Binary(b) => &b.operator,
            Expr::Variable(v) => &v.token,
            Expr::Assign(a) => &a.token,
            Expr::If(i) => &i.token,
            Expr::Group(g) => &g.token,
            Expr::Binding(b) => &b.token,
            Expr::Loop(l) => &l.token,
        }
    }
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Literal(l) => match l {
                Lit::Number {
                    value,
                    expr_type: _,
                    span: _,
                    token: _,
                } => write!(f, "{value}"),
                Lit::Str {
                    value,
                    expr_type: _,
                    span: _,
                    token: _,
                } => write!(f, "{value}"),
                Lit::Bool {
                    value,
                    expr_type: _,
                    span: _,
                    token: _,
                } => write!(f, "{value}"),
                Lit::Undefined {
                    expr_type,
                    token: _,
                } => write!(f, "{expr_type}"),
                Lit::List {
                    token: _,
                    contents,
                    expr_type: _,
                    span: _,
                } => write!(f, "{:?}", contents),
            },
            Expr::Binary(b) => match b.operator.get_token_type() {
                TT::Operator(o) => match o {
                    crate::token::Op::Add => write!(f, "Add {} {}", b.lhs, b.rhs),
                    crate::token::Op::Subtract => write!(f, "Subtract {} {}", b.lhs, b.rhs),
                    crate::token::Op::Multiply => write!(f, "Multiply {} {}", b.lhs, b.rhs),
                    crate::token::Op::Divide => write!(f, "Divide {} {}", b.lhs, b.rhs),
                    crate::token::Op::Equals => write!(f, "{} equals {}", b.lhs, b.rhs),
                    crate::token::Op::LessThan => write!(f, "{} is less than {}", b.lhs, b.rhs),
                    crate::token::Op::GreaterThan => {
                        write!(f, "{} is greater than {}", b.lhs, b.rhs)
                    }
                    crate::token::Op::Equality => write!(f, "{} is equal to {}", b.lhs, b.rhs),
                    crate::token::Op::LessEqual => {
                        write!(f, "{} is less than or equal to {}", b.lhs, b.rhs)
                    }
                    crate::token::Op::GreaterEqual => {
                        write!(f, "{} is greater than or equal to {}", b.lhs, b.rhs)
                    }
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
            Expr::Group(g) => write!(f, "( {} )", g.contents),
            Expr::Binding(b) => write!(f, "let {}", b.name),
            Expr::Loop(_l) => write!(f, "loop"),
        }
    }
}
