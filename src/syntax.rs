use miette::SourceSpan;

use crate::token::{FerryToken, Op, TokenType as TT};
use crate::types::{FerryType, FerryTyping};

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Literal(Lit),
    Binary(Binary),
    Unary(Unary),
    Variable(Variable),
    Assign(Assign),
    If(If),
    Group(Group),
    Binding(Binding),
    Loop(Loop),
    For(For),
    Function(Function),
    Call(Call),
    Module(Module),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Lit {
    Undefined {
        token: FerryToken,
        expr_type: FerryTyping,
    },
    Number {
        token: FerryToken,
        value: i64,
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
pub struct Unary {
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
    pub assigned_type: Option<String>,
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

#[derive(Debug, Clone, PartialEq)]
pub struct For {
    pub token: FerryToken,
    pub variable: Option<Box<Expr>>,
    pub iterator: Box<Expr>,
    pub iterator_type: Option<FerryType>,
    pub contents: Box<Expr>,
    pub expr_type: FerryTyping,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub token: FerryToken,
    pub name: String,
    pub args: Option<Vec<Expr>>,
    pub contents: Box<Expr>,
    pub return_type: Option<FerryType>,
    pub expr_type: FerryTyping,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Call {
    pub invoker: Box<Expr>,
    pub name: String,
    pub token: FerryToken,
    pub args: Vec<Expr>,
    pub expr_type: FerryTyping,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Module {
    pub name: String,
    pub token: FerryToken,
    pub functions: Vec<Function>,
}

pub trait ExprVisitor<T, S> {
    fn visit_literal(&mut self, literal: &mut Lit, state: S) -> T;
    fn visit_binary(&mut self, binary: &mut Binary, state: S) -> T;
    fn visit_unary(&mut self, unary: &mut Unary, state: S) -> T;
    fn visit_variable(&mut self, variable: &mut Variable, state: S) -> T;
    fn visit_assign(&mut self, assign: &mut Assign, state: S) -> T;
    fn visit_if_expr(&mut self, if_expr: &mut If, state: S) -> T;
    fn visit_group(&mut self, group: &mut Group, state: S) -> T;
    fn visit_binding(&mut self, binding: &mut Binding, state: S) -> T;
    fn visit_loop(&mut self, loop_expr: &mut Loop, state: S) -> T;
    fn visit_for(&mut self, for_expr: &mut For, state: S) -> T;
    fn visit_function(&mut self, function: &mut Function, state: S) -> T;
    fn visit_call(&mut self, call: &mut Call, state: S) -> T;
    fn visit_module(&mut self, module: &mut Module, state: S) -> T;
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
        Expr::Unary(unary) => visitor.visit_unary(unary, state),
        Expr::For(for_expr) => visitor.visit_for(for_expr, state),
        Expr::Function(function) => visitor.visit_function(function, state),
        Expr::Call(call) => visitor.visit_call(call, state),
        Expr::Module(module) => visitor.visit_module(module, state),
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
            Expr::Unary(u) => &u.operator,
            Expr::For(f) => &f.token,
            Expr::Function(f) => &f.token,
            Expr::Call(c) => &c.token,
            Expr::Module(m) => &m.token,
        }
    }
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Literal(l) => match l {
                Lit::Number {
                    value,
                    expr_type,
                    span: _,
                    token: _,
                } => write!(f, "{expr_type}: {value}"),
                Lit::Str {
                    value,
                    expr_type,
                    span: _,
                    token: _,
                } => write!(f, "{expr_type}: {value}"),
                Lit::Bool {
                    value,
                    expr_type,
                    span: _,
                    token: _,
                } => write!(f, "{expr_type}: {value}"),
                Lit::Undefined {
                    expr_type,
                    token: _,
                } => write!(f, "{expr_type}"),
                Lit::List {
                    token: _,
                    contents,
                    expr_type,
                    span: _,
                } => {
                    let mut formatting = String::new();
                    formatting.push('[');
                    let mut items = contents.iter().peekable();
                    while let Some(item) = items.next() {
                        formatting.push_str(format!("{item}").as_str());
                        if items.peek().is_some() {
                            formatting.push_str(", ");
                        }
                    }
                    formatting.push(']');
                    write!(f, "{expr_type}: {formatting}")
                }
            },
            Expr::Binary(b) => match b.operator.get_token_type() {
                TT::Operator(o) => match o {
                    Op::Add => write!(f, "Add {} {}", b.lhs, b.rhs),
                    Op::Subtract => write!(f, "Subtract {} {}", b.lhs, b.rhs),
                    Op::Multiply => write!(f, "Multiply {} {}", b.lhs, b.rhs),
                    Op::Divide => write!(f, "Divide {} {}", b.lhs, b.rhs),
                    Op::Equals => write!(f, "{} equals {}", b.lhs, b.rhs),
                    Op::LessThan => write!(f, "{} is less than {}", b.lhs, b.rhs),
                    Op::GreaterThan => {
                        write!(f, "{} is greater than {}", b.lhs, b.rhs)
                    }
                    Op::Equality => write!(f, "{} is equal to {}", b.lhs, b.rhs),
                    Op::LessEqual => {
                        write!(f, "{} is less than or equal to {}", b.lhs, b.rhs)
                    }
                    Op::GreaterEqual => {
                        write!(f, "{} is greater than or equal to {}", b.lhs, b.rhs)
                    }
                    Op::GetI => write!(f, "GetI"),
                    Op::Cons => write!(f, "Cons"),
                },
                _ => unreachable!(),
            },
            Expr::Variable(v) => write!(f, "{}: {}", v.name, v.expr_type),
            Expr::Assign(a) => {
                if a.value.is_some() {
                    write!(
                        f,
                        "{}: {} is {}",
                        a.var,
                        a.expr_type,
                        a.value.clone().unwrap()
                    )
                } else {
                    write!(f, "{}: {} is NULL", a.var, a.expr_type)
                }
            }
            Expr::If(i) => {
                if i.else_expr.is_some() {
                    write!(
                        f,
                        "if {} then: {} else: {} (type: {})",
                        i.condition,
                        i.then_expr,
                        i.else_expr.clone().unwrap(),
                        i.expr_type,
                    )
                } else {
                    write!(
                        f,
                        "if {} then: {} (type: {})",
                        i.condition, i.then_expr, i.expr_type
                    )
                }
            }
            Expr::Group(g) => write!(f, "{}: ( {} )", g.expr_type, g.contents),
            Expr::Binding(b) => {
                if b.value.is_some() {
                    write!(
                        f,
                        "let {}: {} = {}",
                        b.name,
                        b.expr_type,
                        b.value.clone().unwrap()
                    )
                } else {
                    write!(f, "let {}: {}", b.name, b.expr_type)
                }
            }
            Expr::Loop(l) => write!(f, "loop (type: {})", l.expr_type),
            Expr::Unary(u) => write!(f, "{}: {} (type: {})", u.operator, u.rhs, u.expr_type),
            Expr::For(f_expr) => {
                if f_expr.variable.is_some() {
                    write!(
                        f,
                        "for {} in {:#?}: {} \n{}",
                        f_expr.variable.clone().unwrap(),
                        f_expr.iterator,
                        f_expr.expr_type,
                        f_expr.contents,
                    )
                } else {
                    write!(
                        f,
                        "for {:#?}: {}\n{}",
                        f_expr.iterator, f_expr.expr_type, f_expr.contents
                    )
                }
            }
            Expr::Function(func) => {
                if func.args.is_some() {
                    write!(
                        f,
                        "{}({:#?}) returns {}\n{}",
                        func.name, func.args, func.expr_type, func.contents
                    )
                } else {
                    write!(
                        f,
                        "{}() returns {}\n{}",
                        func.name, func.expr_type, func.contents
                    )
                }
            }
            Expr::Call(c) => write!(f, "{}({:?})", c.name, c.args),
            Expr::Module(m) => write!(f, "{}", m.name),
        }
    }
}
