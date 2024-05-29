use crate::syntax::{Expr, Lit};

#[derive(Debug, Clone, PartialEq)]
pub enum FerryTyping {
    Assigned(FerryType),
    Inferred(FerryType),
    Untyped,
    Undefined,
}

#[derive(Debug, Clone, PartialEq)]
pub enum FerryType {
    Untyped,
    Undefined,
    Num,
    String,
    Boolean,
    List,
}

pub trait Typing {
    fn get_type(&self) -> &FerryType;
}

pub trait TypeCheckable {
    fn check(&self, other: &FerryType) -> bool;
}

impl Typing for FerryTyping {
    fn get_type(&self) -> &FerryType {
        match self {
            FerryTyping::Assigned(a) => a,
            FerryTyping::Inferred(i) => i,
            FerryTyping::Untyped => &FerryType::Untyped,
            FerryTyping::Undefined => &FerryType::Undefined,
        }
    }
}

impl TypeCheckable for FerryTyping {
    fn check(&self, other: &FerryType) -> bool {
        self.get_type() == other
    }
}

impl TypeCheckable for FerryType {
    fn check(&self, other: &FerryType) -> bool {
        self == other
    }
}

impl Typing for Expr {
    fn get_type(&self) -> &FerryType {
        match self {
            Expr::Literal(l) => match l {
                Lit::Number {
                    value: _,
                    expr_type,
                    span: _,
                    token: _,
                } => expr_type.get_type(),
                Lit::Str {
                    value: _,
                    expr_type,
                    span: _,
                    token: _,
                } => expr_type.get_type(),
                Lit::Bool {
                    value: _,
                    expr_type,
                    span: _,
                    token: _,
                } => expr_type.get_type(),
                Lit::Undefined {
                    expr_type,
                    token: _,
                } => expr_type.get_type(),
                Lit::List {
                    token: _,
                    contents: _,
                    expr_type,
                    span: _,
                } => expr_type.get_type(),
            },
            Expr::Binary(b) => b.expr_type.get_type(),
            Expr::Variable(v) => v.expr_type.get_type(),
            Expr::Assign(a) => a.expr_type.get_type(),
            Expr::If(i) => i.expr_type.get_type(),
            Expr::Group(g) => g.expr_type.get_type(),
            Expr::Binding(b) => b.expr_type.get_type(),
            Expr::Loop(l) => l.expr_type.get_type(),
            Expr::Unary(u) => u.expr_type.get_type(),
            Expr::For(f) => f.expr_type.get_type(),
        }
    }
}

impl TypeCheckable for Expr {
    fn check(&self, other: &FerryType) -> bool {
        self.get_type() == other
    }
}

impl std::fmt::Display for FerryTyping {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.get_type())
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
            FerryType::List => write!(f, "List"),
        }
    }
}
