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
}

pub trait TypeCheckable {
    fn get_type(&self) -> &FerryType;
    fn check(&self, other: &FerryTyping) -> bool;
}

impl TypeCheckable for FerryTyping {
    fn get_type(&self) -> &FerryType {
        match self {
            FerryTyping::Assigned(a) => a,
            FerryTyping::Inferred(i) => i,
            FerryTyping::Untyped => &FerryType::Untyped,
            FerryTyping::Undefined => &FerryType::Undefined,
        }
    }

    fn check(&self, other: &FerryTyping) -> bool {
        self.get_type() == other.get_type()
    }
}

impl TypeCheckable for Expr {
    fn get_type(&self) -> &FerryType {
        match self {
            Expr::Literal(l) => match l {
                Lit::Number {
                    value: _,
                    expr_type,
                    span: _,
                } => expr_type,
                Lit::Str {
                    value: _,
                    expr_type,
                    span: _,
                } => expr_type,
                Lit::Bool {
                    value: _,
                    expr_type,
                    span: _,
                } => expr_type,
                Lit::Undefined { expr_type } => &expr_type,
            },
            Expr::Binary(b) => &b.expr_type,
            Expr::Variable(v) => &v.expr_type,
            Expr::Assign(a) => &a.expr_type,
            Expr::If(i) => &i.expr_type,
            Expr::Group(g) => &g.expr_type,
        }
    }

    fn check(&self, other: &FerryTyping) -> bool {
        self.get_type() == other.get_type()
    }
}
