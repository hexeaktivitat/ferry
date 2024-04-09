use miette::{Diagnostic, Result, SourceSpan};
use thiserror::Error;

use crate::state::FerryState;
use crate::syntax::{Binary, Expr, FerryType, Literal as SLit, Variable};
use crate::token::{FerryToken, Literal as TLit, Op, TokenType as TT, TokenType::Identifier};

#[derive(Error, Diagnostic, Debug)]
pub enum FerryParseError {}

type FerryResult<T> = Result<T, FerryParseError>;
type FerryParseResult<T> = Result<Vec<T>, Vec<FerryParseError>>;

#[derive(Debug, Clone, PartialEq)]
pub struct FerryParser {
    tokens: Vec<FerryToken>,
    current: usize,
    pub state: FerryState,
}

impl FerryParser {
    pub fn new(tokens: Vec<FerryToken>) -> Self {
        Self {
            tokens,
            current: 0,
            state: FerryState::new(),
        }
    }

    pub fn parse(&mut self) -> FerryParseResult<Expr> {
        let mut statements = Vec::new();
        let mut errors = Vec::new();

        while !self.end_of_program() {
            match self.start() {
                Ok(s) => statements.push(s),
                Err(e) => errors.push(e),
            }
        }

        if errors.is_empty() {
            Ok(statements)
        } else {
            Err(errors)
        }
    }

    fn start(&mut self) -> FerryResult<Expr> {
        let mut expr = self.s_expression()?;

        Ok(expr)
    }

    // pratt parsing starts here
    fn s_expression(&mut self) -> FerryResult<Expr> {
        let mut expr = self.identifier()?;

        Ok(expr)
    }

    fn identifier(&mut self) -> FerryResult<Expr> {
        let mut expr = self.sum()?;

        if let Identifier(name) = self.previous().get_type() {
            expr = Expr::Variable(Variable {
                token: self.previous().clone(),
                name: name.clone(),
                value: None,
                expr_type: FerryType::Untyped,
            });
        }

        Ok(expr)
    }

    fn sum(&mut self) -> FerryResult<Expr> {
        let mut expr = self.factor()?;

        if self.matches(&[TT::Operator(Op::Add), TT::Operator(Op::Subtract)]) {
            let op = self.previous();
            let rhs = self.s_expression()?;
            expr = Expr::Binary(Binary {
                lhs: Box::new(expr),
                operator: op,
                rhs: Box::new(rhs),
                expr_type: FerryType::Untyped,
            });
        }

        Ok(expr)
    }

    fn factor(&mut self) -> FerryResult<Expr> {
        let mut expr = self.target()?;

        if self.matches(&[TT::Operator(Op::Multiply), TT::Operator(Op::Divide)]) {
            let op = self.advance();
            let rhs = self.s_expression()?;
            expr = Expr::Binary(Binary {
                lhs: Box::new(expr),
                operator: op,
                rhs: Box::new(rhs),
                expr_type: FerryType::Untyped,
            });
        }

        Ok(expr)
    }

    fn target(&mut self) -> FerryResult<Expr> {
        self.advance();

        match self.previous().get_type() {
            TT::Value(l) => Ok(match l {
                TLit::Num(n) => Expr::Literal(SLit::Number {
                    value: *n,
                    expr_type: FerryType::Untyped,
                }),
                _ => unreachable!(),
            }),
            _ => unreachable!(),
        }
    }

    // helper functions
    fn peek(&self) -> FerryToken {
        self.tokens[self.current].clone()
    }

    fn advance(&mut self) -> FerryToken {
        if !self.end_of_program() {
            self.current += 1;
        }
        self.previous()
    }

    fn previous(&self) -> FerryToken {
        self.tokens[self.current - 1].clone()
    }

    fn check(&self, t: &impl MatchToken) -> bool {
        if self.end_of_program() {
            false
        } else {
            t.matches(&self.peek())
        }
    }

    fn matches(&mut self, tokens: &[TT]) -> bool {
        for t in tokens {
            if self.check(t) {
                self.advance();
                return true;
            }
        }
        false
    }

    fn end_of_program(&self) -> bool {
        self.tokens[self.current].get_type() == &TT::End
    }
}

// helper classes and types
#[derive(Debug, Clone, Copy, PartialOrd, Ord, PartialEq, Eq)]
enum Associative {
    Neither,
    Left,
    Right,
}

#[derive(Debug, Copy, PartialEq, Eq, Ord, PartialOrd, Clone)]
enum Precedence {
    Start,
    Assign,
    Sum,
    Product,
    Call,
}

trait MatchToken {
    fn matches(&self, token: &FerryToken) -> bool;
}

impl MatchToken for TT {
    fn matches(&self, token: &FerryToken) -> bool {
        &token.get_type() == &self
    }
}

impl<F> MatchToken for F
where
    F: Fn(&FerryToken) -> bool,
{
    fn matches(&self, token: &FerryToken) -> bool {
        self(token)
    }
}

// pratt parser stuff
