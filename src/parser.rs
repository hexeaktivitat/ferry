use miette::{Diagnostic, Result, SourceSpan};
use thiserror::Error;

use crate::state::FerryState;
use crate::syntax::{Assign, Binary, Expr, FerryType, Literal as SLit, Variable};
use crate::token::{FerryToken, Op, TokenType as TT, TokenType::Identifier, Val as TLit};

#[derive(Error, Diagnostic, Debug)]
pub enum FerryParseError {
    #[error("Unexpected token encountered")]
    AlternateToken {
        #[help]
        help: String,
        #[label]
        span: SourceSpan,
    },
}

type FerryResult<T> = Result<T, FerryParseError>;
type FerryParseResult<T> = Result<Vec<T>, Vec<FerryParseError>>;

#[derive(Debug, Clone, PartialEq)]
pub struct FerryParser {
    tokens: Vec<FerryToken>,
    current: usize,
}

impl FerryParser {
    pub fn new(tokens: Vec<FerryToken>) -> Self {
        Self { tokens, current: 0 }
    }

    pub fn parse(&mut self, state: &mut FerryState) -> FerryParseResult<Expr> {
        let mut statements = Vec::new();
        let mut errors = Vec::new();

        while !self.end_of_program() {
            match self.start(state) {
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

    fn start(&mut self, state: &mut FerryState) -> FerryResult<Expr> {
        let mut expr = self.s_expression(state)?;

        Ok(expr)
    }

    // pratt parsing starts here
    fn s_expression(&mut self, state: &mut FerryState) -> FerryResult<Expr> {
        let mut expr = self.assignment(state)?;

        Ok(expr)
    }

    fn assignment(&mut self, state: &mut FerryState) -> FerryResult<Expr> {
        let mut expr = self.sum(state)?;

        if self.matches(&[TT::Operator(Op::Equals)]) {
            let _operator = self.previous();
            let value = self.s_expression(state)?;
            if let Expr::Variable(v) = &expr {
                expr = Expr::Assign(Assign {
                    var: Box::new(expr.clone()),
                    name: v.name.clone(),
                    value: Some(Box::new(value)),
                    expr_type: FerryType::Untyped,
                });
            }
        }

        Ok(expr)
    }

    fn sum(&mut self, state: &mut FerryState) -> FerryResult<Expr> {
        let mut expr = self.factor(state)?;

        if self.matches(&[TT::Operator(Op::Add), TT::Operator(Op::Subtract)]) {
            let op = self.previous();
            let rhs = self.s_expression(state)?;
            expr = Expr::Binary(Binary {
                lhs: Box::new(expr),
                operator: op,
                rhs: Box::new(rhs),
                expr_type: FerryType::Untyped,
            });
        }

        Ok(expr)
    }

    fn factor(&mut self, state: &mut FerryState) -> FerryResult<Expr> {
        let mut expr = self.target(state)?;

        if self.matches(&[TT::Operator(Op::Multiply), TT::Operator(Op::Divide)]) {
            let op = self.previous();
            let rhs = self.s_expression(state)?;
            expr = Expr::Binary(Binary {
                lhs: Box::new(expr),
                operator: op,
                rhs: Box::new(rhs),
                expr_type: FerryType::Untyped,
            });
        }

        Ok(expr)
    }

    fn target(&mut self, _state: &mut FerryState) -> FerryResult<Expr> {
        self.advance();

        match self.previous().get_type() {
            TT::Value(l) => Ok(match l {
                TLit::Num(n) => Expr::Literal(SLit::Number {
                    value: *n,
                    expr_type: FerryType::Untyped,
                    span: *self.previous().get_span(),
                }),
                TLit::String(s) => Expr::Literal(SLit::Str {
                    value: s.clone(),
                    expr_type: FerryType::Untyped,
                    span: *self.previous().get_span(),
                }),
                TLit::Boolean(b) => Expr::Literal(SLit::Bool {
                    value: *b,
                    expr_type: FerryType::Untyped,
                    span: *self.previous().get_span(),
                }),
                TLit::None => todo!(),
                // _ => unreachable!(),
            }),
            TT::Identifier(id) => Ok(Expr::Variable(Variable {
                token: self.previous().clone(),
                name: id.clone(),
                expr_type: FerryType::Untyped,
            })),
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

    fn consume(
        &mut self,
        token_type: &impl MatchToken,
        message: &str,
    ) -> Result<FerryToken, FerryParseError> {
        if self.check(token_type) {
            Ok(self.advance())
        } else {
            Err(FerryParseError::AlternateToken {
                help: message.into(),
                span: *self.tokens[self.current].get_span(),
            })
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
