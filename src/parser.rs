use miette::{Diagnostic, Result, SourceSpan};
use thiserror::Error;

use crate::state::FerryState;
use crate::syntax::{Assign, Binary, Expr, Group, If, Lit as SLit, Variable};
use crate::token::{Ctrl, Kwd};
use crate::token::{FerryToken, Op, TokenType as TT, Val as TLit};
use crate::types::{FerryType, FerryTyping};

#[derive(Error, Diagnostic, Debug)]
pub enum FerryParseError {
    #[error("Expected different token")]
    AlternateToken {
        #[help]
        help: String,
        #[label]
        span: SourceSpan,
    },
    #[error("Unexpected token")]
    UnexpectedToken {
        #[help]
        msg: String,
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
        let expr = self.keywords(state)?;

        Ok(expr)
    }

    // pratt parsing starts here

    fn keywords(&mut self, state: &mut FerryState) -> FerryResult<Expr> {
        let expr = if self.matches(&[TT::Keyword(Kwd::If)]) {
            self.if_expr(state)?
        } else {
            self.s_expression(state)?
        };

        Ok(expr)
    }

    fn if_expr(&mut self, state: &mut FerryState) -> FerryResult<Expr> {
        let token = self.previous();
        let condition = Box::new(self.s_expression(state)?);
        self.consume(
            &TT::Keyword(Kwd::Then),
            "expected 'then' after 'if' conditional",
        )?;
        self.consume(&TT::Control(Ctrl::Colon), "expected ':' after 'then'")?;
        let then_expr = Box::new(self.s_expression(state)?);
        let else_expr = if self.peek().get_token_type() == &TT::Keyword(Kwd::Else) {
            self.consume(&TT::Keyword(Kwd::Else), "idk how you got this")?;
            if self.peek().get_token_type() == &TT::Control(Ctrl::Colon) {
                self.consume(
                    &TT::Control(crate::token::Ctrl::Colon),
                    "colon not consumed",
                )?;
            }
            Some(Box::new(self.s_expression(state)?))
        } else {
            None
        };

        let expr = Expr::If(If {
            token,
            condition,
            then_expr,
            else_expr,
            expr_type: FerryType::Untyped,
        });

        Ok(expr)
    }

    fn s_expression(&mut self, state: &mut FerryState) -> FerryResult<Expr> {
        let expr = self.assignment(state)?;

        Ok(expr)
    }

    fn assignment(&mut self, state: &mut FerryState) -> FerryResult<Expr> {
        let mut expr = self.sum(state)?;

        if self.matches(&[TT::Operator(Op::Equals)]) {
            let operator = self.previous();
            let value = self.s_expression(state)?;
            if let Expr::Variable(v) = &expr {
                expr = Expr::Assign(Assign {
                    var: Box::new(expr.clone()),
                    name: v.name.clone(),
                    value: Some(Box::new(value)),
                    expr_type: FerryType::Untyped,
                    token: operator,
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

    fn target(&mut self, state: &mut FerryState) -> FerryResult<Expr> {
        self.advance();

        match self.previous().get_token_type() {
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
                TLit::None => Expr::Literal(SLit::Undefined {
                    expr_type: FerryType::Undefined,
                }),
                // _ => unreachable!(),
            }),
            TT::Identifier(id) => Ok(Expr::Variable(Variable {
                token: self.previous().clone(),
                name: id.clone(),
                expr_type: FerryType::Untyped,
            })),
            TT::Control(Ctrl::LeftParen) => {
                let contents = Box::new(self.s_expression(state)?);
                self.consume(&TT::Control(Ctrl::RightParen), "Expected ')' after '('")?;
                Ok(Expr::Group(Group {
                    token: self.previous().clone(),
                    contents,
                    expr_type: FerryType::Untyped,
                }))
            }
            _ => Err(FerryParseError::UnexpectedToken {
                msg: format!("Unexpected token: {}", self.previous().get_token_type()),
                span: self.previous().get_span().clone(),
            }),
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
        self.tokens[self.current].get_token_type() == &TT::End
    }
}

trait MatchToken {
    fn matches(&self, token: &FerryToken) -> bool;
}

impl MatchToken for TT {
    fn matches(&self, token: &FerryToken) -> bool {
        &token.get_token_type() == &self
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
