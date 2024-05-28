use miette::{Diagnostic, Result, SourceSpan};
use thiserror::Error;

use crate::state::FerryState;
use crate::syntax::{Assign, Binary, Binding, Expr, Group, If, Lit as SLit, Loop, Variable};
use crate::token::{Ctrl, Kwd};
use crate::token::{FerryToken, Op, TokenType as TT, Val as TLit};
use crate::types::FerryTyping;

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
        } else if self.matches(&[TT::Keyword(Kwd::Let)]) {
            self.binding(state)?
        } else if self.matches(&[TT::Keyword(Kwd::Do)]) {
            self.do_loop(state)?
        } else if self.matches(&[TT::Keyword(Kwd::While)]) {
            self.while_loop(state)?
        } else {
            self.s_expression(state)?
        };

        self.consume_newline()?;

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
        self.consume_newline()?;
        let then_expr = Box::new(self.s_expression(state)?);
        self.consume_newline()?;
        let else_expr = if self.peek().get_token_type() == &TT::Keyword(Kwd::Else) {
            self.consume(&TT::Keyword(Kwd::Else), "idk how you got this")?;
            if self.peek().get_token_type() == &TT::Control(Ctrl::Colon) {
                self.consume(
                    &TT::Control(crate::token::Ctrl::Colon),
                    "colon not consumed",
                )?;
            }
            self.consume_newline()?;
            Some(Box::new(self.s_expression(state)?))
        } else {
            None
        };
        self.consume_newline()?;

        let expr = Expr::If(If {
            token,
            condition,
            then_expr,
            else_expr,
            expr_type: FerryTyping::Untyped,
        });

        Ok(expr)
    }

    fn binding(&mut self, state: &mut FerryState) -> FerryResult<Expr> {
        let token = self.previous();
        if let TT::Identifier(name) = self.advance().get_token_type() {
            self.consume(&TT::Control(Ctrl::Colon), "expected ':' after identifier")?;
            let assigned_type = if let TT::Identifier(id) = self.peek().get_token_type() {
                self.advance();
                match id.clone().as_str() {
                    "Int" => Some(crate::types::FerryType::Num),
                    "String" => Some(crate::types::FerryType::String),
                    _ => None,
                }
            } else {
                None
            };
            let value = if self.peek().get_token_type() == &TT::Operator(Op::Equals) {
                self.advance();
                Some(Box::new(self.start(state)?))
            } else {
                None
            };

            Ok(Expr::Binding(Binding {
                token,
                name: name.clone(),
                assigned_type,
                value,
                expr_type: FerryTyping::Untyped,
            }))
        } else {
            Err(FerryParseError::AlternateToken {
                help: "a?".into(),
                span: *self.previous().get_span(),
            })
        }
    }

    fn do_loop(&mut self, state: &mut FerryState) -> FerryResult<Expr> {
        let token = self.previous();
        self.consume(&TT::Control(Ctrl::Colon), "expected ':' after 'do'")?;
        let condition = None;
        let contents = Box::new(self.start(state)?);

        Ok(Expr::Loop(Loop {
            token,
            condition,
            contents,
            expr_type: FerryTyping::Untyped,
        }))
    }

    fn while_loop(&mut self, state: &mut FerryState) -> FerryResult<Expr> {
        let token = self.previous();
        let condition = Some(Box::new(self.start(state)?));
        self.consume(
            &TT::Keyword(Kwd::Do),
            "expected 'do:' after 'while' conditional",
        )?;
        self.consume(&TT::Control(Ctrl::Colon), "expected ':' after 'do'")?;
        let contents = Box::new(self.start(state)?);

        Ok(Expr::Loop(Loop {
            token,
            condition,
            contents,
            expr_type: FerryTyping::Untyped,
        }))
    }

    fn s_expression(&mut self, state: &mut FerryState) -> FerryResult<Expr> {
        let expr = self.assignment(state)?;

        Ok(expr)
    }

    fn list(&mut self, state: &mut FerryState) -> FerryResult<Expr> {
        let token = self.previous();
        let mut contents: Vec<Expr> = Vec::new();

        if self.matches(&[TT::Control(Ctrl::RightBracket)]) {
            self.consume(
                &TT::Control(Ctrl::RightBracket),
                "expected ']' after list '['",
            )?;
            contents.push(Expr::Literal(SLit::Undefined {
                token: token.clone(),
                expr_type: FerryTyping::Untyped,
            }));
            Ok(Expr::Literal(SLit::List {
                token,
                contents,
                expr_type: FerryTyping::Untyped,
                span: *self.previous().get_span(),
            }))
        } else {
            self.finish_sequence(token, state, contents)
        }
    }

    fn assignment(&mut self, state: &mut FerryState) -> FerryResult<Expr> {
        let mut expr = self.cons(state)?;

        if self.matches(&[TT::Operator(Op::Equals)]) {
            let operator = self.previous();
            let value = self.start(state)?;
            if let Expr::Variable(v) = &expr {
                expr = Expr::Assign(Assign {
                    var: Box::new(expr.clone()),
                    name: v.name.clone(),
                    value: Some(Box::new(value)),
                    expr_type: FerryTyping::Untyped,
                    token: operator,
                });
            }
        }

        Ok(expr)
    }

    fn cons(&mut self, state: &mut FerryState) -> FerryResult<Expr> {
        let mut expr = self.index(state)?;

        if self.matches(&[TT::Operator(Op::Cons)]) {
            let operator = self.previous();
            let rhs = Box::new(self.start(state)?);
            expr = Expr::Binary(Binary {
                lhs: Box::new(expr.clone()),
                operator,
                rhs,
                expr_type: FerryTyping::Untyped,
            });
        }

        Ok(expr)
    }

    fn index(&mut self, state: &mut FerryState) -> FerryResult<Expr> {
        let mut expr = self.comparison(state)?;

        if self.matches(&[TT::Operator(Op::GetI)]) {
            let lhs = Box::new(expr);
            let op = self.previous();
            let rhs = Box::new(self.start(state)?);
            expr = Expr::Binary(Binary {
                operator: op,
                lhs,
                rhs,
                expr_type: FerryTyping::Untyped,
            });
        }

        Ok(expr)
    }

    fn comparison(&mut self, state: &mut FerryState) -> FerryResult<Expr> {
        let mut expr = self.sum(state)?;

        if self.matches(&[
            TT::Operator(Op::GreaterThan),
            TT::Operator(Op::LessThan),
            TT::Operator(Op::Equality),
            TT::Operator(Op::GreaterEqual),
            TT::Operator(Op::LessEqual),
        ]) {
            let op = self.previous();
            let rhs = Box::new(self.start(state)?);
            expr = Expr::Binary(Binary {
                lhs: Box::new(expr),
                operator: op,
                rhs,
                expr_type: FerryTyping::Untyped,
            })
        }

        Ok(expr)
    }

    fn sum(&mut self, state: &mut FerryState) -> FerryResult<Expr> {
        let mut expr = self.factor(state)?;

        if self.matches(&[TT::Operator(Op::Add), TT::Operator(Op::Subtract)]) {
            let op = self.previous();
            let rhs = self.start(state)?;
            expr = Expr::Binary(Binary {
                lhs: Box::new(expr),
                operator: op,
                rhs: Box::new(rhs),
                expr_type: FerryTyping::Untyped,
            });
        }

        Ok(expr)
    }

    fn factor(&mut self, state: &mut FerryState) -> FerryResult<Expr> {
        let mut expr = self.unary(state)?;

        if self.matches(&[TT::Operator(Op::Multiply), TT::Operator(Op::Divide)]) {
            let op = self.previous();
            let rhs = self.start(state)?;
            expr = Expr::Binary(Binary {
                lhs: Box::new(expr),
                operator: op,
                rhs: Box::new(rhs),
                expr_type: FerryTyping::Untyped,
            });
        }

        Ok(expr)
    }

    fn unary(&mut self, state: &mut FerryState) -> FerryResult<Expr> {
        let mut expr = self.target(state)?;

        Ok(expr)
    }

    fn target(&mut self, state: &mut FerryState) -> FerryResult<Expr> {
        self.advance();

        match self.previous().get_token_type() {
            TT::Value(l) => Ok(match l {
                TLit::Num(n) => Expr::Literal(SLit::Number {
                    token: self.previous().clone(),
                    value: *n,
                    expr_type: FerryTyping::Untyped,
                    span: *self.previous().get_span(),
                }),
                TLit::String(s) => Expr::Literal(SLit::Str {
                    token: self.previous().clone(),
                    value: s.clone(),
                    expr_type: FerryTyping::Untyped,
                    span: *self.previous().get_span(),
                }),
                TLit::Boolean(b) => Expr::Literal(SLit::Bool {
                    token: self.previous().clone(),
                    value: *b,
                    expr_type: FerryTyping::Untyped,
                    span: *self.previous().get_span(),
                }),
                TLit::None => Expr::Literal(SLit::Undefined {
                    token: self.previous().clone(),
                    expr_type: FerryTyping::Undefined,
                }),
                // _ => unreachable!(),
            }),
            TT::Identifier(id) => {
                if self.peek().get_token_type() == &TT::Control(Ctrl::LeftBracket) {
                    let lhs = Box::new(Expr::Variable(Variable {
                        token: self.previous().clone(),
                        name: id.clone(),
                        expr_type: FerryTyping::Untyped,
                    }));
                    let operator =
                        FerryToken::new(TT::Operator(Op::GetI), self.peek().get_span().clone());
                    self.consume(&TT::Control(Ctrl::LeftBracket), "expected '[' for index ")?;
                    let rhs = Box::new(self.start(state)?);
                    self.consume(&TT::Control(Ctrl::RightBracket), "expected ']' after '['")?;
                    Ok(Expr::Binary(Binary {
                        lhs,
                        operator,
                        rhs,
                        expr_type: FerryTyping::Untyped,
                    }))
                } else {
                    Ok(Expr::Variable(Variable {
                        token: self.previous().clone(),
                        name: id.clone(),
                        expr_type: FerryTyping::Untyped,
                    }))
                }
            }
            TT::Control(Ctrl::LeftParen) => {
                let contents = Box::new(self.start(state)?);
                self.consume(&TT::Control(Ctrl::RightParen), "Expected ')' after '('")?;
                Ok(Expr::Group(Group {
                    token: self.previous().clone(),
                    contents,
                    expr_type: FerryTyping::Untyped,
                }))
            }
            TT::Control(Ctrl::LeftBracket) => self.list(state),
            _ => {
                println!("oops");
                Err(FerryParseError::UnexpectedToken {
                    msg: format!("Unexpected token: {}", self.previous().get_token_type()),
                    span: *self.previous().get_span(),
                })
            }
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

    fn consume_newline(&mut self) -> Result<Option<FerryToken>, FerryParseError> {
        if self.matches(&[TT::Control(Ctrl::Newline)]) {
            Ok(Some(self.previous()))
        } else {
            Ok(None)
        }
    }

    fn finish_sequence(
        &mut self,
        token: FerryToken,
        state: &mut FerryState,
        mut contents: Vec<Expr>,
    ) -> FerryResult<Expr> {
        while self.peek().get_token_type() != &TT::Control(Ctrl::RightBracket) {
            if self.peek().get_token_type() == &TT::Control(Ctrl::Comma) {
                self.consume(
                    &TT::Control(Ctrl::Comma),
                    "expected ',' during multivalue lists",
                )?;
            }
            let next = self.start(state)?;
            contents.push(next);
        }
        self.consume(
            &TT::Control(Ctrl::RightBracket),
            "expected right bracket after list",
        )?;
        Ok(Expr::Literal(SLit::List {
            token,
            contents,
            expr_type: FerryTyping::Untyped,
            span: *self.previous().get_span(),
        }))
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
