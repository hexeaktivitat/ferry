use miette::{Diagnostic, Result, SourceSpan};
use thiserror::Error;

use crate::lexer::token::{Ctrl, Kwd};
use crate::lexer::token::{Op, Token, TokenType as TT, Val as TLit};
use crate::printerr::{FerryLexErrors, FerryParseErrors};
use crate::state::State;
use crate::state::types::{FerryType, FerryTyping};
use syntax::{
    Assign, Binary, Binding, Call, Expr, For, Function, Group, If, Import, Lit as SLit, Loop,
    Module, Unary, Variable,
};

pub(crate) mod syntax;

#[allow(dead_code)]
#[derive(Error, Diagnostic, Debug)]
pub enum FerryParseError {
    #[error("Expected different token")]
    AlternateToken {
        #[help]
        help: String,
        #[label("unexpected token, value, or identifier")]
        span: SourceSpan,
    },
    #[error("Unexpected token")]
    UnexpectedToken {
        #[help]
        msg: String,
        #[label("unexpected token, value, or identifier")]
        span: SourceSpan,
    },
    #[error("Unexpected end of line")]
    UnexpectedEndOfLine {
        #[help]
        msg: String,
        #[label("Found EOL")]
        span: SourceSpan,
    },
    #[error("Unexpected end of file")]
    UnexpectedEndOfFile {
        #[help]
        msg: String,
        #[label("Found EOF")]
        span: SourceSpan,
    },
    #[error("Module parse error")]
    ModuleParseFailure {
        #[help]
        msg: String,
        #[label("module")]
        span: SourceSpan,
    },
}

type FerryResult<T> = Result<T, FerryParseError>;
type FerryParseResult<T> = Result<Vec<T>, Vec<FerryParseError>>;

#[derive(Debug, Clone, PartialEq)]
pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, current: 0 }
    }

    pub fn parse(&mut self, state: &mut State) -> FerryParseResult<Expr> {
        let mut statements = Vec::new();
        let mut errors = Vec::new();

        while !self.end_of_program() {
            self.start(state)
                .map_or_else(|e| errors.push(e), |s| statements.push(s));
        }

        if errors.is_empty() {
            Ok(statements)
        } else {
            Err(errors)
        }
    }

    fn start(&mut self, state: &mut State) -> FerryResult<Expr> {
        let expr = self.keywords(state)?;

        Ok(expr)
    }

    // pratt parsing starts here

    fn keywords(&mut self, state: &mut State) -> FerryResult<Expr> {
        let expr = if self.matches(&[TT::Keyword(Kwd::If)]) {
            self.if_expr(state)
        } else if self.matches(&[TT::Keyword(Kwd::Let)]) {
            self.binding(state)
        } else if self.matches(&[TT::Keyword(Kwd::Do)]) {
            self.do_loop(state)
        } else if self.matches(&[TT::Keyword(Kwd::While)]) {
            self.while_loop(state)
        } else if self.matches(&[TT::Keyword(Kwd::For)]) {
            self.for_loop(state)
        } else if self.matches(&[TT::Keyword(Kwd::Def)]) {
            self.function(state)
        } else if self.matches(&[TT::Keyword(Kwd::Export)]) {
            self.module(state)
        } else if self.matches(&[TT::Keyword(Kwd::Import)]) {
            self.import(state)
        } else {
            self.s_expression(state)
        };

        if expr.is_err() {
            self.synchronize();
        }

        expr
    }

    fn if_expr(&mut self, state: &mut State) -> FerryResult<Expr> {
        let token = self.previous();
        let condition = Box::new(self.s_expression(state)?);
        self.consume(
            &TT::Keyword(Kwd::Then),
            "expected 'then' after 'if' conditional",
        )?;
        self.consume(&TT::Control(Ctrl::Colon), "expected ':' after 'then'")?;
        // self.consume_newline()?;
        let then_expr = Box::new(self.start(state)?);
        // self.consume_newline()?;
        // let else_expr = if self.peek().get_token_type() == &TT::Keyword(Kwd::Else) {
        let else_expr = if self.check(&TT::Keyword(Kwd::Else)) {
            self.consume(&TT::Keyword(Kwd::Else), "idk how you got this")?;
            self.consume(&TT::Control(Ctrl::Colon), "expected ':' after 'else'")?;
            // self.consume_newline()?;
            Some(Box::new(self.start(state)?))
        } else {
            None
        };
        // self.consume_newline()?;

        let expr = Expr::If(If {
            token,
            condition,
            then_expr,
            else_expr,
            expr_type: FerryTyping::Untyped,
        });

        Ok(expr)
    }

    fn binding(&mut self, state: &mut State) -> FerryResult<Expr> {
        let token = self.previous();
        let span_start = token.get_span().offset();
        let mut span_end: usize = span_start + token.get_span().len();
        if let TT::Identifier(name) = self.advance().get_token_type() {
            self.consume(&TT::Control(Ctrl::Colon), "expected ':' after identifier")?;
            // let assigned_type_token = self.peek();
            let assigned_type = if let TT::Identifier(_) = self.peek().get_token_type() {
                span_end = self.peek().get_span().offset() + self.peek().get_span().len();
                Some(self.advance())
            } else {
                None
            };
            let value = if self.peek().get_token_type() == &TT::Operator(Op::Equals) {
                let equals_token = self.advance();
                span_end = equals_token.get_span().offset() + equals_token.get_span().len();

                Some(Box::new(self.start(state)?))
            } else {
                None
            };

            state.add_variable(name, None);

            Ok(Expr::Binding(Binding {
                token,
                name: name.clone(),
                assigned_type,
                // assigned_type_token,
                value,
                // value_token: None,
                expr_type: FerryTyping::Untyped,
                span: (span_start, span_end - span_start).into(),
            }))
        } else {
            Err(FerryParseError::AlternateToken {
                help: "a?".into(),
                span: *self.previous().get_span(),
            })
        }
    }

    fn do_loop(&mut self, state: &mut State) -> FerryResult<Expr> {
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

    fn while_loop(&mut self, state: &mut State) -> FerryResult<Expr> {
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

    fn for_loop(&mut self, state: &mut State) -> FerryResult<Expr> {
        let token = self.previous();
        let variable = Some(Box::new(self.start(state)?)); //explicitly expect variable decl
        self.consume(
            &TT::Control(Ctrl::Colon),
            "expected ':' after variable iterator name",
        )?;
        let iterator_type = if let TT::Identifier(id) = self.peek().get_token_type() {
            self.advance();
            match id.clone().as_str() {
                "Int" => Some(FerryType::Int),
                "String" => Some(FerryType::String),
                _ => Some(FerryType::Int), // coerce all types to Num
            }
        } else {
            None
        };
        self.consume(&TT::Keyword(Kwd::In), "expected 'in' after 'for'")?;
        let iterator = Box::new(self.start(state)?);
        self.consume(&TT::Control(Ctrl::Colon), "expected ':' after 'do'")?;
        let contents = Box::new(self.start(state)?);

        Ok(Expr::For(For {
            token,
            variable,
            iterator,
            iterator_type,
            contents,
            expr_type: FerryTyping::Untyped,
        }))
    }

    fn function(&mut self, state: &mut State) -> FerryResult<Expr> {
        let token = self.previous();
        self.consume(&TT::Keyword(Kwd::Fn), "expected 'fn' after 'def'")?;
        let Some(name) = self.advance().get_id() else {
            return Err(FerryParseError::UnexpectedToken {
                msg: format!(
                    "Expected identifier, found {}",
                    self.previous().get_token_type()
                ),
                span: *self.previous().get_span(),
            });
        };
        self.consume(
            &TT::Control(Ctrl::LeftParen),
            "expected '(' after function identifier",
        )?;
        let args = if self.peek().get_token_type() == &TT::Control(Ctrl::RightParen) {
            None
        } else {
            let mut ret = Vec::new();
            while self.peek().get_token_type() != &TT::Control(Ctrl::RightParen) {
                let param_id_token = self.advance();
                let span_start = param_id_token.get_span().offset();
                let span_end = span_start + param_id_token.get_span().len();
                let param_id = if let TT::Identifier(id) = param_id_token.get_token_type() {
                    id.clone()
                } else {
                    return Err(FerryParseError::UnexpectedToken {
                        msg: format!(
                            "Expected parameter ID, found {}",
                            self.previous().get_token_type()
                        ),
                        span: *self.previous().get_span(),
                    });
                };
                self.consume(&TT::Control(Ctrl::Colon), "expected ':' after variable id")?;
                let param_type = self.advance();
                // let param_type = if let TT::Identifier(id) = param_type_token.get_token_type() {
                //     span_end =
                //         param_type_token.get_span().offset() + param_type_token.get_span().len();
                //     self.advance();
                //     match id.clone().as_str() {
                //         "Int" => Some(FerryType::Num),
                //         "String" => Some(FerryType::String),
                //         _ => None,
                //     }
                // } else {
                //     return Err(FerryParseError::UnexpectedToken {
                //         msg: "Expected type identifier".into(),
                //         span: *param_type_token.get_span(),
                //     });
                // };

                ret.push(Expr::Binding(Binding {
                    token: self.previous(),
                    name: param_id,
                    assigned_type: Some(param_type),
                    // assigned_type_token: param_id_token,
                    expr_type: FerryTyping::Untyped,
                    value: None,
                    // value_token: None,
                    span: (span_start, span_end - span_start).into(),
                }));

                if self.peek().get_token_type() != &TT::Control(Ctrl::RightParen) {
                    self.consume(&TT::Control(Ctrl::Comma), "expected ',' in params list")?;
                }
            }
            Some(ret)
        };

        self.consume(&TT::Control(Ctrl::RightParen), "expected ')' after '('")?;
        let return_type = if self.peek().get_token_type() == &TT::Control(Ctrl::RightArrow) {
            self.consume(
                &TT::Control(Ctrl::RightArrow),
                "expected '->' after fn definition",
            )?;
            if let TT::Identifier(id) = self.advance().get_token_type() {
                match id.clone().as_str() {
                    "Int" => Some(FerryType::Int),
                    "String" => Some(FerryType::String),
                    "Function" => Some(FerryType::Function),
                    _ => None,
                }
            } else {
                return Err(FerryParseError::UnexpectedToken {
                    msg: format!(
                        "Expected function return type identifier, found {}",
                        self.previous().get_token_type()
                    ),
                    span: *self.previous().get_span(),
                });
            }
        } else {
            None
        };

        self.consume(
            &TT::Control(Ctrl::Colon),
            "expected ':' after function header",
        )?;

        let contents = Box::new(self.start(state)?);

        Ok(Expr::Function(Function {
            token,
            name,
            args,
            contents,
            return_type,
            expr_type: FerryTyping::Untyped,
        }))
    }

    // export as <ID>:
    // def fn fn1()
    // def fn fn2()
    // def fn fn3()
    fn module(&mut self, state: &mut State) -> FerryResult<Expr> {
        let token = self.previous();
        self.consume(
            &TT::Keyword(Kwd::As),
            "expected 'as' after 'export' keyword",
        )?;

        let Some(name) = self.advance().get_id() else {
            return Err(FerryParseError::UnexpectedToken {
                msg: format!(
                    "Expected module identifier, found {}",
                    self.previous().get_token_type()
                ),
                span: *self.previous().get_span(),
            });
        };
        self.consume(
            &TT::Control(Ctrl::Colon),
            "expected ':' after module identifier",
        )?;

        let mut functions = vec![];
        while let Ok(Expr::Function(function)) = self.start(state) {
            functions.push(function);

            if self.peek().get_token_type() == &TT::End {
                break;
            }
        }

        Ok(Expr::Module(Module {
            name,
            token,
            functions,
        }))
    }

    fn import(&mut self, state: &mut State) -> FerryResult<Expr> {
        let token = self.previous();

        let Some(name) = self.advance().get_id() else {
            return Err(FerryParseError::UnexpectedToken {
                msg: format!(
                    "Expected name of module to import, found {}",
                    self.previous().get_token_type()
                ),
                span: *self.previous().get_span(),
            });
        };

        let module = if std::path::Path::exists(std::path::Path::new(&format!("{name}.feri"))) {
            std::fs::read_to_string(format!("{name}.feri")).expect("couldn't find module")
        } else if std::path::Path::exists(std::path::Path::new(&format!("examples/{name}.feri"))) {
            std::fs::read_to_string(format!("examples/{name}.feri")).expect("couldn't find module")
        } else if std::path::Path::exists(std::path::Path::new(&format!("lib/{name}.feri"))) {
            std::fs::read_to_string(format!("lib/{name}.feri")).expect("couldn't find module")
        } else {
            return Err(FerryParseError::UnexpectedToken {
                msg: "Invalid path for module".into(),
                span: *self.previous().get_span(),
            });
        };

        let mut lexer = crate::lexer::Lexer::new(module.as_bytes());
        let lexed_module = lexer
            .lex(state)
            .map_err(|err_list| FerryLexErrors {
                source_code: module.clone(),
                related: err_list,
            })
            .or_else(|errs| {
                eprintln!("{errs}");
                return Err(FerryParseError::ModuleParseFailure {
                    msg: format!("Failed to parse module"),
                    span: *token.get_span(),
                });
            })?;
        let mut parser = Parser::new(lexed_module);
        let module_parse = parser
            .parse(state)
            .map_err(|err_list| FerryParseErrors {
                source_code: module,
                related: err_list,
            })
            .or_else(|errs| {
                eprintln!("{errs}");
                return Err(FerryParseError::ModuleParseFailure {
                    msg: format!("Failed to parse module"),
                    span: *token.get_span(),
                });
            })?;

        let mut functions = vec![];

        if let Some(Expr::Module(module)) = module_parse.first() {
            for function in module.functions.clone() {
                functions.push(function.clone());
            }
        } else {
            return Err(FerryParseError::UnexpectedToken {
                msg: format!("Could not find module {name} in scope"),
                span: *self.previous().get_span(),
            });
        }

        Ok(Expr::Import(Import {
            name,
            token,
            functions,
        }))
    }

    fn s_expression(&mut self, state: &mut State) -> FerryResult<Expr> {
        let expr = self.assignment(state)?;

        Ok(expr)
    }

    fn list(&mut self, state: &mut State) -> FerryResult<Expr> {
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

    fn assignment(&mut self, state: &mut State) -> FerryResult<Expr> {
        let mut expr = self.cons(state)?;

        if self.matches(&[TT::Operator(Op::Equals)]) {
            let operator = self.previous();
            let value = self.start(state)?;
            if let Expr::Variable(v) = &expr {
                expr = Expr::Assign(Assign {
                    var: Box::new(expr.clone()),
                    name: v.name.clone(),
                    value: Box::new(value),
                    expr_type: FerryTyping::Untyped,
                    token: operator,
                });
            }
        }

        Ok(expr)
    }

    fn cons(&mut self, state: &mut State) -> FerryResult<Expr> {
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

    fn index(&mut self, state: &mut State) -> FerryResult<Expr> {
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

    fn comparison(&mut self, state: &mut State) -> FerryResult<Expr> {
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
            });
        }

        Ok(expr)
    }

    fn sum(&mut self, state: &mut State) -> FerryResult<Expr> {
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

    fn factor(&mut self, state: &mut State) -> FerryResult<Expr> {
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

    fn unary(&mut self, state: &mut State) -> FerryResult<Expr> {
        let expr = if self.matches(&[TT::Operator(Op::Subtract)]) {
            let operator = self.previous();
            let rhs = Box::new(self.unary(state)?);
            Expr::Unary(Unary {
                operator,
                rhs,
                expr_type: FerryTyping::Untyped,
            })
        } else {
            self.call(state)?
        };

        Ok(expr)
    }

    fn call(&mut self, state: &mut State) -> FerryResult<Expr> {
        let mut expr = self.target(state)?;

        loop {
            if self.matches(&[TT::Control(Ctrl::LeftParen)]) {
                expr = self.call_function(expr, state)?;
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn target(&mut self, state: &mut State) -> FerryResult<Expr> {
        self.advance();

        match self.previous().get_token_type() {
            TT::Value(l) => Ok(match l {
                TLit::Num(n) => Expr::Literal(SLit::Integer {
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
                TLit::Range(start, end) => {
                    let mut contents = Vec::new();
                    for i in *start..=*end {
                        contents.push(Expr::Literal(SLit::Integer {
                            token: self.previous().clone(),
                            value: i,
                            expr_type: FerryTyping::Untyped,
                            span: *self.previous().get_span(),
                        }));
                    }
                    Expr::Literal(SLit::List {
                        token: self.previous().clone(),
                        contents,
                        expr_type: FerryTyping::Untyped,
                        span: *self.previous().get_span(),
                    })
                } // _ => unreachable!(),
            }),
            // TT::Control(Ctrl::Newline) => {
            //     self.advance();
            //     // self.consume_newline()?;
            //     // self.start(state)
            //     // Err(FerryParseError::UnexpectedEndOfLine {
            //     //     msg: format!("Encountered unexpected end of line"),
            //     //     span: *self.previous().get_span(),
            //     // })
            // }
            TT::Identifier(id) => {
                if self.peek().get_token_type() == &TT::Control(Ctrl::LeftBracket) {
                    let lhs = Box::new(Expr::Variable(Variable {
                        token: self.previous().clone(),
                        name: id.clone(),
                        assigned_type: None,
                        expr_type: FerryTyping::Untyped,
                    }));
                    let operator = Token::new(TT::Operator(Op::GetI), *self.peek().get_span());
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
                        assigned_type: None,
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

            // TT::Comment(_) => {}
            // TT::Control(Ctrl::Newline) => Err(FerryParseError::UnexpectedEndOfLine {
            //     msg: format!("Encountered unexpected end of line"),
            //     span: *self.previous().get_span(),
            // })
            _ => Err(FerryParseError::UnexpectedToken {
                msg: format!("Unexpected token: {}", self.previous().get_token_type()),
                span: *self.previous().get_span(),
            }),
        }
    }

    // helper functions
    fn peek(&self) -> Token {
        self.tokens[self.current].clone()
    }

    fn advance(&mut self) -> Token {
        if !self.end_of_program() {
            self.current += 1;
        }
        self.previous()
    }

    fn previous(&self) -> Token {
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
    ) -> Result<Token, FerryParseError> {
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

    fn synchronize(&mut self) {
        self.advance();

        while !self.end_of_program() {
            if self.previous().get_token_type() == &TT::Control(Ctrl::Newline) {
                return;
            }

            match self.peek().get_token_type() {
                TT::Keyword(kwd) => match kwd {
                    Kwd::If
                    | Kwd::Let
                    | Kwd::Do
                    | Kwd::While
                    | Kwd::For
                    | Kwd::Def
                    | Kwd::Import
                    | Kwd::Export => return,
                    _ => {}
                },
                _ => {}
            };

            self.advance();
        }
    }

    fn end_of_program(&self) -> bool {
        self.tokens[self.current].get_token_type() == &TT::End
    }

    // fn consume_newline(&mut self) -> Result<Option<Token>, FerryParseError> {
    //     match self.consume(&TT::Control(Ctrl::Newline), "invalid state") {
    //         Ok(t) => Ok(Some(t)),
    //         Err(_) => Ok(None),
    //     }
    // }

    fn finish_sequence(
        &mut self,
        token: Token,
        state: &mut State,
        mut contents: Vec<Expr>,
    ) -> FerryResult<Expr> {
        while self.peek().get_token_type() != &TT::Control(Ctrl::RightBracket) {
            let next = self.start(state)?;
            contents.push(next);
            if self.peek().get_token_type() != &TT::Control(Ctrl::RightBracket)
                && self.peek().get_token_type() == &TT::Control(Ctrl::Comma)
            {
                self.consume(
                    &TT::Control(Ctrl::Comma),
                    "expected ',' during multivalue lists",
                )?;
            } else if self.peek().get_token_type() != &TT::Control(Ctrl::Comma) {
                return Err(FerryParseError::UnexpectedToken {
                    msg: "Expected ']' after '[' in list decl".into(),
                    span: *self.peek().get_span(),
                });
            }
        }
        let right_bracket = self.consume(
            &TT::Control(Ctrl::RightBracket),
            "expected right bracket after list",
        )?;
        let offset = self.previous().get_span().offset();
        let span = (offset, right_bracket.get_span().offset() - offset).into();
        Ok(Expr::Literal(SLit::List {
            token,
            contents,
            expr_type: FerryTyping::Untyped,
            span,
        }))
    }

    fn call_function(&mut self, expr: Expr, state: &mut State) -> FerryResult<Expr> {
        let mut args = Vec::new();
        let name = expr.get_token().get_id().unwrap_or_default();

        // self.consume(
        //     &TT::Control(Ctrl::LeftParen),
        //     "expected '(' after function identifier",
        // )?;

        if !self.check(&TT::Control(Ctrl::RightParen)) {
            loop {
                args.push(self.assignment(state)?);
                if !self.matches(&[TT::Control(Ctrl::Comma)]) {
                    break;
                }
            }
        }

        self.consume(
            &TT::Control(Ctrl::RightParen),
            "expected ')' after '(' in function call",
        )?;

        Ok(Expr::Call(Call {
            invoker: Box::new(expr.clone()),
            name,
            token: expr.get_token().clone(),
            args,
            expr_type: FerryTyping::Untyped,
        }))
    }
}

trait MatchToken {
    fn matches(&self, token: &Token) -> bool;
}

impl MatchToken for TT {
    fn matches(&self, token: &Token) -> bool {
        token.get_token_type() == self
    }
}

impl<F> MatchToken for F
where
    F: Fn(&Token) -> bool,
{
    fn matches(&self, token: &Token) -> bool {
        self(token)
    }
}

// pratt parser stuff
