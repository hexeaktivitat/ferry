use ir::{Ir, Opcode};
use miette::{Diagnostic, Result};
use thiserror::Error;

use interpreter::FerryInterpreterError;
use lexer::token::Token;
use lexer::{FerryLexError, Lexer};
use parser::syntax::Expr;
use parser::{FerryParseError, Parser};
use state::{value::Value, State};
use typecheck::{FerryTypeError, Typechecker};
use vm::Vm;

mod interpreter;
mod ir;
mod lexer;
mod parser;
mod state;
mod typecheck;
mod vm;

pub struct Ferry {
    source_code: String,
    tokens: Vec<Token>,
    state: State,
    ast: Vec<Expr>,
    typed_ast: Vec<Expr>,
    ferry_ir: Vec<Opcode>,
    vm: Vm,
}

#[derive(Clone, Copy)]
pub enum PrintReq {
    Tokens,
    State,
    Ast,
    TypedAst,
    Ir,
}

impl Ferry {
    pub fn new(source_code: String) -> Self {
        Self {
            source_code,
            tokens: Vec::new(),
            state: State::new(),
            ast: Vec::new(),
            typed_ast: Vec::new(),
            ferry_ir: Vec::new(),
            vm: Vm::new(),
        }
    }

    pub fn update_source(&mut self, source_code: String) {
        self.source_code = source_code;
    }

    pub fn run(&mut self) -> Result<Value> {
        let mut ferry_lexer = Lexer::new(self.source_code.as_bytes());
        let source_code =
            String::from_utf8(self.source_code.as_bytes().to_vec()).unwrap_or_default();

        let tokens = ferry_lexer.lex().map_err(|err_list| FerryLexErrors {
            source_code: source_code.clone(),
            related: err_list,
        })?;

        self.tokens.clone_from(&tokens);

        let mut ferry_parser = Parser::new(tokens);

        let ast = ferry_parser
            .parse(&mut self.state)
            .map_err(|err_list| FerryParseErrors {
                source_code: source_code.clone(),
                related: err_list,
            })?;

        self.ast.clone_from(&ast);

        let mut typechecker = Typechecker::new();
        let typed_ast = typechecker
            .typecheck(&ast, &mut self.state)
            .map_err(|err_list| FerryTypeErrors {
                source_code: source_code.clone(),
                related: err_list,
            })?;

        self.typed_ast.clone_from(&typed_ast);

        let mut ir = Ir::new();
        let ferry_ir = ir.lower(&typed_ast, &mut self.state)?;
        self.ferry_ir.clone_from(&ferry_ir);

        // self.vm.set_program(self.ferry_ir.clone());
        let result = self.vm.interpret(ferry_ir, &mut self.state)?;

        self.vm.clear();

        Ok(result)
    }

    pub fn print_data(&mut self, req: PrintReq) {
        match req {
            PrintReq::Tokens => {
                println!("\nTOKENS");
                println!("======\n");

                for t in &self.tokens {
                    println!("{t}");
                }
            }
            PrintReq::State => {
                println!("\nSTATE");
                println!("=====\n");

                println!("{}", self.state);
            }
            PrintReq::Ast => {
                println!("\nAST");
                println!("===\n");

                for e in &self.ast {
                    println!("{e}");
                }
            }
            PrintReq::TypedAst => {
                println!("\nTYPED AST");
                println!("======\n");

                for t in &self.typed_ast {
                    println!("{t}");
                }
            }
            PrintReq::Ir => {
                println!("\nLINEAR IR");
                println!("=========\n");

                for i in &self.ferry_ir {
                    println!("{i}");
                }
            }
        }
        println!();
    }
}

// error lists

#[derive(Error, Debug, Diagnostic)]
#[error("Encountered syntax errors")]
#[diagnostic()]
struct FerryLexErrors {
    #[source_code]
    source_code: String,
    #[related]
    related: Vec<FerryLexError>,
}

#[derive(Error, Debug, Diagnostic)]
#[error("Encountered parsing errors")]
#[diagnostic()]
struct FerryParseErrors {
    #[source_code]
    source_code: String,
    #[related]
    related: Vec<FerryParseError>,
}

#[derive(Error, Debug, Diagnostic)]
#[error("Encountered typecheck errors")]
#[diagnostic()]
struct FerryTypeErrors {
    #[source_code]
    source_code: String,
    #[related]
    related: Vec<FerryTypeError>,
}

#[expect(unused)]
#[derive(Error, Debug, Diagnostic)]
#[error("Encountered interpreter errors")]
#[diagnostic()]
struct FerryInterpreterErrors {
    #[source_code]
    source_code: String,
    #[related]
    related: Vec<FerryInterpreterError>,
}
