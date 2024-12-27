use ir::{FerryIr, FerryOpcode};
use miette::{Diagnostic, Result};
use thiserror::Error;

use interpreter::FerryInterpreterError;
use lexer::token::FerryToken;
use lexer::{FerryLexError, FerryLexer};
use parser::syntax::Expr;
use parser::{FerryParseError, FerryParser};
use state::{value::FerryValue, FerryState};
use typecheck::{FerryTypeError, FerryTypechecker};
use vm::FerryVm;

mod interpreter;
mod ir;
mod lexer;
mod parser;
mod state;
mod typecheck;
mod vm;

pub struct Ferry {
    source_code: String,
    tokens: Vec<FerryToken>,
    state: FerryState,
    ast: Vec<Expr>,
    typed_ast: Vec<Expr>,
    ferry_ir: Vec<FerryOpcode>,
    vm: FerryVm,
}

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
            state: FerryState::new(),
            ast: Vec::new(),
            typed_ast: Vec::new(),
            ferry_ir: Vec::new(),
            vm: FerryVm::new(),
        }
    }

    pub fn update_source(&mut self, source_code: String) {
        self.source_code = source_code;
    }

    pub fn run(&mut self) -> Result<FerryValue> {
        let mut ferry_lexer = FerryLexer::new(self.source_code.as_bytes());

        let tokens = ferry_lexer.lex().map_err(|err_list| FerryLexErrors {
            source_code: String::from_utf8(self.source_code.as_bytes().to_vec()).unwrap(),
            related: err_list,
        })?;

        self.tokens = tokens.clone();

        let mut ferry_parser = FerryParser::new(tokens);

        let ast = ferry_parser
            .parse(&mut self.state)
            .map_err(|err_list| FerryParseErrors {
                source_code: String::from_utf8(self.source_code.as_bytes().to_vec()).unwrap(),
                related: err_list,
            })?;

        self.ast = ast.clone();

        let mut typechecker = FerryTypechecker::new(ast);
        let typed_ast =
            typechecker
                .typecheck(&mut self.state)
                .map_err(|err_list| FerryTypeErrors {
                    source_code: String::from_utf8(self.source_code.as_bytes().to_vec()).unwrap(),
                    related: err_list,
                })?;

        self.typed_ast = typed_ast.clone();

        let mut ir = FerryIr::new(typed_ast);
        let ferry_ir = ir.lower(&mut self.state).unwrap();
        self.ferry_ir = ferry_ir.clone();

        // self.vm.set_program(self.ferry_ir.clone());
        let result = self.vm.interpret(ferry_ir, &mut self.state).unwrap();

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
