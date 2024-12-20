use ir::{FerryIr, FerryOpcode};
use miette::{Diagnostic, Result};
use thiserror::Error;

use interpreter::FerryInterpreterError;
use lexer::token::FerryToken;
use lexer::{FerryLexError, FerryLexer};
use parser::syntax::Expr;
use parser::{FerryParseError, FerryParser};
use state::{FerryState, FerryValue};
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
    Asm,
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

        self.tokens = ferry_lexer.lex().map_err(|err_list| FerryLexErrors {
            source_code: String::from_utf8(self.source_code.as_bytes().to_vec()).unwrap(),
            related: err_list,
        })?;

        let mut ferry_parser = FerryParser::new(self.tokens.clone());

        self.ast = ferry_parser
            .parse(&mut self.state)
            .map_err(|err_list| FerryParseErrors {
                source_code: String::from_utf8(self.source_code.as_bytes().to_vec()).unwrap(),
                related: err_list,
            })?;

        let mut typechecker = FerryTypechecker::new(self.ast.clone());
        self.typed_ast =
            typechecker
                .typecheck(&mut self.state)
                .map_err(|err_list| FerryTypeErrors {
                    source_code: String::from_utf8(self.source_code.as_bytes().to_vec()).unwrap(),
                    related: err_list,
                })?;

        // let mut interpreter = FerryInterpreter::new(self.typed_ast.clone());
        // let result = match interpreter.interpret(&mut self.state).map_err(|err_list| {
        //     FerryInterpreterErrors {
        //         source_code: String::from_utf8(self.source_code.as_bytes().to_vec()).unwrap(),
        //         related: err_list,
        //     }
        // })? {
        //     Some(r) => r,
        //     None => FerryValue::Unit,
        // };

        let mut ir = FerryIr::new(self.ast.clone());
        self.ferry_ir = ir.lower(&mut self.state).unwrap();

        // self.vm.set_program(self.ferry_ir.clone());
        let result = self
            .vm
            .interpret(self.ferry_ir.clone(), &mut self.state)
            .unwrap();

        self.vm.clear();

        // self.print_data(PrintReq::TypedAst);

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
                    println!("{}", e);
                }
            }
            PrintReq::TypedAst => {
                println!("\nTYPED AST");
                println!("======\n");

                for t in &self.typed_ast {
                    println!("{}", t);
                }
            }
            PrintReq::Asm => {
                unimplemented!()
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
