use interpreter::FerryInterpreterError;
use lexer::FerryLexError;
use miette::{Diagnostic, Result};
use parser::{FerryParseError, FerryParser};
use riscv::{FerryRiscVAssembler, Instruction};
use state::FerryValue;
use syntax::Expr;
use thiserror::Error;
use typecheck::{FerryTypeError, FerryTypechecker};

mod interpreter;
mod lexer;
mod parser;
mod riscv;
mod state;
mod syntax;
mod token;
mod typecheck;
mod types;

use crate::interpreter::FerryInterpreter;
use crate::lexer::FerryLexer;
use crate::state::FerryState;
use crate::token::FerryToken;

pub struct Ferry {
    source_code: String,
    tokens: Vec<FerryToken>,
    state: FerryState,
    ast: Vec<Expr>,
    typed_ast: Vec<Expr>,
    riscv_asm: Vec<Instruction>,
}

pub enum PrintReq {
    Tokens,
    State,
    Ast,
    TypedAst,
    Asm,
}

impl<'source> Ferry {
    pub fn new(source_code: String) -> Self {
        Self {
            source_code,
            tokens: Vec::new(),
            state: FerryState::new(),
            ast: Vec::new(),
            typed_ast: Vec::new(),
            riscv_asm: Vec::new(),
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

        let mut interpreter = FerryInterpreter::new(self.typed_ast.clone());
        let result = match interpreter.interpret(&mut self.state).map_err(|err_list| {
            FerryInterpreterErrors {
                source_code: String::from_utf8(self.source_code.as_bytes().to_vec()).unwrap(),
                related: err_list,
            }
        })? {
            Some(r) => r,
            None => FerryValue::Unit,
        };

        // let mut assembler = FerryRiscVAssembler::new();
        // self.riscv_asm = assembler
        //     .assemble(self.typed_ast.clone(), &mut self.state)
        //     .unwrap();

        // println!("\n\nSTATE");
        // println!("=====");

        // println!("\n{}", self.state);

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
                println!("\nRISC-V ASM");
                println!("==========\n");

                let mut assembler = FerryRiscVAssembler::new();
                self.riscv_asm = assembler
                    .assemble(self.typed_ast.clone(), &mut self.state)
                    .unwrap();

                for op in &self.riscv_asm {
                    println!("{}", op);
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

#[derive(Error, Debug, Diagnostic)]
#[error("Encountered type errors")]
#[diagnostic()]
struct FerryInterpreterErrors {
    #[source_code]
    source_code: String,
    #[related]
    related: Vec<FerryInterpreterError>,
}
