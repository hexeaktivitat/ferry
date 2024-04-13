use interpreter::FerryInterpreterError;
use lexer::FerryLexError;
use miette::{Diagnostic, Result};
use parser::{FerryParseError, FerryParser};
use riscv::FerryRiscVAssembler;
use state::FerryValue;
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

use crate::interpreter::FerryInterpreter;
use crate::lexer::FerryLexer;
use crate::state::FerryState;
use crate::token::FerryToken;

// 'source lifetime: entire length of code compilation / execution
pub struct Ferry {
    source_code: String,
    tokens: Vec<FerryToken>,
    state: FerryState,
}

impl<'source> Ferry {
    pub fn new(source_code: String) -> Self {
        Self {
            source_code,
            tokens: Vec::new(),
            state: FerryState::new(),
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

        let ast = ferry_parser
            .parse(&mut self.state)
            .map_err(|err_list| FerryParseErrors {
                source_code: String::from_utf8(self.source_code.as_bytes().to_vec()).unwrap(),
                related: err_list,
            })?;

        let mut typechecker = FerryTypechecker::new(ast.clone());
        let typed_ast =
            typechecker
                .typecheck(&mut self.state)
                .map_err(|err_list| FerryTypeErrors {
                    source_code: String::from_utf8(self.source_code.as_bytes().to_vec()).unwrap(),
                    related: err_list,
                })?;

        let mut interpreter = FerryInterpreter::new(typed_ast.clone());
        let result = interpreter.interpret(&mut self.state)?.unwrap();

        let mut assembler = FerryRiscVAssembler::new();
        let asm = assembler
            .assemble(typed_ast.clone(), &mut self.state)
            .unwrap();

        println!("\nRISC-V ASM");
        println!("==========\n");

        for op in asm {
            println!("{}", op);
        }

        println!("\n\nSTATE\n");
        println!("=====");

        println!("\n{}", self.state);

        Ok(result)
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
