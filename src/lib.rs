use lexer::FerryLexError;
use miette::{Diagnostic, Result, SourceSpan};
use parser::{FerryParseError, FerryParser};
use thiserror::Error;

mod interpreter;
mod lexer;
mod parser;
mod state;
mod syntax;
mod token;

use crate::interpreter::FerryInterpreter;
use crate::lexer::FerryLexer;
use crate::state::FerryState;
use crate::token::FerryToken;

// 'source lifetime: entire length of code compilation / execution
pub struct Ferry<'source> {
    source_code: &'source [u8],
    tokens: Vec<FerryToken>,
    state: FerryState,
}

impl<'source> Ferry<'source> {
    pub fn new(source_code: &'source [u8]) -> Self {
        Self {
            source_code,
            tokens: Vec::new(),
            state: FerryState::new(),
        }
    }

    pub fn run(&mut self) -> Result<String> {
        let mut result = String::new();
        let mut ferry_lexer = FerryLexer::new(self.source_code);

        self.tokens = ferry_lexer.lex().map_err(|err_list| FerryLexErrors {
            source_code: String::from_utf8(self.source_code.to_vec()).unwrap(),
            related: err_list,
        })?;

        let mut ferry_parser = FerryParser::new(self.tokens.clone());

        let ast = ferry_parser.parse().map_err(|err_list| FerryParseErrors {
            source_code: String::from_utf8(self.source_code.to_vec()).unwrap(),
            related: err_list,
        })?;
        self.state = ferry_parser.state.clone();

        let mut interpreter = FerryInterpreter::new(ast);
        result = format!("{:?}", interpreter.interpret(&mut self.state));

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
