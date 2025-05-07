use std::path::PathBuf;

use miette::{Error, SourceSpan, miette};

use crate::lexer::token::Token;

use super::types::FerryType;

#[derive(Debug, Clone)]
pub(crate) struct Symbol {
    pub(crate) identifier: String,
    pub(crate) source_file: PathBuf,
    pub(crate) span: SourceSpan,
    pub(crate) declared: bool,
    pub(crate) initialized: bool,
    pub(crate) use_count: u32,
    pub(crate) symbol_type: Option<SymbolType>,
    pub(crate) expr_type: Option<FerryType>,
}

#[derive(Debug, Copy, Clone)]
pub(crate) enum SymbolType {
    Variable,
    Function,
    Type,
}

impl Symbol {
    pub(crate) fn new(identifier: String, source_file: PathBuf, span: SourceSpan) -> Self {
        Self {
            identifier,
            source_file,
            span,
            declared: false,
            initialized: false,
            use_count: 0,
            symbol_type: None,
            expr_type: None,
        }
    }

    pub(crate) fn declare(&mut self) -> Result<(), Error> {
        if self.declared {
            return Err(miette!(format!(
                "symbol {} has already been declared",
                self.identifier
            )));
        }

        self.declared = true;

        Ok(())
    }
}
