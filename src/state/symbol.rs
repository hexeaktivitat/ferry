use std::{fmt::Write, path::PathBuf};

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
    Module,
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

    pub(crate) fn set_symbol_type(&mut self, symbol_type: SymbolType) {
        self.symbol_type = Some(symbol_type);
    }
}

impl std::fmt::Display for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Symbol '{}':\n Symbol type: {:?}\n Expression type: {:?}\n Declared: {}\n Initialized: {}\n Use count: {}\n\n",
            self.identifier,
            self.symbol_type,
            self.expr_type,
            self.declared,
            self.initialized,
            self.use_count
        )
    }
}

impl std::fmt::Display for SymbolType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SymbolType::Variable => f.write_str("Variable"),
            SymbolType::Function => f.write_str("Function"),
            SymbolType::Type => f.write_str("Type"),
            SymbolType::Module => f.write_str("Module"),
        }
    }
}
