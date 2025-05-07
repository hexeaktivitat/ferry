use std::path::PathBuf;

use miette::{Error, SourceSpan, miette};

use crate::lexer::token::Token;

use super::types::FerryType;

#[derive(Debug, Clone)]
pub(crate) struct Symbol {
    pub(crate) identifier: String,
    pub(crate) source_file: PathBuf,
    pub(crate) span: SourceSpan,
    pub(crate) initialized: bool,
    pub(crate) use_count: u32,
    pub(crate) symbol_type: Option<FerryType>,
}

impl Symbol {
    pub(crate) fn new(
        identifier: String,
        symbol_type: Option<FerryType>,
        source_file: PathBuf,
        span: SourceSpan,
    ) -> Self {
        Self {
            identifier,
            source_file,
            span,
            initialized: false,
            use_count: 0,
            symbol_type,
        }
    }

    pub(crate) fn initialize(&mut self) -> Result<(), Error> {
        if self.initialized {
            return Err(miette!(format!(
                "re-initialization of already initialized symbol {}",
                self.identifier
            )));
        }

        self.initialized = true;

        Ok(())
    }
}
