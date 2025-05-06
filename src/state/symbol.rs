use std::path::PathBuf;

use miette::SourceSpan;

use crate::lexer::token::Token;

use super::types::FerryType;

pub(crate) struct Symbol {
    identifier: String,
    token: Token,
    source_file: PathBuf,
    span: SourceSpan,
    symbol_type: Option<FerryType>,
}

impl Symbol {
    pub(crate) fn new(
        identifier: String,
        token: Token,
        source_file: PathBuf,
        span: SourceSpan,
    ) -> Self {
        Self {
            identifier,
            token,
            source_file,
            span,
            symbol_type: None,
        }
    }
}
