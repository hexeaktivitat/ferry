use miette::Diagnostic;
use thiserror::Error;

use crate::{lexer::FerryLexError, parser::FerryParseError, typecheck::FerryTypeError};

#[derive(Error, Debug, Diagnostic)]
#[error("Encountered syntax errors")]
#[diagnostic()]
pub struct FerryLexErrors {
    #[source_code]
    pub source_code: String,
    #[related]
    pub related: Vec<FerryLexError>,
}

#[derive(Error, Debug, Diagnostic)]
#[error("Encountered parsing errors")]
#[diagnostic()]
pub struct FerryParseErrors {
    #[source_code]
    pub source_code: String,
    #[related]
    pub related: Vec<FerryParseError>,
}

#[derive(Error, Debug, Diagnostic)]
#[error("Encountered typecheck errors")]
#[diagnostic()]
pub struct FerryTypeErrors {
    #[source_code]
    pub source_code: String,
    #[related]
    pub related: Vec<FerryTypeError>,
}
