use miette::SourceSpan;

/// `Token`
///
/// Data for a lexed token.
#[derive(Debug, Clone, PartialEq)]
pub struct FerryToken {
    token_type: TokenType,
    span: SourceSpan,
}

impl FerryToken {
    pub fn new(token_type: TokenType, span: SourceSpan) -> Self {
        Self { token_type, span }
    }

    pub fn get_type(&self) -> &TokenType {
        &self.token_type
    }

    pub fn get_span(&self) -> &SourceSpan {
        &self.span
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    // Literal value
    Value(Literal),
    Operator(Op),
    Control,
    Keyword,
    Identifier(String),
    End,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Num(f64),
    String(String),
    Boolean(bool),
    None,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Op {
    Add,
    Subtract,
    Multiply,
    Divide,
}
