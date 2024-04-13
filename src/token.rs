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
    Value(Val),
    Operator(Op),
    Control(Ctrl),
    Keyword,
    Identifier(String),
    End,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Val {
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
    Equals,
    RightArrow,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Ctrl {
    Semicolon,
}
