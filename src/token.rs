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

    pub fn get_token_type(&self) -> &TokenType {
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
    Keyword(Kwd),
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
    LessThan,
    GreaterThan,
    Equality,
    LessEqual,
    GreaterEqual,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Ctrl {
    Semicolon,
    Colon,
    LeftParen,
    RightParen,
    Newline,
    LeftBracket,
    RightBracket,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Kwd {
    If,
    Then,
    Else,
    Let,
    Do,
    While,
    For,
}

impl std::fmt::Display for FerryToken {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.token_type)
    }
}

impl std::fmt::Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenType::Value(v) => match v {
                Val::Num(n) => write!(f, "Value<Num> {n}"),
                Val::String(s) => write!(f, "Value<String> {s}"),
                Val::Boolean(b) => write!(f, "Value<Boolean> {b}"),
                Val::None => write!(f, "Value<None>"),
            },
            TokenType::Operator(o) => match o {
                Op::Add => write!(f, "Operator<Add>"),
                Op::Subtract => write!(f, "Operator<Subtract>"),
                Op::Multiply => write!(f, "Operator<Multiply>"),
                Op::Divide => write!(f, "Operator<Divide>"),
                Op::Equals => write!(f, "Operator<Equals>"),
                Op::LessThan => write!(f, "Operator<LessThan>"),
                Op::GreaterThan => write!(f, "Operator<GreaterThan>"),
                Op::Equality => write!(f, "Operator<Equality>"),
                Op::LessEqual => write!(f, "Operator<LessThanOrEqual>"),
                Op::GreaterEqual => write!(f, "Operator<GreaterThanOrEqual"),
            },
            TokenType::Control(c) => match c {
                Ctrl::Semicolon => write!(f, "Control<Semicolon>"),
                Ctrl::Colon => write!(f, "Control<Colon>"),
                Ctrl::LeftParen => write!(f, "Control<LeftParen>"),
                Ctrl::RightParen => write!(f, "Control<RightParen>"),
                Ctrl::Newline => write!(f, "Control<Newline>"),
                Ctrl::LeftBracket => write!(f, "Control<LeftBracket>"),
                Ctrl::RightBracket => write!(f, "Control<RightBracket"),
            },
            TokenType::Keyword(k) => match k {
                Kwd::If => write!(f, "Keyword<If>"),
                Kwd::Then => write!(f, "Keyword<Then>"),
                Kwd::Else => write!(f, "Keyword<Else>"),
                Kwd::Let => write!(f, "Keyword<Let>"),
                Kwd::Do => write!(f, "Keyword<Do>"),
                Kwd::While => write!(f, "Keyword<While>"),
                Kwd::For => write!(f, "Keyword<For>"),
            },
            TokenType::Identifier(i) => write!(f, "Identifier {i}"),
            TokenType::End => write!(f, "END"),
        }
    }
}
