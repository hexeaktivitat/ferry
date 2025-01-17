use miette::SourceSpan;

/// `Token`
///
/// Data for a lexed token.
#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    token_type: TokenType,
    span: SourceSpan,
}

impl Token {
    pub fn new(token_type: TokenType, span: SourceSpan) -> Self {
        Self { token_type, span }
    }

    pub fn get_token_type(&self) -> &TokenType {
        &self.token_type
    }

    pub fn get_span(&self) -> &SourceSpan {
        &self.span
    }

    pub fn get_id(&self) -> Option<String> {
        if let TokenType::Identifier(id) = &self.token_type {
            Some(id.clone())
        } else {
            None
        }
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
    Comment(String),
    End,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Val {
    Num(i64),
    String(String),
    Boolean(bool),
    Range(i64, i64),
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
    GetI,
    Cons,
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
    Comma,
    RightArrow,
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
    In,
    Def,
    Fn,
    Return,
    From,
    Import,
    Export,
    As,
}

impl std::fmt::Display for Token {
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
                Val::Range(s, e) => write!(f, "Value<Range> {s}..{e}"),
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
                Op::GetI => write!(f, "Operator<GetI>"),
                Op::Cons => write!(f, "Operator<Cons>"),
            },
            TokenType::Control(c) => match c {
                Ctrl::Semicolon => write!(f, "Control<Semicolon>"),
                Ctrl::Colon => write!(f, "Control<Colon>"),
                Ctrl::LeftParen => write!(f, "Control<LeftParen>"),
                Ctrl::RightParen => write!(f, "Control<RightParen>"),
                Ctrl::Newline => write!(f, "Control<Newline>"),
                Ctrl::LeftBracket => write!(f, "Control<LeftBracket>"),
                Ctrl::RightBracket => write!(f, "Control<RightBracket>"),
                Ctrl::Comma => write!(f, "Control<Comma>"),
                Ctrl::RightArrow => write!(f, "Control<RightArrow>"),
            },
            TokenType::Keyword(k) => match k {
                Kwd::If => write!(f, "Keyword<If>"),
                Kwd::Then => write!(f, "Keyword<Then>"),
                Kwd::Else => write!(f, "Keyword<Else>"),
                Kwd::Let => write!(f, "Keyword<Let>"),
                Kwd::Do => write!(f, "Keyword<Do>"),
                Kwd::While => write!(f, "Keyword<While>"),
                Kwd::For => write!(f, "Keyword<For>"),
                Kwd::In => write!(f, "Keyword<In>"),
                Kwd::Def => write!(f, "Keyword<Def>"),
                Kwd::Fn => write!(f, "Keyword<Fn>"),
                Kwd::Return => write!(f, "Keyword<Return>"),
                Kwd::From => write!(f, "Keyword<From>"),
                Kwd::Import => write!(f, "Keyword<Import>"),
                Kwd::Export => write!(f, "Keyword<Export>"),
                Kwd::As => write!(f, "Keyword<As>"),
            },
            TokenType::Identifier(i) => write!(f, "Identifier {i}"),
            TokenType::End => write!(f, "END"),
            TokenType::Comment(c) => write!(f, "Comment: {c}"),
        }
    }
}
