use miette::{Diagnostic, Result, SourceSpan};
use thiserror::Error;

pub(crate) mod token;

#[derive(Error, Diagnostic, Debug)]
pub enum FerryLexError {
    #[error("Syntax error: Unknown character")]
    #[diagnostic(code(syntax::unknown_character))]
    UnknownCharacter {
        #[help]
        advice: String,
        #[label("unknown char")]
        bad_char: SourceSpan,
    },
    #[error("Syntax error: Unexpected character")]
    #[diagnostic(code(syntax::unexpected_character))]
    UnexpectedCharacter {
        #[help]
        advice: String,
        #[label("unexpected char")]
        unexpected_character: SourceSpan,
    },
    #[error("Syntax error: Unterminated string")]
    #[diagnostic(code(syntax::unterminated_string))]
    UnterminatedString {
        #[help]
        advice: String,
        #[label("starting quote")]
        start_quote: SourceSpan,
        #[label("end of line")]
        current_pos: SourceSpan,
    },
    #[error("Syntax error: Not a number")]
    #[diagnostic(code(syntax::not_a_number))]
    InvalidNumeric {
        #[help]
        advice: String,
        #[label("not a numeric value")]
        bad_num: SourceSpan,
    },
    #[error("Syntax error: Expected integer, found float (Floats currently unsupported)")]
    #[diagnostic(code(syntax::invalid_integer))]
    InvalidInteger {
        #[help]
        advice: String,
        #[label("floating point")]
        float_num: SourceSpan,
    },
}

use token::{Ctrl, Kwd, Op, Token, TokenType as TT, Val};

type FerryResult<T> = Result<T, FerryLexError>;
type FerryLexResult<T> = Result<Vec<T>, Vec<FerryLexError>>;

#[derive(Debug)]
pub struct Lexer<'source> {
    source: &'source [u8],
    start: usize,
    current: usize,
}

impl<'source> Lexer<'source> {
    pub fn new(source: &'source [u8]) -> Self {
        Self {
            source,
            start: 0,
            current: 0,
        }
    }

    pub fn lex(&mut self) -> FerryLexResult<Token> {
        let mut tokens = Vec::new();
        let mut errors = Vec::new();

        while !self.end_of_code() {
            match self
                .scan_token()
                .and_then(|ot| ot.map(|t| self.make_token(t)).transpose())
            {
                Ok(Some(token)) => tokens.push(token),
                Ok(None) => {}
                Err(e) => errors.push(e),
            }
        }

        let token = self.make_token(TT::End).unwrap();
        tokens.push(token);

        if errors.is_empty() {
            Ok(tokens)
        } else {
            Err(errors)
        }
    }

    fn scan_token(&mut self) -> FerryResult<Option<TT>> {
        self.start = self.current;

        match self.advance() {
            // CONTROL CHARACTERS
            b';' => Ok(Some(TT::Control(Ctrl::Semicolon))),
            b':' => Ok(Some(TT::Control(Ctrl::Colon))),
            b'(' => Ok(Some(TT::Control(Ctrl::LeftParen))),
            b')' => Ok(Some(TT::Control(Ctrl::RightParen))),
            b'[' => Ok(Some(TT::Control(Ctrl::LeftBracket))),
            b']' => Ok(Some(TT::Control(Ctrl::RightBracket))),
            b',' => Ok(Some(TT::Control(Ctrl::Comma))),

            // OPERATORS
            b'+' => Ok(Some(TT::Operator(Op::Add))),
            b'-' => {
                if self.match_next(b'>') {
                    Ok(Some(TT::Control(Ctrl::RightArrow)))
                } else {
                    Ok(Some(TT::Operator(Op::Subtract)))
                }
            }
            b'*' => Ok(Some(TT::Operator(Op::Multiply))),
            b'/' => {
                if self.peek() == b'/' {
                    while self.peek() != b'\n' && !self.end_of_code() {
                        self.current += 1;
                    }
                    let _comment = self.substring(self.start + 2, self.current - 1)?;
                    if self.peek() == b'\n' {
                        self.current += 1;
                    }
                    Ok(None)
                } else {
                    Ok(Some(TT::Operator(Op::Divide)))
                }
            }
            b'=' => {
                if self.match_next(b'=') {
                    Ok(Some(TT::Operator(Op::Equality)))
                } else {
                    Ok(Some(TT::Operator(Op::Equals)))
                }
            }
            b'<' => {
                if self.match_next(b'=') {
                    Ok(Some(TT::Operator(Op::LessEqual)))
                } else {
                    Ok(Some(TT::Operator(Op::LessThan)))
                }
            }
            b'>' => {
                if self.match_next(b'=') {
                    Ok(Some(TT::Operator(Op::GreaterEqual)))
                } else {
                    Ok(Some(TT::Operator(Op::GreaterThan)))
                }
            }

            // STRING LITERAL
            b'"' => self.string().map(Some),

            // NUMERIC LITERAL
            c if c.is_ascii_digit() => self.number().map(Some),

            // KEYWORDS AND IDENTIFIERS
            c @ b'_' | c if c.is_ascii_alphabetic() => {
                self.identifier().map(|id| match id.as_str() {
                    // keywords
                    "if" => Some(TT::Keyword(Kwd::If)),
                    "then" => Some(TT::Keyword(Kwd::Then)),
                    "else" => Some(TT::Keyword(Kwd::Else)),
                    "let" => Some(TT::Keyword(Kwd::Let)),
                    "do" => Some(TT::Keyword(Kwd::Do)),
                    "while" => Some(TT::Keyword(Kwd::While)),
                    "for" => Some(TT::Keyword(Kwd::For)),
                    "in" => Some(TT::Keyword(Kwd::In)),
                    "def" => Some(TT::Keyword(Kwd::Def)),
                    "fn" => Some(TT::Keyword(Kwd::Fn)),
                    "return" => Some(TT::Keyword(Kwd::Return)),
                    "from" => Some(TT::Keyword(Kwd::From)),
                    "import" => Some(TT::Keyword(Kwd::Import)),
                    "export" => Some(TT::Keyword(Kwd::Export)),
                    "as" => Some(TT::Keyword(Kwd::As)),

                    // reserved boolean keywords
                    "true" => Some(TT::Value(Val::Boolean(true))),
                    "false" => Some(TT::Value(Val::Boolean(false))),

                    // list operators
                    "geti" => Some(TT::Operator(Op::GetI)),
                    "cons" => Some(TT::Operator(Op::Cons)),
                    _ => Some(TT::Identifier(id)),
                })
            }

            // SIGNIFICANT WHITESPACE
            // b'\n' => Ok(Some(TT::Control(Ctrl::Newline))),
            b'\n' => Ok(None),

            // NON-SIGNIFICANT WHITESPACE
            b' ' | b'\r' | b'\t' => Ok(None),
            c => Err(FerryLexError::UnknownCharacter {
                advice: format!("Encountered an unknown character: '{}'", c as char),
                bad_char: (self.start, self.current - self.start).into(),
            }),
        }
    }

    /// creates the token and associates it with a span of the source code
    fn make_token(&self, token_type: TT) -> FerryResult<Token> {
        let span = (self.start, self.current - self.start).into();
        Ok(Token::new(token_type, span))
    }

    /// returns the next byte of source code and advances the internal counter
    fn advance(&mut self) -> u8 {
        let c = self.source[self.current];
        self.current += 1;
        c
    }

    /// EOF test
    fn end_of_code(&self) -> bool {
        self.current >= self.source.len()
    }

    /// lookahead(1)
    fn peek(&self) -> u8 {
        *self.source.get(self.current).unwrap_or(&b'\0')
    }

    /// lookahead(2)
    fn peek_next(&self) -> u8 {
        *self.source.get(self.current + 1).unwrap_or(&b'\0')
    }

    /// ignores matching expected values
    fn match_next(&mut self, expected: u8) -> bool {
        if self.end_of_code() || self.source[self.current] != expected {
            false
        } else {
            self.current += 1;
            true
        }
    }

    /// creates a string value from the bytestream
    fn string(&mut self) -> FerryResult<TT> {
        while self.peek() != b'"' && !self.end_of_code() && self.peek() != b'\n' {
            self.current += 1;
        }
        if self.end_of_code() {
            return Err(FerryLexError::UnterminatedString {
                advice: "Expected end of string, found end of file".into(),
                start_quote: (self.start, 1).into(),
                current_pos: (self.current, 1).into(),
            });
        } else if self.peek() == b'\n' {
            return Err(FerryLexError::UnterminatedString {
                advice: "Expected end of string, found newline".into(),
                start_quote: (self.start, 1).into(),
                current_pos: (self.current, 1).into(),
            });
        } else {
            self.advance();

            Ok(TT::Value(Val::String(
                self.substring(self.start + 1, self.current - 1)?,
            )))
        }
    }

    /// given a start and ending position, parses a given slice of the bytestream
    /// into a string literal
    fn substring(&self, start: usize, end: usize) -> FerryResult<String> {
        String::from_utf8(self.source[start..end].to_vec()).map_err(|_source| {
            FerryLexError::UnexpectedCharacter {
                advice: "character unrecognized in this context".into(),
                unexpected_character: (self.start, self.current - self.start).into(),
            }
        })
    }

    /// parses numerical values from the bytestream
    fn number(&mut self) -> FerryResult<TT> {
        while self.peek().is_ascii_digit() && !self.end_of_code() {
            self.advance();
        }

        if self.peek().is_ascii_alphabetic() {
            return Err(FerryLexError::InvalidNumeric {
                advice: "Not a valid numeric value".into(),
                bad_num: (self.current, 1).into(),
            });
        }

        if self.peek() == b'.' && self.peek_next().is_ascii_digit() {
            let current_pos = self.current;
            self.advance();
            while self.peek().is_ascii_digit() {
                self.advance();
            }
            return Err(FerryLexError::InvalidInteger {
                advice: "Integer values cannot be floats".into(),
                float_num: (current_pos, 1).into(),
            });
        } else if self.peek() == b'.' && self.peek_next() == b'.' {
            // consume the .. token
            let start = self
                .substring(self.start, self.current)?
                .parse::<i64>()
                .expect("should have been i64");
            self.advance();
            self.advance();
            let end_start = self.current;
            while self.peek().is_ascii_digit() {
                self.advance();
            }
            let end = self
                .substring(end_start, self.current)?
                .parse::<i64>()
                .expect("should have been i64");
            return Ok(TT::Value(Val::Range(start, end)));
        }

        Ok(TT::Value(Val::Num(
            self.substring(self.start, self.current)?
                .parse::<i64>()
                .expect("that was not an i64? how"),
        )))
    }

    /// used specifically for valid code terms rather than string literals
    /// variable names, function name, class names, etc.
    fn identifier(&mut self) -> FerryResult<String> {
        while self.peek().is_ascii_alphabetic() || self.peek() == b'_' {
            self.advance();
        }

        self.substring(self.start, self.current)
    }
}
