use miette::{Diagnostic, Result, SourceSpan};
use thiserror::Error;

#[derive(Error, Diagnostic, Debug)]
pub enum FerryLexError {
    #[error("Syntax error: Unexpected character")]
    #[diagnostic(code(syntax::unexpected_character))]
    UnexpectedCharacter {
        #[help]
        advice: String,
        #[label]
        bad_char: SourceSpan,
    },
    #[error("Syntax error: Unterminated string")]
    #[diagnostic(code(syntax::unterminated_string))]
    UnterminatedString {
        #[help]
        advice: String,
        #[label]
        start_quote: SourceSpan,
    },
    #[error("Syntax error: Not a number")]
    #[diagnostic(code(syntax::not_a_number))]
    NotANumber {
        #[help]
        advice: String,
        #[label]
        bad_num: SourceSpan,
    },
}

use crate::token::{TokenType as TT, *};

type FerryResult<T> = Result<T, FerryLexError>;
type FerryLexResult<T> = Result<Vec<T>, Vec<FerryLexError>>;

#[derive(Debug)]
pub struct FerryLexer<'source> {
    source: &'source [u8],
    start: usize,
    current: usize,
}

impl<'source> FerryLexer<'source> {
    pub fn new(source: &'source [u8]) -> Self {
        Self {
            source,
            start: 0,
            current: 0,
        }
    }

    pub fn lex(&mut self) -> FerryLexResult<FerryToken> {
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

            // OPERATORS
            b'+' => Ok(Some(TT::Operator(Op::Add))),
            b'-' => {
                // if self.peek() == b'>' {
                //     Ok(Some(TT::Operator(Op::RightArrow)))
                // } else {
                Ok(Some(TT::Operator(Op::Subtract)))
                // }
            }
            b'*' => Ok(Some(TT::Operator(Op::Multiply))),
            b'/' => Ok(Some(TT::Operator(Op::Divide))),
            b'=' => {
                if self.peek() == b'=' {
                    Ok(Some(TT::Operator(Op::Equality)))
                } else {
                    Ok(Some(TT::Operator(Op::Equals)))
                }
            }
            b'<' => {
                if self.peek() == b'=' {
                    Ok(Some(TT::Operator(Op::LessEqual)))
                } else {
                    Ok(Some(TT::Operator(Op::LessThan)))
                }
            }
            b'>' => {
                if self.peek() == b'=' {
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

                    // reserved boolean keywords
                    "true" => Some(TT::Value(Val::Boolean(true))),
                    "false" => Some(TT::Value(Val::Boolean(false))),
                    _ => Some(TT::Identifier(id)),
                })
            }

            // SIGNIFICANT WHITESPACE
            b'\n' => Ok(Some(TT::Control(Ctrl::Newline))),
            // b'\n' => Ok(None),

            // NON-SIGNIFICANT WHITESPACE
            b' ' | b'\r' => Ok(None),
            _ => Err(FerryLexError::UnexpectedCharacter {
                advice: "Expected literally anything else".into(),
                bad_char: (self.start, self.current - self.start).into(),
            }),
        }
    }

    /// creates the token and associates it with a span of the source code
    fn make_token(&self, token_type: TT) -> FerryResult<FerryToken> {
        let span = (self.start, self.current - self.start).into();
        Ok(FerryToken::new(token_type, span))
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
        while self.peek() != b'"' && !self.end_of_code() {
            // if self.peek() == b'\n' {
            //     self.line += 1;
            // }
            self.advance();
        }
        if self.end_of_code() {
            return Err(FerryLexError::UnterminatedString {
                advice: "check for a missing quote around intended string".into(),
                start_quote: (self.start, self.current - self.start).into(),
            });
        }

        self.advance();

        Ok(TT::Value(Val::String(
            self.substring(self.start + 1, self.current - 1)?,
        )))
    }

    /// given a start and ending position, parses a given slice of the bytestream
    /// into a string literal
    fn substring(&self, start: usize, end: usize) -> FerryResult<String> {
        String::from_utf8(self.source[start..end].to_vec()).map_err(|_source| {
            FerryLexError::UnexpectedCharacter {
                advice: "character unrecognized in this context".into(),
                bad_char: (self.start, self.current - self.start).into(),
            }
        })
    }

    /// parses numerical values from the bytestream
    fn number(&mut self) -> FerryResult<TT> {
        while self.peek().is_ascii_digit() && !self.end_of_code() {
            self.advance();
        }

        if self.peek().is_ascii_alphabetic() {
            return Err(FerryLexError::NotANumber {
                advice: "not a valid integer value".into(),
                bad_num: (self.start, self.current - self.start).into(),
            });
        }

        if self.peek() == b'.' && self.peek_next().is_ascii_digit() {
            self.advance();
            while self.peek().is_ascii_digit() {
                self.advance();
            }
        }

        Ok(TokenType::Value(Val::Num(
            self.substring(self.start, self.current)?
                .parse::<f64>()
                .expect("that was not an f64? how"),
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
