use std::fmt;

use crate::Token;

#[derive(Clone, Debug)]
pub struct ParserError {
    pub kind: ParserErrorKind,
    pub line: usize,
    pub col: usize,
}

impl ParserError {
    pub fn new(kind: ParserErrorKind, line: usize, col: usize) -> Self {
        ParserError { kind, line, col }
    }
}

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{} {}", self.line, self.col, self.kind)
    }
}

#[derive(Clone, Debug)]
pub enum ParserErrorKind {
    SyntaxError(Token),
    UnexpectedToken { expected: Token, got: Token },
}

impl fmt::Display for ParserErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ParserErrorKind::SyntaxError(token) => write!(f, "syntax error: '{token}'"),
            ParserErrorKind::UnexpectedToken { expected, got } => {
                write!(f, "unexpected token: '{got}', expected='{expected}'")
            }
        }
    }
}
