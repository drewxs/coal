use std::fmt;

use crate::{Span, Token};

#[derive(Clone, Debug)]
pub struct ParserError {
    pub kind: ParserErrorKind,
    pub span: Span,
}

impl ParserError {
    pub fn new(kind: ParserErrorKind, span: Span) -> Self {
        ParserError { kind, span }
    }
}

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let ((l1, c1), (l2, c2)) = self.span;
        write!(f, "{l1}:{c1}-{l2}:{c2} {}", self.kind)
    }
}

#[derive(Clone, Debug)]
pub enum ParserErrorKind {
    SyntaxError(Token),
    UnexpectedToken { expected: Token, got: Token },
    TypeAnnotationsNeeded,
}

impl fmt::Display for ParserErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ParserErrorKind::SyntaxError(token) => {
                write!(f, "syntax error: '{token}'")
            }
            ParserErrorKind::UnexpectedToken { expected, got } => {
                write!(f, "unexpected token: '{got}', expected: '{expected}'")
            }
            ParserErrorKind::TypeAnnotationsNeeded => {
                write!(f, "type annotations needed")
            }
        }
    }
}
