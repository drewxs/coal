use std::fmt;

use crate::{Span, Token, Type};

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
    UnexpectedToken(Token, Token),
    NotFound(String),
    MethodNotFound(Type, String),
    TypeAnnotationsNeeded,
    InvalidArgumentsLength(usize, usize),
    TypeMismatch(Type, Type),
}

impl fmt::Display for ParserErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ParserErrorKind::SyntaxError(token) => {
                write!(f, "syntax error: '{token}'")
            }
            ParserErrorKind::UnexpectedToken(t1, t2) => {
                write!(f, "unexpected token: '{t1}', expected: '{t2}'")
            }
            ParserErrorKind::NotFound(name) => {
                write!(f, "not found: {name}")
            }
            ParserErrorKind::MethodNotFound(t, name) => {
                write!(f, "method not found: {t}.{name}()")
            }
            ParserErrorKind::TypeAnnotationsNeeded => {
                write!(f, "type annotations needed")
            }
            ParserErrorKind::InvalidArgumentsLength(t1, t2) => {
                write!(f, "invalid number of arguments: expected={t1}, got={t2}")
            }
            ParserErrorKind::TypeMismatch(t1, t2) => {
                write!(f, "type mismatch: expected={t1}, got={t2}")
            }
        }
    }
}
