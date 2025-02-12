use std::fmt;

use crate::{Span, TokenKind, Type};

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
    SyntaxError(TokenKind),
    UnexpectedToken(TokenKind, TokenKind),
    NotFound(String),
    MethodNotFound(Type, String),
    TypeAnnotationsNeeded,
    InvalidArgumentsLength(usize, usize),
    TypeMismatch(Type, Type),
    NonIndexableType(Type),
    InvalidIndex(Type, Type),
    MissingElseClause,
    UnhashableMapKey(Type),
    InvalidStructAttr(String),
    AttrMissing(String),
}

impl fmt::Display for ParserErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ParserErrorKind::SyntaxError(token) => {
                write!(f, "syntax error: `{token}`")
            }
            ParserErrorKind::UnexpectedToken(t1, t2) => {
                write!(f, "unexpected token: `{t1}`, expected: `{t2}`")
            }
            ParserErrorKind::NotFound(name) => {
                write!(f, "not found: `{name}`")
            }
            ParserErrorKind::MethodNotFound(t, name) => {
                write!(f, "method not found: `{t}.{name}()`")
            }
            ParserErrorKind::TypeAnnotationsNeeded => {
                write!(f, "type annotations needed")
            }
            ParserErrorKind::InvalidArgumentsLength(n1, n2) => {
                write!(
                    f,
                    "invalid number of arguments. expected: `{n1}`, got: `{n2}`"
                )
            }
            ParserErrorKind::TypeMismatch(t1, t2) => {
                write!(f, "type mismatch. expected: `{t1}`, got: `{t2}`")
            }
            ParserErrorKind::NonIndexableType(t) => {
                write!(f, "type `{t}` is not indexable")
            }
            ParserErrorKind::InvalidIndex(t, idx_t) => {
                write!(f, "the type `{t}` cannot be indexed by `{idx_t}`")
            }
            ParserErrorKind::MissingElseClause => {
                write!(f, "`if` may be missing an `else` clause")
            }
            ParserErrorKind::UnhashableMapKey(t) => {
                write!(f, "map key not hashable: type `{t}`")
            }
            ParserErrorKind::InvalidStructAttr(name) => {
                write!(f, "invalid struct attribute: `{name}`")
            }
            ParserErrorKind::AttrMissing(name) => {
                write!(f, "missing required attribute: `{name}`")
            }
        }
    }
}
