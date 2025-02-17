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
    AttrMissing(String),
    DuplicateAttr(String),
    DuplicateFunc(String),
    InvalidArgumentsLength(usize, usize),
    InvalidIndex(Type, Type),
    InvalidSelfPlacement,
    InvalidStructAttr(String),
    MethodNotFound(Type, String),
    MissingElseClause,
    NotFound(String),
    NonIndexableType(Type),
    SyntaxError(TokenKind),
    TypeAnnotationsNeeded,
    TypeMismatch(Type, Type),
    UnexpectedToken(TokenKind, TokenKind),
    UnhashableMapKey(Type),
}

impl fmt::Display for ParserErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ParserErrorKind::AttrMissing(name) => {
                write!(f, "missing required attribute: `{name}`")
            }
            ParserErrorKind::DuplicateAttr(name) => {
                write!(f, "attribute already defined: `{name}`")
            }
            ParserErrorKind::DuplicateFunc(name) => {
                write!(f, "function already defined: `{name}`")
            }
            ParserErrorKind::InvalidArgumentsLength(n1, n2) => {
                write!(
                    f,
                    "invalid number of arguments. expected: `{n1}`, got: `{n2}`"
                )
            }
            ParserErrorKind::InvalidIndex(t, idx_t) => {
                write!(f, "the type `{t}` cannot be indexed by `{idx_t}`")
            }
            ParserErrorKind::InvalidSelfPlacement => {
                write!(
                    f,
                    "`self` is reserved for the first argument in a method argument list"
                )
            }
            ParserErrorKind::InvalidStructAttr(name) => {
                write!(f, "invalid struct attribute: `{name}`")
            }
            ParserErrorKind::MethodNotFound(t, name) => {
                write!(f, "method not found: `{t}.{name}()`")
            }
            ParserErrorKind::MissingElseClause => {
                write!(f, "`if` may be missing an `else` clause")
            }
            ParserErrorKind::NonIndexableType(t) => {
                write!(f, "type `{t}` is not indexable")
            }
            ParserErrorKind::NotFound(name) => {
                write!(f, "not found: `{name}`")
            }
            ParserErrorKind::SyntaxError(token) => {
                write!(f, "syntax error: `{token}`")
            }
            ParserErrorKind::TypeAnnotationsNeeded => {
                write!(f, "type annotations needed")
            }
            ParserErrorKind::TypeMismatch(t1, t2) => {
                write!(f, "type mismatch. expected: `{t1}`, got: `{t2}`")
            }
            ParserErrorKind::UnexpectedToken(t1, t2) => {
                write!(f, "unexpected token: `{t1}`, expected: `{t2}`")
            }
            ParserErrorKind::UnhashableMapKey(t) => {
                write!(f, "map key not hashable: type `{t}`")
            }
        }
    }
}
