use std::fmt;

use thiserror::Error;

use crate::{ResolvedType, Span, TokenKind};

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

#[derive(Error, Clone, Debug)]
pub enum ParserErrorKind {
    #[error("missing required attribute: `{0}`")]
    AttrMissing(String),

    #[error("attribute already defined: `{0}`")]
    DuplicateAttr(String),

    #[error("function already defined: `{0}`")]
    DuplicateFunc(String),

    #[error("invalid number of arguments. expected: `{0}`, got: `{1}`")]
    InvalidArgumentsLength(usize, usize),

    #[error("the type `{0}` cannot be indexed by `{1}`")]
    InvalidIndex(Box<ResolvedType>, Box<ResolvedType>),

    #[error("`self` is reserved for the first argument in a method argument list")]
    InvalidSelfPlacement,

    #[error("invalid struct attribute: `{0}`")]
    InvalidStructAttr(String),

    #[error("method not found: `{0}.{1}()`")]
    MethodNotFound(Box<ResolvedType>, String),

    #[error("`if` may be missing an `else` clause")]
    MissingElseClause,

    #[error("not found: `{0}`")]
    NotFound(String),

    #[error("type `{0}` is not indexable")]
    NonIndexableType(Box<ResolvedType>),

    #[error("syntax error: `{0}`")]
    SyntaxError(TokenKind),

    #[error("type annotations needed")]
    TypeAnnotationsNeeded,

    #[error("type mismatch. expected: `{0}`, got: `{1}`")]
    TypeMismatch(Box<ResolvedType>, Box<ResolvedType>),

    #[error("unexpected token: `{0}`, expected: `{1}`")]
    UnexpectedToken(TokenKind, TokenKind),

    #[error("map key not hashable: type `{0}`")]
    UnhashableMapKey(Box<ResolvedType>),
}
