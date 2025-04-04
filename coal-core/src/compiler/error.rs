use std::fmt;

use thiserror::Error;

use crate::{ParserError, Span};

#[derive(Clone, Debug, Hash, PartialEq)]
pub struct CompileError {
    pub kind: CompileErrorKind,
    pub span: Span,
}

impl CompileError {
    pub fn new(kind: CompileErrorKind, span: Span) -> Self {
        CompileError { kind, span }
    }
}

impl From<&ParserError> for CompileError {
    fn from(value: &ParserError) -> Self {
        CompileError {
            kind: CompileErrorKind::ParserError(value.kind.to_string()),
            span: value.span,
        }
    }
}

impl fmt::Display for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let ((l1, c1), (l2, c2)) = self.span;
        write!(f, "{l1}:{c1}-{l2}:{c2} {}", self.kind)
    }
}

#[derive(Error, Clone, Debug, Hash, PartialEq)]
pub enum CompileErrorKind {
    #[error("[parser] {0}")]
    ParserError(String),

    #[error("[compiler] not found: `{0}`")]
    NotFound(String),
}
