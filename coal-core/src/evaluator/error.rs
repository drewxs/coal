use std::fmt;

use thiserror::Error;

use crate::{Infix, ParserError, Span, Type};

#[derive(Clone, Debug, Hash, PartialEq)]
pub struct RuntimeError {
    pub kind: RuntimeErrorKind,
    pub span: Span,
}

impl RuntimeError {
    pub fn new(kind: RuntimeErrorKind, span: Span) -> Self {
        RuntimeError { kind, span }
    }
}

impl From<&ParserError> for RuntimeError {
    fn from(value: &ParserError) -> Self {
        RuntimeError {
            kind: RuntimeErrorKind::ParserError(value.kind.to_string()),
            span: value.span,
        }
    }
}

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let ((l1, c1), (l2, c2)) = self.span;
        write!(f, "{l1}:{c1}-{l2}:{c2} {}", self.kind)
    }
}

#[derive(Error, Clone, Debug, Hash, PartialEq)]
pub enum RuntimeErrorKind {
    #[error("{0}")]
    Custom(String),

    #[error("[parse] {0}")]
    ParserError(String),

    #[error("[runtime] bad operand type for unary {0}: {1}")]
    BadOperandTypeForUnary(char, Type),

    #[error("[runtime] failed to evaluate expression")]
    FailedToEvaluate,

    #[error("[runtime] identifier already exists: {0}")]
    IdentifierExists(String),

    #[error("[runtime] identifier not found: {0}")]
    IdentifierNotFound(String),

    #[error("[runtime] index out of bounds. len={0}, index={1}")]
    IndexOutOfBounds(usize, usize),

    #[error("[runtime] invalid args: expected={0}, got={1}")]
    InvalidArguments(String, String),

    #[error("[runtime] invalid # args: expected={0}, got={1}")]
    InvalidArgumentsLength(usize, usize),

    #[error("[runtime] method not found: {0}")]
    MethodNotFound(String),

    #[error("[runtime] mismatch: expected={0}, got={1}")]
    Mismatch(String, Type),

    #[error("[runtime] reassignment to function")]
    ReassignmentToFunction,

    #[error("[runtime] type mismatch: expected={0}, got={1}")]
    TypeMismatch(Type, Type),

    #[error("[runtime] unsupported operation: {0} {1} {2}")]
    UnsupportedOperation(Infix, Type, Type),
}
