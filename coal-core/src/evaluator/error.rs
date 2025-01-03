use std::fmt;

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

#[derive(Clone, Debug, Hash, PartialEq)]
pub enum RuntimeErrorKind {
    Custom(String),
    ParserError(String),
    Mismatch(String, Type),
    TypeMismatch(Type, Type),
    IdentifierNotFound(String),
    IdentifierExists(String),
    MethodNotFound(String),
    BadOperandTypeForUnary(char, Type),
    UnsupportedOperation(Infix, Type, Type),
    ReassignmentToFunction,
    InvalidArguments(String, String),
    InvalidArgumentsLength(usize, usize),
    IndexOutOfBounds(usize, usize),
    InvalidIndex(Type, Type),
    FailedToEvaluate,
}

impl fmt::Display for RuntimeErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            RuntimeErrorKind::Custom(s) => write!(f, "{s}"),
            RuntimeErrorKind::ParserError(e) => write!(f, "{e}"),
            RuntimeErrorKind::Mismatch(t1, t2) => {
                write!(f, "mismatch: expected={t1}, got={t2}")
            }
            RuntimeErrorKind::TypeMismatch(t1, t2) => {
                write!(f, "type mismatch: expected={t1}, got={t2}")
            }
            RuntimeErrorKind::IdentifierNotFound(name) => {
                write!(f, "identifier not found: {name}")
            }
            RuntimeErrorKind::IdentifierExists(name) => {
                write!(f, "identifier already exists: {name}")
            }
            RuntimeErrorKind::MethodNotFound(name) => {
                write!(f, "method not found: {name}")
            }
            RuntimeErrorKind::BadOperandTypeForUnary(op, t) => {
                write!(f, "bad operand type for unary {op}: {t}")
            }
            RuntimeErrorKind::UnsupportedOperation(op, t1, t2) => {
                write!(f, "unsupported operation: {t1} {op} {t2}")
            }
            RuntimeErrorKind::ReassignmentToFunction => {
                write!(f, "cannot assign to function")
            }
            RuntimeErrorKind::InvalidArguments(n1, n2) => {
                write!(f, "invalid args: expected={n1}, got={n2}")
            }
            RuntimeErrorKind::InvalidArgumentsLength(n1, n2) => {
                write!(f, "invalid # args: expected={n1}, got={n2}",)
            }
            RuntimeErrorKind::IndexOutOfBounds(i, len) => {
                write!(f, "index out of bounds. len={len}, index={i}")
            }
            RuntimeErrorKind::InvalidIndex(t, idx_t) => {
                write!(f, "the type `{t}` cannot be indexed by `{idx_t}`")
            }
            RuntimeErrorKind::FailedToEvaluate => {
                write!(f, "failed to evaluate expression")
            }
        }
    }
}
