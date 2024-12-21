use std::fmt;

use crate::{Infix, Span, Type};

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

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let ((l1, c1), (l2, c2)) = self.span;
        write!(f, "{l1}:{c1}-{l2}:{c2} {}", self.kind)
    }
}

#[derive(Clone, Debug, Hash, PartialEq)]
pub enum RuntimeErrorKind {
    Custom(String),
    Mismatch(String, Type),
    TypeMismatch(Type, Type),
    IdentifierNotFound(String),
    IdentifierExists(String),
    BadOperandTypeForUnary(char, Type),
    UnsupportedOperation(Infix, Type, Type),
    ReassignmentToFunction,
    InvalidArguments(String, String),
    IncorrectNumberOfArguments(usize, usize),
}

impl fmt::Display for RuntimeErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            RuntimeErrorKind::Custom(s) => write!(f, "{s}"),
            RuntimeErrorKind::Mismatch(t1, t2) => {
                write!(f, "type mismatch: expected={t1}, got={t2}")
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
            RuntimeErrorKind::BadOperandTypeForUnary(op, t) => {
                write!(f, "bad operand type for unary {op}: {t}")
            }
            RuntimeErrorKind::UnsupportedOperation(op, t1, t2) => {
                write!(f, "unsupported operation: {t1} {op} {t2}")
            }
            RuntimeErrorKind::ReassignmentToFunction => {
                write!(f, "cannot assign to function")
            }
            RuntimeErrorKind::InvalidArguments(expected, got) => {
                write!(f, "invalid arguments: expected={expected}, got={got}")
            }
            RuntimeErrorKind::IncorrectNumberOfArguments(x, y) => {
                write!(
                    f,
                    "expected {x} argument{}, got {y}",
                    if *x > 1 { "s" } else { "" },
                )
            }
        }
    }
}
