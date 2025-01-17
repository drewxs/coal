use std::fmt;

use crate::Span;

#[derive(Clone, Debug)]
pub struct ParserWarning {
    pub kind: ParserWarningKind,
    pub span: Span,
}

impl ParserWarning {
    pub fn new(kind: ParserWarningKind, span: Span) -> Self {
        ParserWarning { kind, span }
    }
}

impl fmt::Display for ParserWarning {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let ((l1, c1), (l2, c2)) = self.span;
        write!(f, "{l1}:{c1}-{l2}:{c2} {}", self.kind)
    }
}

#[derive(Clone, Debug)]
pub enum ParserWarningKind {
    UnreachableStatement,
    MapKeyRepeated(String),
}

impl fmt::Display for ParserWarningKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ParserWarningKind::UnreachableStatement => {
                write!(f, "any code following this expression is unreachable")
            }
            ParserWarningKind::MapKeyRepeated(key) => {
                write!(f, "map key repeated: `{key}`")
            }
        }
    }
}
