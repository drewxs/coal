use std::fmt;

#[derive(Clone, Debug)]
pub struct ParserError {
    pub kind: ParserErrorKind,
    pub msg: String,
}

impl ParserError {
    pub fn new(kind: ParserErrorKind, msg: String) -> Self {
        ParserError { kind, msg }
    }
}

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[{}] {}", self.kind, self.msg)
    }
}

#[derive(Clone, Debug)]
pub enum ParserErrorKind {
    UnexpectedToken,
}

impl fmt::Display for ParserErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}
