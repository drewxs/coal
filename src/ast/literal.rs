use std::fmt;

#[derive(Clone, Debug, PartialEq)]
pub enum Literal {
    Str(String),
    Int(i64),
    Float(f64),
    Bool(bool),
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Literal::Str(s) => write!(f, "\"{s}\""),
            Literal::Int(i) => write!(f, "{i}"),
            Literal::Float(x) => write!(f, "{x:?}"),
            Literal::Bool(b) => write!(f, "{b}"),
        }
    }
}
