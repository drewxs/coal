use std::fmt;

#[derive(Clone, Debug, PartialEq)]
pub enum Literal {
    Str(String),
    Int(i64),
    Float(f64),
    Bool(bool),
    Nil,
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Literal::Str(s) => write!(f, "\"{s}\""),
            Literal::Int(i) => write!(f, "{i}"),
            Literal::Float(x) => write!(f, "{x:?}"),
            Literal::Bool(b) => write!(f, "{b}"),
            Literal::Nil => write!(f, "nil"),
        }
    }
}

impl From<&str> for Literal {
    fn from(s: &str) -> Self {
        match s.parse::<f64>() {
            Ok(f) if f.fract() == 0.0 => Literal::Int(f as i64),
            Ok(f) => Literal::Float(f),
            Err(_) => match s {
                "true" => Literal::Bool(true),
                "false" => Literal::Bool(false),
                "nil" => Literal::Nil,
                _ => Literal::Str(s.to_string()),
            },
        }
    }
}
