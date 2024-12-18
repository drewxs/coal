use std::fmt;

use crate::Token;

#[derive(Clone, Debug, PartialEq)]
pub enum Infix {
    // Operators
    Plus,
    Minus,
    Mul,
    Div,
    IntDiv,
    Rem,

    // Comparisons
    EQ,
    NEQ,
    LT,
    LTE,
    GT,
    GTE,
}

impl TryFrom<&Token> for Infix {
    type Error = String;

    fn try_from(token: &Token) -> Result<Self, Self::Error> {
        match token {
            Token::Plus => Ok(Infix::Plus),
            Token::Minus => Ok(Infix::Minus),
            Token::Asterisk => Ok(Infix::Mul),
            Token::Slash => Ok(Infix::Div),
            Token::DoubleSlash => Ok(Infix::IntDiv),
            Token::Rem => Ok(Infix::Rem),
            Token::EQ => Ok(Infix::EQ),
            Token::NEQ => Ok(Infix::NEQ),
            Token::LT => Ok(Infix::LT),
            Token::LTE => Ok(Infix::LTE),
            Token::GT => Ok(Infix::GT),
            Token::GTE => Ok(Infix::GTE),
            _ => Err(format!("invalid infix token: {token:?}")),
        }
    }
}

impl fmt::Display for Infix {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Infix::Plus => write!(f, "+"),
            Infix::Minus => write!(f, "-"),
            Infix::Mul => write!(f, "*"),
            Infix::Div => write!(f, "/"),
            Infix::IntDiv => write!(f, "//"),
            Infix::Rem => write!(f, "%"),
            Infix::EQ => write!(f, "=="),
            Infix::NEQ => write!(f, "!="),
            Infix::LT => write!(f, "<"),
            Infix::LTE => write!(f, "<="),
            Infix::GT => write!(f, ">"),
            Infix::GTE => write!(f, ">="),
        }
    }
}
