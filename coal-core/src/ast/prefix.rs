use std::fmt;

use crate::Token;

#[derive(Clone, Debug, PartialEq)]
pub enum Prefix {
    Minus,
    Not,
}

impl TryFrom<&Token> for Prefix {
    type Error = String;

    fn try_from(token: &Token) -> Result<Self, Self::Error> {
        match token {
            Token::Bang => Ok(Prefix::Not),
            Token::Sub => Ok(Prefix::Minus),
            _ => Err(format!("invalid prefix token: {token:?}")),
        }
    }
}

impl fmt::Display for Prefix {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Prefix::Minus => write!(f, "-"),
            Prefix::Not => write!(f, "!"),
        }
    }
}
