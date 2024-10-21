use std::fmt;

use crate::Token;

#[derive(Clone, Debug, PartialEq)]
pub enum Prefix {
    Plus,
    Minus,
    Not,
}

impl TryFrom<&Token> for Prefix {
    type Error = String;

    fn try_from(token: &Token) -> Result<Self, Self::Error> {
        match token {
            Token::Bang => Ok(Prefix::Not),
            Token::Plus => Ok(Prefix::Plus),
            Token::Minus => Ok(Prefix::Minus),
            _ => Err(format!("invalid prefix token: {token:?}")),
        }
    }
}

impl fmt::Display for Prefix {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Prefix::Plus => write!(f, "+"),
            Prefix::Minus => write!(f, "-"),
            Prefix::Not => write!(f, "!"),
        }
    }
}
