use std::fmt;

use crate::TokenKind;

#[derive(Clone, Debug, PartialEq)]
pub enum Prefix {
    Minus,
    Not,
}

impl TryFrom<&TokenKind> for Prefix {
    type Error = String;

    fn try_from(token: &TokenKind) -> Result<Self, Self::Error> {
        match token {
            TokenKind::Bang => Ok(Prefix::Not),
            TokenKind::Sub => Ok(Prefix::Minus),
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
