use std::fmt;

use crate::TokenKind;

#[derive(Clone, Debug, Hash, PartialEq)]
pub enum Infix {
    // Operators
    Add,
    Sub,
    Mul,
    Div,
    Rem,

    // Comparisons
    EQ,
    NEQ,
    LT,
    LTE,
    GT,
    GTE,
}

impl TryFrom<&TokenKind> for Infix {
    type Error = String;

    fn try_from(token: &TokenKind) -> Result<Self, Self::Error> {
        match token {
            TokenKind::Add | TokenKind::AddAssign => Ok(Infix::Add),
            TokenKind::Sub | TokenKind::SubAssign => Ok(Infix::Sub),
            TokenKind::Mul | TokenKind::MulAssign => Ok(Infix::Mul),
            TokenKind::Div | TokenKind::DivAssign => Ok(Infix::Div),
            TokenKind::Rem | TokenKind::RemAssign => Ok(Infix::Rem),
            TokenKind::EQ => Ok(Infix::EQ),
            TokenKind::NEQ => Ok(Infix::NEQ),
            TokenKind::LT => Ok(Infix::LT),
            TokenKind::LTE => Ok(Infix::LTE),
            TokenKind::GT => Ok(Infix::GT),
            TokenKind::GTE => Ok(Infix::GTE),
            _ => Err(format!("invalid infix token: {token:?}")),
        }
    }
}

impl fmt::Display for Infix {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Infix::Add => write!(f, "+"),
            Infix::Sub => write!(f, "-"),
            Infix::Mul => write!(f, "*"),
            Infix::Div => write!(f, "/"),
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
