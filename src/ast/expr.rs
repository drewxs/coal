use std::fmt;

use super::{Ident, Infix, Literal, Prefix};

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Ident(Ident),
    Literal(Literal),
    Prefix(Prefix, Box<Expr>),
    Infix(Infix, Box<Expr>, Box<Expr>),
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Ident(ident) => write!(f, "{ident}"),
            Expr::Literal(literal) => write!(f, "{literal}"),
            Expr::Prefix(prefix, expr) => write!(f, "({prefix}{expr})"),
            Expr::Infix(infix, lhs, rhs) => write!(f, "({lhs} {infix} {rhs})"),
        }
    }
}
