use std::fmt;

use super::{Expr, Ident, Type};

#[derive(Clone, Debug, PartialEq)]
pub enum Stmt {
    Void,
    Let(Ident, Type, Expr),
    Expr(Expr),
    Return(Expr),
}

impl fmt::Display for Stmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Stmt::Void => write!(f, ";"),
            Stmt::Let(ident, t, expr) => write!(f, "let {ident}: {t} = {expr};"),
            Stmt::Expr(expr) => write!(f, "{expr};"),
            Stmt::Return(expr) => write!(f, "return {expr};"),
        }
    }
}
