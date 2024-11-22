use std::fmt;

use super::{Expr, Ident, Type};

#[derive(Clone, Debug, PartialEq)]
pub enum Stmt {
    Void,
    Let(Ident, Type, Expr),
    Expr(Expr),
    Return(Expr),
}

impl Stmt {
    pub fn fmt_with_indent(&self, f: &mut fmt::Formatter, indent_level: usize) -> fmt::Result {
        let indent = " ".repeat(indent_level * 4);

        match self {
            Stmt::Void => write!(f, "{indent}"),
            Stmt::Let(ident, t, expr) => {
                write!(f, "{}let {ident}: {t} = ", indent)?;
                expr.fmt_with_indent(f, indent_level)?;
                writeln!(f, ";")
            }
            Stmt::Expr(expr) => expr.fmt_with_indent(f, indent_level),
            Stmt::Return(expr) => {
                write!(f, "{}return ", { indent })?;
                expr.fmt_with_indent(f, indent_level)?;
                writeln!(f, ";")
            }
        }
    }
}

impl fmt::Display for Stmt {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.fmt_with_indent(f, 0)
    }
}
