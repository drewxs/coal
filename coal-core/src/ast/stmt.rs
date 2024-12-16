use std::fmt;

use super::{Comment, Expr, Ident, Type};

#[derive(Clone, Debug, PartialEq)]
pub enum Stmt {
    Void,
    Newline,
    Comment(Comment),
    Let(Ident, Type, Expr),
    Expr(Expr),
    Return(Expr),
}

impl Stmt {
    pub fn fmt_with_indent(&self, f: &mut fmt::Formatter, indent_level: usize) -> fmt::Result {
        let indent = " ".repeat(indent_level * 4);

        match self {
            Stmt::Void => Ok(()),
            Stmt::Newline => writeln!(f),
            Stmt::Comment(comment) => write!(f, "{}{comment}", indent),
            Stmt::Let(ident, t, expr) => {
                write!(f, "{}let {ident}: {t} = ", indent)?;
                expr.fmt_with_indent(f, indent_level)?;
                writeln!(f, ";")
            }
            Stmt::Expr(expr) => match expr {
                Expr::Ident(_, _)
                | Expr::Literal(_, _)
                | Expr::Prefix(_, _, _)
                | Expr::Infix(_, _, _, _)
                | Expr::Call { .. } => {
                    expr.fmt_with_indent(f, indent_level)?;
                    writeln!(f, ";")
                }
                _ => expr.fmt_with_indent(f, indent_level),
            },
            Stmt::Return(expr) => {
                write!(f, "{}return ", indent)?;
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
