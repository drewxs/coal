use std::fmt;

use super::{Comment, Expr, Ident, Infix, Type};

#[derive(Clone, Debug, PartialEq)]
pub enum Stmt {
    Void,
    Newline,
    Comment(Comment),
    Let(Ident, Type, Expr),
    Assign(Expr, Expr),
    OpAssign(Infix, Expr, Expr),
    Return(Expr),
    Expr(Expr),
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
            Stmt::Assign(lhs, rhs) => {
                writeln!(f, "{}{lhs} = {rhs};", indent)
            }
            Stmt::OpAssign(op, lhs, rhs) => {
                writeln!(f, "{}{lhs} {op}= {rhs};", indent)
            }
            Stmt::Return(expr) => {
                write!(f, "{}return ", indent)?;
                expr.fmt_with_indent(f, indent_level)?;
                writeln!(f, ";")
            }
            Stmt::Expr(expr) => match expr {
                Expr::Ident(_, _, _)
                | Expr::Literal(_, _)
                | Expr::Prefix(_, _, _)
                | Expr::Infix(_, _, _, _)
                | Expr::Call { .. }
                | Expr::MethodCall { .. } => {
                    writeln!(f, "{}{expr};", indent)
                }
                _ => expr.fmt_with_indent(f, indent_level),
            },
        }
    }
}

impl fmt::Display for Stmt {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.fmt_with_indent(f, 0)
    }
}
