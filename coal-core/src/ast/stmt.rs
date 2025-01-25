use std::fmt;

use crate::{ParserError, ParserErrorKind, Span};

use super::{Comment, Expr, Func, Ident, Infix, Param, Type};

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
    pub fn span(&self) -> Span {
        match self {
            Stmt::Void | Stmt::Newline | Stmt::Comment(_) => Span::default(),
            Stmt::Let(_, _, e) => e.span(),
            Stmt::Assign(lhs, rhs) => (lhs.span().0, rhs.span().1),
            Stmt::OpAssign(_, lhs, rhs) => (lhs.span().0, rhs.span().1),
            Stmt::Return(e) => e.span(),
            Stmt::Expr(e) => e.span(),
        }
    }

    pub fn ret_t(&self, expected: &Type, last: bool) -> Result<Type, ParserError> {
        match self {
            Stmt::Return(e) => Type::try_from(e).map_err(|_| {
                ParserError::new(
                    ParserErrorKind::TypeMismatch(expected.clone(), Type::Void),
                    e.span(),
                )
            }),
            Stmt::Expr(e) => e.ret_t(expected, last),
            _ => Ok(Type::Void),
        }
    }

    pub fn ret_stmts(&self) -> Vec<Stmt> {
        let mut rets = vec![];
        match self {
            Stmt::Return(_) => rets.push(self.clone()),
            Stmt::Expr(expr) => rets.extend(expr.ret_stmts()),
            _ => {}
        }
        rets
    }

    pub fn fmt_with_indent(&self, f: &mut fmt::Formatter, indent_level: usize) -> fmt::Result {
        let indent = "    ".repeat(indent_level);

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
                write!(f, "{}{lhs} = ", indent)?;
                rhs.fmt_with_indent(f, indent_level)?;
                writeln!(f, ";")
            }
            Stmt::OpAssign(op, lhs, rhs) => {
                write!(f, "{}{lhs} {op}= ", indent)?;
                rhs.fmt_with_indent(f, indent_level)?;
                writeln!(f, ";")
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
                    write!(f, "{indent}")?;
                    expr.fmt_with_indent(f, indent_level)?;
                    writeln!(f, ";")
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
