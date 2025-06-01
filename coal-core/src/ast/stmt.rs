use std::fmt;

use crate::{ParserError, ParserErrorKind, Span, indent};

use super::{Comment, Expr, Ident, Infix, StructDecl, Type};

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
    StructDecl(StructDecl, Span),
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
            Stmt::StructDecl(_, span) => *span,
        }
    }

    pub fn ret_t(&self, expected: &Type, last: bool) -> Result<Type, ParserError> {
        match self {
            Stmt::Return(e) => Type::try_from(e).map_err(|_| {
                ParserError::new(
                    ParserErrorKind::TypeMismatch(expected.clone().into(), Type::Void.into()),
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
        let base_indent = indent(indent_level);

        match self {
            Stmt::Void => Ok(()),
            Stmt::Newline => writeln!(f),
            Stmt::Comment(comment) => write!(f, "{base_indent}{comment}"),
            Stmt::Let(ident, t, expr) => {
                if let Expr::Struct(_, _) = expr {
                    write!(f, "{base_indent}let {ident} = ")?;
                } else {
                    write!(f, "{base_indent}let {ident}: {t} = ")?;
                }
                expr.fmt_with_indent(f, indent_level, true)?;
                writeln!(f, ";")
            }
            Stmt::Assign(lhs, rhs) => {
                write!(f, "{base_indent}{lhs} = ")?;
                rhs.fmt_with_indent(f, indent_level, true)?;
                writeln!(f, ";")
            }
            Stmt::OpAssign(op, lhs, rhs) => {
                write!(f, "{base_indent}{lhs} {op}= ")?;
                rhs.fmt_with_indent(f, indent_level, true)?;
                writeln!(f, ";")
            }
            Stmt::Return(expr) => {
                write!(f, "{base_indent}return ")?;
                expr.fmt_with_indent(f, indent_level, true)?;
                writeln!(f, ";")
            }
            Stmt::Expr(expr) => match expr {
                Expr::Ident(_, _, _)
                | Expr::Literal(_, _)
                | Expr::Prefix(_, _, _)
                | Expr::Infix(_, _, _, _)
                | Expr::Call { .. }
                | Expr::MethodCall { .. }
                | Expr::AttrAccess { .. } => {
                    write!(f, "{base_indent}")?;
                    expr.fmt_with_indent(f, indent_level, true)?;
                    writeln!(f, ";")
                }
                _ => expr.fmt_with_indent(f, indent_level, true),
            },
            Stmt::StructDecl(s, _) => s.fmt_with_indent(f, indent_level),
        }
    }
}

impl fmt::Display for Stmt {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.fmt_with_indent(f, 0)
    }
}
