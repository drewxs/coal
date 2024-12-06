use std::fmt;

use crate::Span;

use super::{Ident, Infix, Literal, Prefix, Stmt, Type, Var};

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Ident(Ident, Span),
    Literal(Literal, Span),
    Prefix(Prefix, Box<Expr>, Span),
    Infix(Infix, Box<Expr>, Box<Expr>, Span),
    If {
        cond: Box<Expr>,
        then: Vec<Stmt>,
        elifs: Vec<IfExpr>,
        alt: Option<Vec<Stmt>>,
        span: Span,
    },
    Fn {
        name: String,
        args: Vec<Var>,
        ret_t: Type,
        body: Vec<Stmt>,
        span: Span,
    },
    Call {
        func: Box<Expr>,
        args: Vec<Expr>,
        span: Span,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub struct IfExpr {
    pub cond: Box<Expr>,
    pub then: Vec<Stmt>,
}

impl Expr {
    pub fn span(&self) -> Span {
        match self {
            Expr::Ident(_, span) => *span,
            Expr::Literal(_, span) => *span,
            Expr::Prefix(_, _, span) => *span,
            Expr::Infix(_, _, _, span) => *span,
            Expr::If { span, .. } => *span,
            Expr::Fn { span, .. } => *span,
            Expr::Call { span, .. } => *span,
        }
    }

    pub fn fmt_with_indent(&self, f: &mut fmt::Formatter, indent_level: usize) -> fmt::Result {
        let indent = "    ".repeat(indent_level);

        match self {
            Expr::Ident(ident, _) => write!(f, "{ident}"),
            Expr::Literal(literal, _) => write!(f, "{literal}"),
            Expr::Prefix(prefix, expr, _) => write!(f, "{prefix}{expr}"),
            Expr::Infix(infix, lhs, rhs, _) => write!(f, "{lhs} {infix} {rhs}"),
            Expr::If {
                cond,
                then,
                elifs,
                alt,
                ..
            } => {
                writeln!(f, "{}if {cond} {{", indent)?;
                for stmt in then {
                    stmt.fmt_with_indent(f, indent_level + 1)?;
                }
                for elif in elifs {
                    writeln!(f, "{}}} elif {} {{", indent, elif.cond)?;
                    for stmt in &elif.then {
                        stmt.fmt_with_indent(f, indent_level + 1)?;
                    }
                }
                if let Some(else_block) = alt {
                    writeln!(f, "{}}} else {{", indent)?;
                    for stmt in else_block {
                        stmt.fmt_with_indent(f, indent_level + 1)?;
                    }
                }
                writeln!(f, "{indent}}}")
            }
            Expr::Fn {
                name,
                args,
                ret_t,
                body,
                ..
            } => {
                write!(f, "{}fn {name}(", indent)?;
                let args_str = args
                    .iter()
                    .map(|arg| format!("{arg}"))
                    .collect::<Vec<String>>()
                    .join(", ");
                writeln!(f, "{args_str}) -> {ret_t} {{")?;
                for stmt in body {
                    writeln!(f, "{}    {stmt}", indent)?;
                }
                write!(f, "}}")
            }
            Expr::Call { func, args, .. } => match func.as_ref() {
                Expr::Ident(name, _) => {
                    let args_str = args
                        .iter()
                        .map(|arg| format!("{arg}"))
                        .collect::<Vec<String>>()
                        .join(", ");
                    write!(f, "{name}({args_str})")
                }
                _ => Err(fmt::Error),
            },
        }
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.fmt_with_indent(f, 0)
    }
}
