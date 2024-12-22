use std::fmt;

use crate::Span;

use super::{Ident, Infix, Literal, Prefix, Stmt, Type, Var};

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Ident(Ident, Type, Span),
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
    While {
        cond: Box<Expr>,
        body: Vec<Stmt>,
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
        name: String,
        args: Vec<Expr>,
        ret_t: Type,
        span: Span,
    },
    MethodCall {
        lhs: Box<Expr>,
        name: String,
        args: Vec<Expr>,
        ret_t: Type,
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
            Expr::Ident(_, _, span) => *span,
            Expr::Literal(_, span) => *span,
            Expr::Prefix(_, _, span) => *span,
            Expr::Infix(_, _, _, span) => *span,
            Expr::If { span, .. } => *span,
            Expr::While { span, .. } => *span,
            Expr::Fn { span, .. } => *span,
            Expr::Call { span, .. } => *span,
            Expr::MethodCall { span, .. } => *span,
        }
    }

    pub fn fmt_with_indent(&self, f: &mut fmt::Formatter, indent_level: usize) -> fmt::Result {
        let indent = "    ".repeat(indent_level);

        match self {
            Expr::Ident(ident, _, _) => write!(f, "{ident}"),
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
            Expr::While { cond, body, .. } => {
                writeln!(f, "{}while {cond} {{", indent)?;
                for stmt in body {
                    stmt.fmt_with_indent(f, indent_level + 1)?;
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
                let args = args
                    .iter()
                    .map(|arg| format!("{arg}"))
                    .collect::<Vec<String>>()
                    .join(", ");
                writeln!(f, "{args}) -> {ret_t} {{")?;
                for stmt in body {
                    stmt.fmt_with_indent(f, indent_level + 1)?;
                }
                writeln!(f, "{}}}", indent)
            }
            Expr::Call { name, args, .. } => {
                let args = args
                    .iter()
                    .map(|arg| format!("{arg}"))
                    .collect::<Vec<String>>()
                    .join(", ");
                write!(f, "{name}({args})")
            }
            Expr::MethodCall {
                lhs, name, args, ..
            } => {
                let args = args
                    .iter()
                    .map(|arg| format!("{arg}"))
                    .collect::<Vec<String>>()
                    .join(", ");
                write!(f, "{lhs}.{name}({args})")
            }
        }
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.fmt_with_indent(f, 0)
    }
}
