use std::fmt;

use super::{Ident, Infix, Literal, Prefix, Stmt, Type, Var};

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Ident(Ident),
    Literal(Literal),
    Prefix(Prefix, Box<Expr>),
    Infix(Infix, Box<Expr>, Box<Expr>),
    If {
        cond: Box<Expr>,
        then: Vec<Stmt>,
        elifs: Vec<IfExpr>,
        alt: Option<Vec<Stmt>>,
    },
    Fn {
        name: String,
        args: Vec<Var>,
        ret_t: Type,
        body: Vec<Stmt>,
    },
    Call {
        func: Box<Expr>,
        args: Vec<Expr>,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub struct IfExpr {
    pub cond: Box<Expr>,
    pub then: Vec<Stmt>,
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.fmt_with_indent(f, 0)
    }
}

impl Expr {
    pub fn fmt_with_indent(&self, f: &mut fmt::Formatter<'_>, indent_level: usize) -> fmt::Result {
        let indent = "    ".repeat(indent_level);

        match self {
            Expr::Ident(ident) => write!(f, "{ident}"),
            Expr::Literal(literal) => write!(f, "{literal}"),
            Expr::Prefix(prefix, expr) => write!(f, "({prefix}{expr})"),
            Expr::Infix(infix, lhs, rhs) => write!(f, "({lhs} {infix} {rhs})"),
            Expr::If {
                cond,
                then,
                elifs,
                alt,
            } => {
                writeln!(f, "{indent}if {cond} {{")?;
                for stmt in then {
                    stmt.fmt_with_indent(f, indent_level + 1)?;
                }
                for elif in elifs {
                    writeln!(f, "{indent}}} elif {} {{", elif.cond)?;
                    for stmt in &elif.then {
                        stmt.fmt_with_indent(f, indent_level + 1)?;
                    }
                }
                if let Some(else_block) = alt {
                    writeln!(f, "{indent}}} else {{")?;
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
            } => {
                writeln!(f, "fn {name}(")?;
                let args_str = args
                    .iter()
                    .map(|arg| format!("{arg}"))
                    .collect::<Vec<String>>()
                    .join(", ");
                writeln!(f, "{args_str}) -> {ret_t} {{")?;
                for stmt in body {
                    writeln!(f, "    {stmt}")?;
                }
                write!(f, "}}")
            }
            Expr::Call { func, args } => match func.as_ref() {
                Expr::Ident(ident) => {
                    let args_str = args
                        .iter()
                        .map(|arg| format!("{arg}"))
                        .collect::<Vec<String>>()
                        .join(", ");
                    write!(f, "{ident}({})", args_str)
                }
                _ => Err(fmt::Error),
            },
        }
    }
}
