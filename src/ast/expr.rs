use std::fmt;

use super::{Ident, Infix, Literal, Prefix, Stmt};

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Ident(Ident),
    Literal(Literal),
    Prefix(Prefix, Box<Expr>),
    Infix(Infix, Box<Expr>, Box<Expr>),
    If {
        condition: Box<Expr>,
        consequence: Vec<Stmt>,
        else_ifs: Vec<IfExpr>,
        alternative: Option<Vec<Stmt>>,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub struct IfExpr {
    pub condition: Box<Expr>,
    pub consequence: Vec<Stmt>,
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Ident(ident) => write!(f, "{ident}"),
            Expr::Literal(literal) => write!(f, "{literal}"),
            Expr::Prefix(prefix, expr) => write!(f, "({prefix}{expr})"),
            Expr::Infix(infix, lhs, rhs) => write!(f, "({lhs} {infix} {rhs})"),
            Expr::If {
                condition,
                consequence,
                else_ifs,
                alternative,
            } => {
                writeln!(f, "if {condition} {{")?;
                for stmt in consequence {
                    writeln!(f, "    {stmt}")?;
                }
                for expr in else_ifs {
                    writeln!(f, "}} elif {} {{", expr.condition)?;
                    for stmt in &expr.consequence {
                        writeln!(f, "    {stmt}")?;
                    }
                }
                if let Some(alternative) = alternative {
                    writeln!(f, "}} else {{")?;
                    for stmt in alternative {
                        writeln!(f, "    {stmt}")?;
                    }
                }
                write!(f, "}}")
            }
        }
    }
}
