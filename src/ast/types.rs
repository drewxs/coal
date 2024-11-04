use std::fmt;

use super::{Expr, Ident, Literal, Stmt};

#[derive(Clone, Debug, PartialEq, Default)]
pub enum Type {
    String,
    Int,
    Float,
    Bool,
    List(Box<Type>),
    Hash(Box<(Type, Type)>),
    Function(Vec<Ident>, Vec<Stmt>),
    #[default]
    Nil,
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::String => write!(f, "str"),
            Type::Int => write!(f, "int"),
            Type::Float => write!(f, "float"),
            Type::Bool => write!(f, "bool"),
            Type::List(t) => write!(f, "list[{t}]"),
            Type::Hash(t) => {
                let (k, v) = t.as_ref();
                write!(f, "hash[{k}, {v}]")
            }
            Type::Function(idents, stmts) => write!(f, "fn({idents:?}) {{{stmts:?}}}"),
            Type::Nil => write!(f, "nil"),
        }
    }
}

impl TryFrom<&Expr> for Type {
    type Error = String;

    fn try_from(literal: &Expr) -> Result<Self, Self::Error> {
        match literal {
            Expr::Literal(Literal::Str(_)) => Ok(Type::String),
            Expr::Literal(Literal::Int(_)) => Ok(Type::Int),
            Expr::Literal(Literal::Float(_)) => Ok(Type::Float),
            Expr::Literal(Literal::Bool(_)) => Ok(Type::Bool),
            _ => Err(String::from("invaild literal")),
        }
    }
}
