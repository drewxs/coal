use std::fmt;

use super::{Ident, Stmt};

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    String,
    Int,
    Float,
    Bool,
    Vec(Box<Type>),
    Hash(Box<(Type, Type)>),
    Function(Vec<Ident>, Vec<Stmt>),
    Nil,
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::String => write!(f, "str"),
            Type::Int => write!(f, "int"),
            Type::Float => write!(f, "float"),
            Type::Bool => write!(f, "bool"),
            Type::Vec(t) => write!(f, "[{t}]"),
            Type::Hash(t) => write!(f, "{{{t:?}}}"),
            Type::Function(idents, stmts) => write!(f, "fn({idents:?}) {{{stmts:?}}}"),
            Type::Nil => write!(f, "nil"),
        }
    }
}
