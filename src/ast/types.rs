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
