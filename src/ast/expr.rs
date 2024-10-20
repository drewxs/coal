use crate::ast::{Ident, Literal, Prefix};

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Ident(Ident),
    Literal(Literal),
    Prefix(Prefix, Box<Expr>),
}
