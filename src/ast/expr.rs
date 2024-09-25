use crate::ast::{Ident, Literal};

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Ident(Ident),
    Literal(Literal),
}
