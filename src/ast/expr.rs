use super::{Ident, Infix, Literal, Prefix};

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Ident(Ident),
    Literal(Literal),
    Prefix(Prefix, Box<Expr>),
    Infix(Infix, Box<Expr>, Box<Expr>),
}
