use crate::ast::{Expr, Ident, Type};

#[derive(Clone, Debug, PartialEq)]
pub enum Stmt {
    Void,
    Let(Ident, Type, Expr),
    Expr(Expr),
    Return(Expr),
}
