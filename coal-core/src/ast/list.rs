use super::{Expr, Type};

#[derive(Clone, Debug, PartialEq)]
pub struct List {
    pub data: Vec<Expr>,
    pub t: Type,
    pub repeat: Option<Box<Expr>>,
}

impl List {
    pub fn new(data: &[Expr], t: Type, repeat: Option<Box<Expr>>) -> Self {
        List {
            data: data.to_owned(),
            t,
            repeat,
        }
    }
}

impl Default for List {
    fn default() -> Self {
        List {
            data: vec![],
            t: Type::Unknown,
            repeat: None,
        }
    }
}
