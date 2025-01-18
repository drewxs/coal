use super::{Expr, Type};

#[derive(Clone, Debug, PartialEq)]
pub struct Map {
    pub data: Vec<(Expr, Expr)>,
    pub t: (Type, Type),
}

impl Map {
    pub fn new(data: &[(Expr, Expr)], t: (Type, Type)) -> Self {
        Map {
            data: data.to_owned(),
            t,
        }
    }
}

impl Default for Map {
    fn default() -> Self {
        Map {
            data: vec![],
            t: (Type::Unknown, Type::Unknown),
        }
    }
}
