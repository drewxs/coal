use crate::{Object, Type, Var};

#[derive(Clone, Debug, PartialEq)]
pub struct Builtin {
    pub func: fn(&[Object]) -> Option<Object>,
    pub args: Vec<Var>,
    pub ret_t: Type,
}
