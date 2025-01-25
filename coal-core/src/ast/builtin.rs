use crate::{Object, Type, Param};

#[derive(Clone, Debug, PartialEq)]
pub struct Builtin {
    pub func: fn(&[Object]) -> Option<Object>,
    pub args: Vec<Param>,
    pub ret_t: Type,
}
