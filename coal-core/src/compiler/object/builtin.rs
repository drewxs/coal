use std::rc::Rc;

use crate::{Object, Param, Type};

#[derive(Clone, Debug, PartialEq, Hash)]
pub struct Builtin {
    pub func: fn(&[Rc<Object>]) -> Option<Rc<Object>>,
    pub args: Vec<Param>,
    pub ret_t: Type,
}
