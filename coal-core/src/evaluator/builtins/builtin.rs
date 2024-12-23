use crate::{Object, Type, Var};

pub type BuiltinFn = fn(&[Object]) -> Option<Object>;

#[derive(Clone, Debug, PartialEq)]
pub struct Builtin {
    pub name: String,
    pub func: BuiltinFn,
    pub args: Vec<Var>,
    pub ret_t: Type,
}

impl Builtin {
    pub fn new(name: &str, func: BuiltinFn, args: Vec<Var>, ret_t: Type) -> Self {
        Builtin {
            name: name.to_owned(),
            func,
            args,
            ret_t,
        }
    }
}
