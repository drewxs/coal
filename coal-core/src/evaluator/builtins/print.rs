use crate::{Object, Type, Var};

use super::Builtin;

fn func(args: &[Object]) -> Option<Object> {
    print!("{}", args[0]);
    None
}

pub fn builtin() -> Builtin {
    Builtin {
        name: String::from("print"),
        func,
        args: vec![Var::new("args", Type::Any)],
        ret_t: Type::Void,
    }
}
