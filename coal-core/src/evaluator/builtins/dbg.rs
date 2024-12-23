use crate::{Object, Type, Var};

use super::Builtin;

fn func(args: &[Object]) -> Option<Object> {
    println!("{:?}", args[0]);
    None
}

pub fn builtin() -> Builtin {
    Builtin {
        name: String::from("dbg"),
        func,
        args: vec![Var::new("args", Type::Any)],
        ret_t: Type::Void,
    }
}
