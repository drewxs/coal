use crate::{Builtin, Object, Type, Var};

use super::Def;

pub struct Println;

impl Def for Println {
    fn name() -> &'static str {
        "println"
    }

    fn def() -> Builtin {
        Builtin {
            func: Println::func,
            args: vec![Var::new("args", Type::Any)],
            ret_t: Type::Void,
        }
    }

    fn func(args: &[Object]) -> Option<Object> {
        println!("{}", args[0]);
        None
    }
}
