use std::rc::Rc;

use crate::{Builtin, Object, Param, Type};

use super::Def;

pub struct Println;

impl Def for Println {
    fn name() -> &'static str {
        "println"
    }

    fn def() -> Builtin {
        Builtin {
            func: Self::func,
            args: vec![Param::new("args", Type::Any)],
            ret_t: Type::Void,
        }
    }

    fn func(args: &[Rc<Object>]) -> Option<Rc<Object>> {
        println!("{}", args[0]);
        None
    }
}
