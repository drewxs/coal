use std::rc::Rc;

use crate::{Builtin, Object, Param, Type};

use super::Def;

pub struct Dbg;

impl Def for Dbg {
    fn name() -> &'static str {
        "dbg"
    }

    fn def() -> Builtin {
        Builtin {
            func: Self::func,
            args: vec![Param::new("args", Type::Any)],
            ret_t: Type::Void,
        }
    }

    fn func(args: &[Rc<Object>]) -> Option<Rc<Object>> {
        println!("{:?}", args[0]);
        None
    }
}
