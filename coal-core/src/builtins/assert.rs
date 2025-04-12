use std::rc::Rc;

use crate::{Builtin, Object, Param, Type};

use super::Def;

pub struct Assert;

impl Def for Assert {
    fn name() -> &'static str {
        "assert"
    }

    fn def() -> Builtin {
        Builtin {
            func: Self::func,
            args: vec![Param::new("cond", Type::Any)],
            ret_t: Type::Void,
        }
    }

    fn func(args: &[Rc<Object>]) -> Option<Rc<Object>> {
        assert!(args[0].is_truthy());
        None
    }
}
