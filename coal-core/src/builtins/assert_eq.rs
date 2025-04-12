use std::rc::Rc;

use crate::{Builtin, Object, Param, Type};

use super::Def;

pub struct AssertEq;

impl Def for AssertEq {
    fn name() -> &'static str {
        "assert_eq"
    }

    fn def() -> Builtin {
        Builtin {
            func: Self::func,
            args: vec![Param::new("lhs", Type::Any), Param::new("rhs", Type::Any)],
            ret_t: Type::Void,
        }
    }

    fn func(args: &[Rc<Object>]) -> Option<Rc<Object>> {
        assert_eq!(args[0], args[1]);
        None
    }
}
