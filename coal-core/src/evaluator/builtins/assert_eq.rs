use crate::{Builtin, Object, Type, Var};

use super::Def;

pub struct AssertEq;

impl Def for AssertEq {
    fn name() -> &'static str {
        "assert_eq"
    }

    fn def() -> Builtin {
        Builtin {
            func: AssertEq::func,
            args: vec![Var::new("lhs", Type::Any), Var::new("rhs", Type::Any)],
            ret_t: Type::Void,
        }
    }

    fn func(args: &[Object]) -> Option<Object> {
        assert_eq!(args[0], args[1]);
        None
    }
}
