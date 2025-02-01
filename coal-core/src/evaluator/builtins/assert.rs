use crate::{Builtin, Object, Param, Type};

use super::Def;

pub struct Assert;

impl Def for Assert {
    fn name() -> &'static str {
        "assert"
    }

    fn def() -> Builtin {
        Builtin {
            func: Assert::func,
            args: vec![Param::new("cond", Type::Any)],
            ret_t: Type::Void,
        }
    }

    fn func(args: &[Object]) -> Option<Object> {
        assert!(args[0].is_truthy());
        None
    }
}
