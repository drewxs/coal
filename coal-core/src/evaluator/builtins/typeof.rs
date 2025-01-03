use crate::{Builtin, Object, Type, Var};

use super::Def;

pub struct Typeof;

impl Def for Typeof {
    fn name() -> &'static str {
        "typeof"
    }

    fn def() -> Builtin {
        Builtin {
            func: Typeof::func,
            args: vec![Var::new("args", Type::Any)],
            ret_t: Type::Void,
        }
    }

    fn func(args: &[Object]) -> Option<Object> {
        Some(Object::Type(Type::from(&args[0])))
    }
}
