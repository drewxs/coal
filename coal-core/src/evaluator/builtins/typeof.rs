use crate::{Builtin, Object, Param, Type};

use super::Def;

pub struct Typeof;

// TODO: Since this is a statically typed language, this might not even need to exist.
impl Def for Typeof {
    fn name() -> &'static str {
        "typeof"
    }

    fn def() -> Builtin {
        Builtin {
            func: Typeof::func,
            args: vec![Param::new("args", Type::Any)],
            ret_t: Type::Str,
        }
    }

    fn func(args: &[Object]) -> Option<Object> {
        Some(Object::Str(Type::from(&args[0]).to_string()))
    }
}
