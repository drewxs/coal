use crate::{Builtin, Object, Type, Var};

use super::Def;

pub struct Print;

impl Def for Print {
    fn name() -> &'static str {
        "print"
    }

    fn def() -> Builtin {
        Builtin {
            func: Print::func,
            args: vec![Var::new("args", Type::Any)],
            ret_t: Type::Void,
        }
    }

    fn func(args: &[Object]) -> Option<Object> {
        print!("{}", args[0]);
        None
    }
}
