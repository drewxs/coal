use std::collections::HashMap;

use super::Type;

pub fn definitions() -> HashMap<String, Type> {
    let mut builtins = HashMap::new();

    builtins.insert(
        "assert".to_owned(),
        Type::Fn {
            args_t: vec![Type::Bool],
            ret_t: Box::new(Type::Void),
            uses_self: false,
        },
    );
    builtins.insert(
        "assert_eq".to_owned(),
        Type::Fn {
            args_t: vec![Type::Any, Type::Any],
            ret_t: Box::new(Type::Void),
            uses_self: false,
        },
    );
    builtins.insert(
        "dbg".to_owned(),
        Type::Fn {
            args_t: vec![Type::Any],
            ret_t: Box::new(Type::Void),
            uses_self: false,
        },
    );
    builtins.insert(
        "print".to_owned(),
        Type::Fn {
            args_t: vec![Type::Any],
            ret_t: Box::new(Type::Void),
            uses_self: false,
        },
    );
    builtins.insert(
        "println".to_owned(),
        Type::Fn {
            args_t: vec![Type::Any],
            ret_t: Box::new(Type::Void),
            uses_self: false,
        },
    );

    builtins
}
