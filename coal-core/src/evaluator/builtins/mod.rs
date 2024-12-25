mod builtin;
mod dbg;
mod print;
mod println;

use std::collections::HashMap;

pub use builtin::Builtin;

use crate::Type;

use super::Object;

pub fn map<'s>() -> HashMap<&'s str, Builtin> {
    let mut builtins = HashMap::new();

    builtins.insert("print", print::builtin());
    builtins.insert("println", println::builtin());
    builtins.insert("dbg", dbg::builtin());

    builtins
}

pub fn types() -> HashMap<String, Type> {
    map()
        .into_iter()
        .map(|(k, b)| (k.to_owned(), b.ret_t))
        .collect::<HashMap<String, Type>>()
}

pub fn objects() -> HashMap<String, Object> {
    map()
        .into_iter()
        .map(|(k, b)| (k.to_owned(), Object::Builtin(b)))
        .collect()
}
