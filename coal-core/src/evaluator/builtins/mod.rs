mod dbg;
mod def;
mod print;
mod println;
mod r#typeof;

use std::collections::HashMap;

use crate::{Builtin, Object, Type};

use dbg::Dbg;
pub use def::Def;
use print::Print;
use println::Println;
use r#typeof::Typeof;

pub fn map<'s>() -> HashMap<&'s str, Builtin> {
    let mut builtins = HashMap::new();

    builtins.insert(Dbg::name(), Dbg::def());
    builtins.insert(Print::name(), Print::def());
    builtins.insert(Println::name(), Println::def());
    builtins.insert(Typeof::name(), Typeof::def());

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
