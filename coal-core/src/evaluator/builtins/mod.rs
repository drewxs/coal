mod builtin;
mod dbg;
mod print;
mod println;

use std::collections::HashMap;

pub use builtin::Builtin;

pub fn builtins<'s>() -> HashMap<&'s str, Builtin> {
    let mut builtins = HashMap::new();

    builtins.insert("print", print::builtin());
    builtins.insert("println", println::builtin());
    builtins.insert("dbg", dbg::builtin());

    builtins
}
