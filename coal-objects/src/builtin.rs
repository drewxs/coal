use std::hash::{Hash, Hasher};

use crate::Object;

#[derive(Clone, Debug)]
pub struct Builtin {
    pub name: &'static str,
    pub func: fn(&[Object]) -> Option<Object>,
}

impl PartialEq for Builtin {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl Hash for Builtin {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.name.hash(state);
        self.func.hash(state);
    }
}

fn assert(args: &[Object]) -> Option<Object> {
    assert!(args[0].is_truthy());
    None
}

fn assert_eq(args: &[Object]) -> Option<Object> {
    assert_eq!(args[0], args[1]);
    None
}

fn dbg(args: &[Object]) -> Option<Object> {
    println!("{:?}", args[0]);
    None
}

fn print(args: &[Object]) -> Option<Object> {
    print!("{}", args[0]);
    None
}

fn println(args: &[Object]) -> Option<Object> {
    println!("{}", args[0]);
    None
}

pub fn builtin_defs() -> Vec<Builtin> {
    vec![
        Builtin {
            name: "assert",
            func: assert,
        },
        Builtin {
            name: "assert_eq",
            func: assert_eq,
        },
        Builtin {
            name: "dbg",
            func: dbg,
        },
        Builtin {
            name: "print",
            func: print,
        },
        Builtin {
            name: "println",
            func: println,
        },
    ]
}
