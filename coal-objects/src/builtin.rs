use std::{collections::HashMap, rc::Rc};

use crate::Object;

#[derive(Clone, Debug, PartialEq, Hash)]
pub struct Builtin(fn(&[Rc<Object>]) -> Option<Rc<Object>>);

fn assert(args: &[Rc<Object>]) -> Option<Rc<Object>> {
    assert!(args[0].is_truthy());
    None
}

fn assert_eq(args: &[Rc<Object>]) -> Option<Rc<Object>> {
    assert_eq!(args[0], args[1]);
    None
}

fn dbg(args: &[Rc<Object>]) -> Option<Rc<Object>> {
    println!("{:?}", args[0]);
    None
}

fn print(args: &[Rc<Object>]) -> Option<Rc<Object>> {
    print!("{}", args[0]);
    None
}

fn println(args: &[Rc<Object>]) -> Option<Rc<Object>> {
    println!("{}", args[0]);
    None
}

pub fn builtin_defs<'s>() -> HashMap<&'s str, Builtin> {
    let mut builtins = HashMap::new();

    builtins.insert("assert", Builtin(assert));
    builtins.insert("assert_eq", Builtin(assert_eq));
    builtins.insert("dbg", Builtin(dbg));
    builtins.insert("print", Builtin(print));
    builtins.insert("println", Builtin(println));

    builtins
}
