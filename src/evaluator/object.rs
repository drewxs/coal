use core::fmt;
use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::{cell::RefCell, rc::Rc};

use crate::{Expr, Literal, Stmt, Type};

use super::Env;

#[derive(Clone, Debug, PartialEq)]
pub enum Object {
    Int(i64),
    Float(f64),
    Str(String),
    Bool(bool),
    List {
        data: Vec<Object>,
        t: Type,
    },
    Map {
        data: HashMap<Object, Object>,
        t: (Type, Type),
    },
    Fn {
        name: String,
        args: Vec<Expr>,
        body: Vec<Stmt>,
        env: Rc<RefCell<Env>>,
        t: Type,
    },
    Nil,
    Error(String),
}

pub const NIL: Object = Object::Nil;
pub const TRUE: Object = Object::Bool(true);
pub const FALSE: Object = Object::Bool(false);

impl Eq for Object {}

impl Hash for Object {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Object::Int(i) => i.hash(state),
            Object::Float(f) => f.to_bits().hash(state),
            Object::Str(s) => s.hash(state),
            Object::Bool(b) => b.hash(state),
            _ => "".hash(state),
        }
    }
}

impl Object {
    pub fn fmt_with_indent(&self, f: &mut fmt::Formatter, indent_level: usize) -> fmt::Result {
        let indent = "    ".repeat(indent_level);

        match self {
            Object::Int(i) => write!(f, "{i}"),
            Object::Float(x) => write!(f, "{x}"),
            Object::Str(s) => write!(f, "{s}"),
            Object::Bool(b) => write!(f, "{b}"),
            Object::List { data, .. } => write!(f, "{data:?}"),
            Object::Map { data, .. } => write!(f, "{data:?}"),
            Object::Fn {
                name,
                args,
                body,
                t,
                ..
            } => {
                let args_str = args
                    .iter()
                    .map(|arg| format!("{arg}"))
                    .collect::<Vec<String>>()
                    .join(", ");
                write!(f, "{name}({args_str}) -> {t} {{")?;
                for stmt in body {
                    writeln!(f, "{indent}    {stmt}")?;
                }
                write!(f, "}}")
            }
            Object::Nil => write!(f, "nil"),
            Object::Error(e) => write!(f, "{e}"),
        }
    }
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.fmt_with_indent(f, 0)
    }
}

impl From<bool> for Object {
    fn from(b: bool) -> Self {
        if b {
            TRUE
        } else {
            FALSE
        }
    }
}

impl From<&Literal> for Object {
    fn from(literal: &Literal) -> Self {
        match literal {
            Literal::Str(s) => Object::Str(s.clone()),
            Literal::Int(i) => Object::Int(*i),
            Literal::Float(f) => Object::Float(*f),
            Literal::Bool(b) => Object::from(*b),
            Literal::Nil => Object::Nil,
        }
    }
}
