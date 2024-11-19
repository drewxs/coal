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
    String(String),
    Bool(bool),
    Vec(Vec<Object>),
    Map(HashMap<Object, Object>),
    Fn {
        name: String,
        args: Vec<Expr>,
        ret_t: Type,
        body: Vec<Stmt>,
        env: Rc<RefCell<Env>>,
    },
    Nil,
    Error(String),
}

impl Eq for Object {}

impl Hash for Object {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Object::Int(i) => i.hash(state),
            Object::Float(f) => f.to_bits().hash(state),
            Object::String(s) => s.hash(state),
            Object::Bool(b) => b.hash(state),
            _ => "".hash(state),
        }
    }
}

impl Object {
    pub fn fmt_with_indent(&self, f: &mut fmt::Formatter<'_>, indent_level: usize) -> fmt::Result {
        let indent = "    ".repeat(indent_level);

        match self {
            Object::Int(i) => write!(f, "{i}"),
            Object::Float(x) => write!(f, "{x}"),
            Object::String(s) => write!(f, "{s}"),
            Object::Bool(b) => write!(f, "{b}"),
            Object::Vec(v) => write!(f, "{v:?}"),
            Object::Map(m) => write!(f, "{m:?}"),
            Object::Fn {
                name,
                args,
                body,
                ret_t,
                ..
            } => {
                let args_str = args
                    .iter()
                    .map(|arg| format!("{arg}"))
                    .collect::<Vec<String>>()
                    .join(", ");
                write!(f, "{name}({args_str}) -> {ret_t} {{")?;
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
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.fmt_with_indent(f, 0)
    }
}

impl From<&Literal> for Object {
    fn from(literal: &Literal) -> Self {
        match literal {
            Literal::Str(s) => Object::String(s.clone()),
            Literal::Int(i) => Object::Int(*i),
            Literal::Float(f) => Object::Float(*f),
            Literal::Bool(b) => Object::Bool(*b),
            Literal::Nil => Object::Nil,
        }
    }
}