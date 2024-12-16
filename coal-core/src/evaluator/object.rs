use std::{
    collections::HashMap,
    fmt,
    hash::{DefaultHasher, Hash, Hasher},
};

use crate::{Literal, ParserError, Span, Stmt, Type, Var};

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
        args: Vec<Var>,
        body: Vec<Stmt>,
        ret_t: Type,
    },
    Nil,
    Return(Box<Object>),
    Error {
        message: String,
        span: Span,
    },
}

pub const TRUE: Object = Object::Bool(true);
pub const FALSE: Object = Object::Bool(false);

impl Object {
    pub fn is_truthy(&self) -> bool {
        !matches!(self, Object::Bool(false) | Object::Nil)
    }

    pub fn calculate_hash(&self) -> u64 {
        let mut hasher = DefaultHasher::new();
        self.hash(&mut hasher);
        hasher.finish()
    }
}

impl Eq for Object {}

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

impl From<&ParserError> for Object {
    fn from(value: &ParserError) -> Self {
        Object::Error {
            message: value.kind.to_string(),
            span: value.span,
        }
    }
}

impl Hash for Object {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Object::Int(i) => i.hash(state),
            Object::Float(f) => f.to_bits().hash(state),
            Object::Str(s) => s.hash(state),
            Object::Bool(b) => b.hash(state),
            Object::List { data, .. } => data.hash(state),
            Object::Map { .. } => "map".hash(state),
            Object::Fn { name, .. } => name.hash(state),
            Object::Nil => {}
            Object::Return(v) => v.hash(state),
            Object::Error { message, span } => {
                message.hash(state);
                span.hash(state);
            }
        }
    }
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Object::Int(i) => write!(f, "{i}"),
            Object::Float(x) => write!(f, "{x:?}"),
            Object::Str(s) => write!(f, "{s}"),
            Object::Bool(b) => write!(f, "{b}"),
            Object::List { data, .. } => write!(f, "{data:?}"),
            Object::Map { data, .. } => write!(f, "{data:?}"),
            Object::Fn { .. } => write!(f, "Fn[{}]", self.calculate_hash()),
            Object::Nil => write!(f, "nil"),
            Object::Return(v) => write!(f, "{v}"),
            Object::Error {
                message,
                span: trace,
            } => {
                let ((l1, c1), (l2, c2)) = trace;
                write!(f, "{l1}:{c1}-{l2}:{c2} {}", message)
            }
        }
    }
}
