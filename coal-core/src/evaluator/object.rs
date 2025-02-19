mod r#struct;

use std::{
    cmp::Ordering,
    collections::HashMap,
    fmt,
    hash::{DefaultHasher, Hash, Hasher},
    ops::{Add, Div, Mul, Rem, Sub},
};

pub use r#struct::StructObj;

use crate::{
    indent, Expr, Func, Literal, Param, ParserError, Span, Stmt, StructDecl, Type, F32, F64, I128,
    I32, I64, U32, U64,
};

use super::{Builtin, RuntimeError, RuntimeErrorKind};

#[derive(Clone, Debug, PartialEq)]
pub enum Object {
    U32(u32),
    U64(u64),
    I32(i32),
    I64(i64),
    I128(i128),
    F32(f32),
    F64(f64),
    Str(String),
    Bool(bool),
    Range(usize, usize),
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
        args: Vec<Param>,
        body: Vec<Stmt>,
        ret_t: Type,
    },
    Closure {
        args: Vec<Param>,
        body: Vec<Stmt>,
        ret_t: Type,
    },
    StructDecl(StructDecl),
    Struct(StructObj),
    Builtin(Builtin),
    Return(Box<Object>),
    Type(Type),
    Error(RuntimeError),
    Nil,
    Void,
}

pub const TRUE: Object = Object::Bool(true);
pub const FALSE: Object = Object::Bool(false);

impl Object {
    pub fn is_truthy(&self) -> bool {
        !matches!(self, Object::Bool(false) | Object::Nil)
    }

    pub fn is_fn(&self) -> bool {
        matches!(self, Object::Fn { .. } | Object::Closure { .. })
    }

    pub fn call(&mut self, name: &str, args: &[Object], span: &Span) -> Option<Object> {
        if let Err(e) = self.validate_call_args(name, args, span) {
            return Some(e);
        }

        if name == "to_s" {
            return Some(Object::Str(self.to_string()));
        }

        match self {
            // Object::U32(_)
            // | Object::U64(_)
            // | Object::I32(_)
            // | Object::I64(_)
            // | Object::I128(_)
            // | Object::F32(_)
            // | Object::F64(_) => match name {
            //     _ => None,
            // },
            Object::Str(s) => match name {
                "len" => Some(Object::U64(s.len() as u64)),
                "split" => {
                    if let Object::Str(ch) = &args[0] {
                        Some(Object::List {
                            data: s
                                .split(ch.as_str())
                                .map(|s| Object::Str(s.to_owned()))
                                .collect(),
                            t: Type::Str,
                        })
                    } else {
                        None
                    }
                }
                _ => Some(Object::Error(RuntimeError::new(
                    RuntimeErrorKind::MethodNotFound(name.to_owned()),
                    *span,
                ))),
            },
            Object::List { data, t } => match name {
                "len" => Some(Object::U64(data.len() as u64)),
                "push" => {
                    let new_t = Type::from(&args[0]);
                    if new_t != *t {
                        return Some(Object::Error(RuntimeError::new(
                            RuntimeErrorKind::TypeMismatch(t.to_owned(), new_t),
                            *span,
                        )));
                    }

                    data.push(args[0].to_owned());

                    None
                }
                "pop" => {
                    if data.is_empty() {
                        None
                    } else {
                        Some(data.pop().unwrap())
                    }
                }
                "get" => {
                    let t = Type::from(&args[0]);

                    if let Some(Object::I64(idx)) = args[0].cast(&I64) {
                        if idx < 0 {
                            data.get((data.len() as i64 + idx) as usize)
                                .cloned()
                                .or(Some(Object::Nil))
                        } else {
                            data.get(idx as usize).cloned().or(Some(Object::Nil))
                        }
                    } else {
                        Some(Object::Error(RuntimeError::new(
                            RuntimeErrorKind::TypeMismatch(U64, t),
                            *span,
                        )))
                    }
                }
                "first" => data.first().cloned().or(Some(Object::Nil)),
                "last" => data.last().cloned().or(Some(Object::Nil)),
                "join" => {
                    if let Object::Str(sep) = &args[0] {
                        let result = data
                            .iter()
                            .map(|item| item.to_string())
                            .collect::<Vec<_>>()
                            .join(sep);

                        Some(Object::Str(result))
                    } else {
                        Some(Object::Error(RuntimeError::new(
                            RuntimeErrorKind::TypeMismatch(Type::Str, Type::from(&args[0])),
                            *span,
                        )))
                    }
                }
                _ => Some(Object::Error(RuntimeError::new(
                    RuntimeErrorKind::MethodNotFound(name.to_owned()),
                    *span,
                ))),
            },
            Object::Map { data, t: (kt, _vt) } => match name {
                "get" => {
                    let idx_t = Type::from(&args[0]);
                    if idx_t == *kt {
                        data.get(&args[0]).cloned().or(Some(Object::Nil))
                    } else if let Some(idx) = args[0].cast(kt) {
                        data.get(&idx).cloned().or(Some(Object::Nil))
                    } else {
                        Some(Object::Error(RuntimeError::new(
                            RuntimeErrorKind::TypeMismatch(idx_t, kt.clone()),
                            *span,
                        )))
                    }
                }
                _ => Some(Object::Error(RuntimeError::new(
                    RuntimeErrorKind::MethodNotFound(name.to_owned()),
                    *span,
                ))),
            },
            _ => Some(Object::Error(RuntimeError::new(
                RuntimeErrorKind::MethodNotFound(name.to_owned()),
                *span,
            ))),
        }
    }

    pub fn validate_call_args(
        &self,
        name: &str,
        args: &[Object],
        span: &Span,
    ) -> Result<(), Object> {
        if let Some(method) = Type::from(self).sig(name) {
            for (arg, t) in args.iter().zip(method.args_t.iter()) {
                let arg_t = Type::from(arg);
                if arg_t != *t {
                    if arg.cast(t).is_some() {
                        continue;
                    }

                    let args_t = args
                        .iter()
                        .map(|arg| Type::from(arg).to_string())
                        .collect::<Vec<_>>()
                        .join(", ");

                    return Err(Object::Error(RuntimeError::new(
                        RuntimeErrorKind::InvalidArguments(method.args_str(), args_t),
                        *span,
                    )));
                }
            }

            Ok(())
        } else {
            Err(Object::Error(RuntimeError::new(
                RuntimeErrorKind::MethodNotFound(name.to_owned()),
                *span,
            )))
        }
    }

    pub fn calculate_hash(&self) -> String {
        let mut hasher = DefaultHasher::new();
        self.hash(&mut hasher);
        let hash = hasher.finish();
        format!("0x{:x?}", hash)
    }

    pub fn cast(&self, to: &Type) -> Option<Object> {
        match (self, to) {
            // u32
            (Object::U32(from), &U64) => Some(Object::U64(*from as u64)),
            (Object::U32(from), &I32) => Some(Object::I32(*from as i32)),
            (Object::U32(from), &I64) => Some(Object::I64(*from as i64)),
            (Object::U32(from), &I128) => Some(Object::I128(*from as i128)),
            (Object::U32(from), &F32) => Some(Object::F32(*from as f32)),
            (Object::U32(from), &F64) => Some(Object::F64(*from as f64)),

            // u64
            (Object::U64(from), &U32) => Some(Object::U32(*from as u32)),
            (Object::U64(from), &I32) => Some(Object::I32(*from as i32)),
            (Object::U64(from), &I64) => Some(Object::I64(*from as i64)),
            (Object::U64(from), &I128) => Some(Object::I128(*from as i128)),
            (Object::U64(from), &F32) => Some(Object::F32(*from as f32)),
            (Object::U64(from), &F64) => Some(Object::F64(*from as f64)),

            // i32
            (Object::I32(from), &U32) => Some(Object::U32(*from as u32)),
            (Object::I32(from), &U64) => Some(Object::U64(*from as u64)),
            (Object::I32(from), &I64) => Some(Object::I64(*from as i64)),
            (Object::I32(from), &I128) => Some(Object::I128(*from as i128)),
            (Object::I32(from), &F32) => Some(Object::F32(*from as f32)),
            (Object::I32(from), &F64) => Some(Object::F64(*from as f64)),

            // i64
            (Object::I64(from), &U32) => Some(Object::U32(*from as u32)),
            (Object::I64(from), &U64) => Some(Object::U64(*from as u64)),
            (Object::I64(from), &I32) => Some(Object::I32(*from as i32)),
            (Object::I64(from), &I128) => Some(Object::I128(*from as i128)),
            (Object::I64(from), &F32) => Some(Object::F32(*from as f32)),
            (Object::I64(from), &F64) => Some(Object::F64(*from as f64)),

            // i128
            (Object::I128(from), &U32) => Some(Object::U32(*from as u32)),
            (Object::I128(from), &U64) => Some(Object::U64(*from as u64)),
            (Object::I128(from), &I32) => Some(Object::I32(*from as i32)),
            (Object::I128(from), &I64) => Some(Object::I64(*from as i64)),
            (Object::I128(from), &F32) => Some(Object::F32(*from as f32)),
            (Object::I128(from), &F64) => Some(Object::F64(*from as f64)),

            // f32
            (Object::F32(from), &U32) => Some(Object::U32(*from as u32)),
            (Object::F32(from), &U64) => Some(Object::U64(*from as u64)),
            (Object::F32(from), &I32) => Some(Object::I32(*from as i32)),
            (Object::F32(from), &I64) => Some(Object::I64(*from as i64)),
            (Object::F32(from), &I128) => Some(Object::I128(*from as i128)),
            (Object::F32(from), &F64) => Some(Object::F64(*from as f64)),

            // f64
            (Object::F64(from), &U32) => Some(Object::U32(*from as u32)),
            (Object::F64(from), &U64) => Some(Object::U64(*from as u64)),
            (Object::F64(from), &I32) => Some(Object::I32(*from as i32)),
            (Object::F64(from), &I64) => Some(Object::I64(*from as i64)),
            (Object::F64(from), &I128) => Some(Object::I128(*from as i128)),
            (Object::F64(from), &F32) => Some(Object::F32(*from as f32)),

            (Object::List { data, t }, Type::List(t2)) => {
                if data.is_empty() && !t.is_defined() {
                    Some(Object::List {
                        data: vec![],
                        t: *t2.clone(),
                    })
                } else {
                    None
                }
            }
            (Object::Map { data, t }, Type::Map(t2)) => {
                if data.is_empty() && !(t.0.is_defined() && t.1.is_defined()) {
                    Some(Object::Map {
                        data: HashMap::new(),
                        t: (t2.0.clone(), t2.1.clone()),
                    })
                } else {
                    None
                }
            }

            _ => None,
        }
    }

    fn promote(lhs: &Object, rhs: &Object) -> (Object, Object) {
        match (lhs, rhs) {
            // u32
            (Object::U32(lhs), Object::U64(rhs)) => (Object::U64(*lhs as u64), Object::U64(*rhs)),
            (Object::U32(lhs), Object::I32(rhs)) => (Object::I32(*lhs as i32), Object::I32(*rhs)),
            (Object::U32(lhs), Object::I64(rhs)) => (Object::I64(*lhs as i64), Object::I64(*rhs)),
            (Object::U32(lhs), Object::I128(rhs)) => {
                (Object::I128(*lhs as i128), Object::I128(*rhs))
            }
            (Object::U32(lhs), Object::F32(rhs)) => (Object::F32(*lhs as f32), Object::F32(*rhs)),
            (Object::U32(lhs), Object::F64(rhs)) => (Object::F64(*lhs as f64), Object::F64(*rhs)),

            // u64
            (Object::U64(lhs), Object::U32(rhs)) => (Object::U64(*lhs), Object::U64(*rhs as u64)),
            (Object::U64(lhs), Object::I32(rhs)) => {
                (Object::I64(*lhs as i64), Object::I64(*rhs as i64))
            }
            (Object::U64(lhs), Object::I64(rhs)) => (Object::I64(*lhs as i64), Object::I64(*rhs)),
            (Object::U64(lhs), Object::I128(rhs)) => {
                (Object::I128(*lhs as i128), Object::I128(*rhs))
            }
            (Object::U64(lhs), Object::F32(rhs)) => (Object::F32(*lhs as f32), Object::F32(*rhs)),
            (Object::U64(lhs), Object::F64(rhs)) => (Object::F64(*lhs as f64), Object::F64(*rhs)),

            // i32
            (Object::I32(lhs), Object::U32(rhs)) => (Object::I32(*lhs), Object::I32(*rhs as i32)),
            (Object::I32(lhs), Object::U64(rhs)) => {
                (Object::I64(*lhs as i64), Object::I64(*rhs as i64))
            }
            (Object::I32(lhs), Object::I64(rhs)) => (Object::I64(*lhs as i64), Object::I64(*rhs)),
            (Object::I32(lhs), Object::I128(rhs)) => {
                (Object::I128(*lhs as i128), Object::I128(*rhs))
            }
            (Object::I32(lhs), Object::F32(rhs)) => (Object::F32(*lhs as f32), Object::F32(*rhs)),
            (Object::I32(lhs), Object::F64(rhs)) => (Object::F64(*lhs as f64), Object::F64(*rhs)),

            // i64
            (Object::I64(lhs), Object::U32(rhs)) => (Object::I64(*lhs), Object::I64(*rhs as i64)),
            (Object::I64(lhs), Object::U64(rhs)) => (Object::I64(*lhs), Object::I64(*rhs as i64)),
            (Object::I64(lhs), Object::I32(rhs)) => (Object::I64(*lhs), Object::I64(*rhs as i64)),
            (Object::I64(lhs), Object::I128(rhs)) => {
                (Object::I128(*lhs as i128), Object::I128(*rhs))
            }
            (Object::I64(lhs), Object::F32(rhs)) => (Object::F32(*lhs as f32), Object::F32(*rhs)),
            (Object::I64(lhs), Object::F64(rhs)) => (Object::F64(*lhs as f64), Object::F64(*rhs)),

            // i128
            (Object::I128(lhs), Object::U32(rhs)) => {
                (Object::I128(*lhs), Object::I128(*rhs as i128))
            }
            (Object::I128(lhs), Object::U64(rhs)) => {
                (Object::I128(*lhs), Object::I128(*rhs as i128))
            }
            (Object::I128(lhs), Object::I32(rhs)) => {
                (Object::I128(*lhs), Object::I128(*rhs as i128))
            }
            (Object::I128(lhs), Object::I64(rhs)) => {
                (Object::I128(*lhs), Object::I128(*rhs as i128))
            }
            (Object::I128(lhs), Object::F32(rhs)) => (Object::F32(*lhs as f32), Object::F32(*rhs)),
            (Object::I128(lhs), Object::F64(rhs)) => (Object::F64(*lhs as f64), Object::F64(*rhs)),

            // f32
            (Object::F32(lhs), Object::U32(rhs)) => (Object::F32(*lhs), Object::F32(*rhs as f32)),
            (Object::F32(lhs), Object::U64(rhs)) => (Object::F32(*lhs), Object::F32(*rhs as f32)),
            (Object::F32(lhs), Object::I32(rhs)) => (Object::F32(*lhs), Object::F32(*rhs as f32)),
            (Object::F32(lhs), Object::I64(rhs)) => (Object::F32(*lhs), Object::F32(*rhs as f32)),
            (Object::F32(lhs), Object::I128(rhs)) => (Object::F32(*lhs), Object::F32(*rhs as f32)),
            (Object::F32(lhs), Object::F64(rhs)) => (Object::F32(*lhs), Object::F32(*rhs as f32)),

            // f64
            (Object::F64(lhs), Object::U32(rhs)) => (Object::F64(*lhs), Object::F64(*rhs as f64)),
            (Object::F64(lhs), Object::U64(rhs)) => (Object::F64(*lhs), Object::F64(*rhs as f64)),
            (Object::F64(lhs), Object::I32(rhs)) => (Object::F64(*lhs), Object::F64(*rhs as f64)),
            (Object::F64(lhs), Object::I64(rhs)) => (Object::F64(*lhs), Object::F64(*rhs as f64)),
            (Object::F64(lhs), Object::I128(rhs)) => (Object::F64(*lhs), Object::F64(*rhs as f64)),

            _ => (lhs.clone(), rhs.clone()),
        }
    }

    pub fn to_string_raw(&self) -> String {
        match self {
            Object::Str(s) => s.to_owned(),
            _ => self.to_string(),
        }
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
            Literal::Bool(b) => Object::from(*b),
            Literal::Str(s) => Object::Str(s.clone()),
            Literal::U32(i) => Object::U32(*i),
            Literal::U64(i) => Object::U64(*i),
            Literal::I32(i) => Object::I32(*i),
            Literal::I64(i) => Object::I64(*i),
            Literal::I128(i) => Object::I128(*i),
            Literal::F32(f) => Object::F32(*f),
            Literal::F64(f) => Object::F64(*f),
            Literal::List(l) => Object::List {
                data: l.data.iter().map(Object::from).collect(),
                t: l.t.clone(),
            },
            Literal::Map(m) => Object::Map {
                data: m
                    .data
                    .iter()
                    .map(|(k, v)| (Object::from(k), Object::from(v)))
                    .collect(),
                t: m.t.clone(),
            },
            Literal::Nil => Object::Nil,
        }
    }
}

impl From<&Expr> for Object {
    fn from(expr: &Expr) -> Self {
        match expr {
            Expr::Literal(l, _) => Object::from(l),
            _ => Object::Nil,
        }
    }
}

impl From<&Func> for Object {
    fn from(value: &Func) -> Self {
        Object::Fn {
            name: value.name.clone(),
            args: value.args.clone(),
            body: value.body.clone(),
            ret_t: value.ret_t.clone(),
        }
    }
}

impl From<&StructDecl> for Object {
    fn from(sd: &StructDecl) -> Self {
        Object::StructDecl(sd.clone())
    }
}

impl From<&ParserError> for Object {
    fn from(value: &ParserError) -> Self {
        Object::Error(RuntimeError {
            kind: RuntimeErrorKind::Custom(value.kind.to_string()),
            span: value.span,
        })
    }
}

impl Hash for Object {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Object::U32(i) => i.hash(state),
            Object::U64(i) => i.hash(state),
            Object::I32(i) => i.hash(state),
            Object::I64(i) => i.hash(state),
            Object::I128(i) => i.hash(state),
            Object::F32(f) => f.to_bits().hash(state),
            Object::F64(f) => f.to_bits().hash(state),
            Object::Bool(b) => b.hash(state),
            Object::Str(s) => s.hash(state),
            Object::Range(start, end) => {
                start.hash(state);
                end.hash(state);
            }
            Object::List { data, .. } => {
                data.len().hash(state);
                data.hash(state);
            }
            Object::Map { data, .. } => {
                data.len().hash(state);
                for (k, v) in data {
                    k.hash(state);
                    v.hash(state);
                }
            }
            Object::Fn { name, args, .. } => {
                name.hash(state);
                args.len().hash(state);
                args.hash(state);
            }
            Object::Closure { args, .. } => {
                args.len().hash(state);
                args.hash(state);
            }
            Object::StructDecl(StructDecl { name, attrs, funcs }) => {
                name.hash(state);
                attrs.len().hash(state);
                for (p, _) in attrs {
                    p.hash(state);
                }
                funcs.len().hash(state);
                for f in funcs {
                    f.hash(state);
                }
            }
            Object::Struct(StructObj { name, attrs, funcs }) => {
                name.hash(state);
                attrs.len().hash(state);
                for (k, v) in attrs.iter().chain(funcs.iter()) {
                    k.hash(state);
                    v.hash(state);
                }
            }
            Object::Builtin(b) => b.func.hash(state),
            Object::Return(v) => v.hash(state),
            Object::Type(t) => t.hash(state),
            Object::Error(e) => e.hash(state),
            Object::Nil => 0.hash(state),
            Object::Void => 0.hash(state),
        }
    }
}

impl Add for Object {
    type Output = Option<Object>;

    fn add(self, rhs: Self) -> Self::Output {
        match Object::promote(&self, &rhs) {
            (Object::Str(s1), Object::Str(s2)) => Some(Object::Str(format!("{s1}{s2}"))),
            (Object::U32(lhs), Object::U32(rhs)) => Some(Object::U32(lhs + rhs)),
            (Object::U64(lhs), Object::U64(rhs)) => Some(Object::U64(lhs + rhs)),
            (Object::I32(lhs), Object::I32(rhs)) => Some(Object::I32(lhs + rhs)),
            (Object::I64(lhs), Object::I64(rhs)) => Some(Object::I64(lhs + rhs)),
            (Object::I128(lhs), Object::I128(rhs)) => Some(Object::I128(lhs + rhs)),
            (Object::F32(lhs), Object::F32(rhs)) => Some(Object::F32(lhs + rhs)),
            (Object::F64(lhs), Object::F64(rhs)) => Some(Object::F64(lhs + rhs)),
            _ => None,
        }
    }
}

impl Sub for Object {
    type Output = Option<Object>;

    fn sub(self, rhs: Self) -> Self::Output {
        match Object::promote(&self, &rhs) {
            (Object::U32(lhs), Object::U32(rhs)) => Some(Object::U32(lhs - rhs)),
            (Object::U64(lhs), Object::U64(rhs)) => Some(Object::U64(lhs - rhs)),
            (Object::I32(lhs), Object::I32(rhs)) => Some(Object::I32(lhs - rhs)),
            (Object::I64(lhs), Object::I64(rhs)) => Some(Object::I64(lhs - rhs)),
            (Object::I128(lhs), Object::I128(rhs)) => Some(Object::I128(lhs - rhs)),
            (Object::F32(lhs), Object::F32(rhs)) => Some(Object::F32(lhs - rhs)),
            (Object::F64(lhs), Object::F64(rhs)) => Some(Object::F64(lhs - rhs)),
            _ => None,
        }
    }
}

impl Mul for Object {
    type Output = Option<Object>;

    fn mul(self, rhs: Self) -> Self::Output {
        match Object::promote(&self, &rhs) {
            (Object::Str(lhs), Object::U32(rhs)) => Some(Object::Str(lhs.repeat(rhs as usize))),
            (Object::Str(lhs), Object::U64(rhs)) => Some(Object::Str(lhs.repeat(rhs as usize))),
            (Object::Str(lhs), Object::I32(rhs)) => Some(Object::Str(lhs.repeat(rhs as usize))),
            (Object::Str(lhs), Object::I64(rhs)) => Some(Object::Str(lhs.repeat(rhs as usize))),
            (Object::Str(lhs), Object::I128(rhs)) => Some(Object::Str(lhs.repeat(rhs as usize))),
            (Object::U32(lhs), Object::U32(rhs)) => Some(Object::U32(lhs * rhs)),
            (Object::U64(lhs), Object::U64(rhs)) => Some(Object::U64(lhs * rhs)),
            (Object::I32(lhs), Object::I32(rhs)) => Some(Object::I32(lhs * rhs)),
            (Object::I64(lhs), Object::I64(rhs)) => Some(Object::I64(lhs * rhs)),
            (Object::I128(lhs), Object::I128(rhs)) => Some(Object::I128(lhs * rhs)),
            (Object::F32(lhs), Object::F32(rhs)) => Some(Object::F32(lhs * rhs)),
            (Object::F64(lhs), Object::F64(rhs)) => Some(Object::F64(lhs * rhs)),
            _ => None,
        }
    }
}

impl Div for Object {
    type Output = Option<Object>;

    fn div(self, rhs: Self) -> Self::Output {
        match Object::promote(&self, &rhs) {
            (Object::U32(lhs), Object::U32(rhs)) => Some(Object::U32(lhs / rhs)),
            (Object::U64(lhs), Object::U64(rhs)) => Some(Object::U64(lhs / rhs)),
            (Object::I32(lhs), Object::I32(rhs)) => Some(Object::I32(lhs / rhs)),
            (Object::I64(lhs), Object::I64(rhs)) => Some(Object::I64(lhs / rhs)),
            (Object::I128(lhs), Object::I128(rhs)) => Some(Object::I128(lhs / rhs)),
            (Object::F32(lhs), Object::F32(rhs)) => Some(Object::F32(lhs / rhs)),
            (Object::F64(lhs), Object::F64(rhs)) => Some(Object::F64(lhs / rhs)),
            _ => None,
        }
    }
}

impl Rem for Object {
    type Output = Option<Object>;

    fn rem(self, rhs: Self) -> Self::Output {
        match Object::promote(&self, &rhs) {
            (Object::U32(lhs), Object::U32(rhs)) => Some(Object::U32(lhs % rhs)),
            (Object::U64(lhs), Object::U64(rhs)) => Some(Object::U64(lhs % rhs)),
            (Object::I32(lhs), Object::I32(rhs)) => Some(Object::I32(lhs % rhs)),
            (Object::I64(lhs), Object::I64(rhs)) => Some(Object::I64(lhs % rhs)),
            (Object::I128(lhs), Object::I128(rhs)) => Some(Object::I128(lhs % rhs)),
            _ => None,
        }
    }
}

impl PartialOrd for Object {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match Object::promote(self, other) {
            (Object::Bool(lhs), Object::Bool(rhs)) => lhs.partial_cmp(&rhs),
            (Object::Str(lhs), Object::Str(rhs)) => lhs.partial_cmp(&rhs),
            (Object::U32(lhs), Object::U32(rhs)) => lhs.partial_cmp(&rhs),
            (Object::U64(lhs), Object::U64(rhs)) => lhs.partial_cmp(&rhs),
            (Object::I32(lhs), Object::I32(rhs)) => lhs.partial_cmp(&rhs),
            (Object::I64(lhs), Object::I64(rhs)) => lhs.partial_cmp(&rhs),
            (Object::I128(lhs), Object::I128(rhs)) => lhs.partial_cmp(&rhs),
            (Object::F32(lhs), Object::F32(rhs)) => lhs.partial_cmp(&rhs),
            (Object::F64(lhs), Object::F64(rhs)) => lhs.partial_cmp(&rhs),
            _ => None,
        }
    }
}

impl TryInto<usize> for Object {
    type Error = ();

    fn try_into(self) -> Result<usize, ()> {
        match self {
            Object::U32(i) => Ok(i as usize),
            Object::U64(i) => Ok(i as usize),
            Object::I32(i) => Ok(i as usize),
            Object::I64(i) => Ok(i as usize),
            Object::I128(i) => Ok(i as usize),
            _ => Err(()),
        }
    }
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let base_indent = indent(1);

        match self {
            Object::U32(i) => write!(f, "{i}"),
            Object::U64(i) => write!(f, "{i}"),
            Object::I32(i) => write!(f, "{i}"),
            Object::I64(i) => write!(f, "{i}"),
            Object::I128(i) => write!(f, "{i}"),
            Object::F32(x) => write!(f, "{x:?}"),
            Object::F64(x) => write!(f, "{x:?}"),
            Object::Str(s) => write!(f, "\"{s}\""),
            Object::Bool(b) => write!(f, "{b}"),
            Object::Range(start, end) => write!(f, "({start}..{end})"),
            Object::List { data, .. } => match data.len() {
                0 => write!(f, "[]"),
                1 | 2 => {
                    let v = data
                        .iter()
                        .map(|x| match x {
                            Object::Str(s) => format!("\"{s}\""),
                            _ => x.to_string(),
                        })
                        .collect::<Vec<_>>()
                        .join(", ");
                    write!(f, "[{v}]")
                }
                _ => {
                    writeln!(f, "[")?;
                    for i in data {
                        writeln!(f, "{}{i},", base_indent)?;
                    }
                    write!(f, "]")
                }
            },
            Object::Map { data, .. } => match data.len() {
                0 => write!(f, "{{}}"),
                1 | 2 => {
                    let v = data
                        .iter()
                        .map(|(k, v)| format!("{k}: {v}"))
                        .collect::<Vec<_>>()
                        .join(", ");
                    write!(f, "{{{v}}}")
                }
                _ => {
                    writeln!(f, "{{")?;
                    for (k, v) in data {
                        writeln!(f, "{}{k}: {v}", base_indent)?;
                    }
                    write!(f, "}}")
                }
            },
            Object::Fn { .. } => write!(f, "<fn_{}>", self.calculate_hash()),
            Object::Closure { .. } => write!(f, "<closure_{}>", self.calculate_hash()),
            Object::StructDecl { .. } => write!(f, "<struct_{}>", self.calculate_hash()),
            Object::Struct(StructObj { name, attrs, .. }) => {
                write!(f, "{name} {{")?;
                match attrs.len() {
                    0 => {}
                    1 | 2 => {
                        let v = attrs
                            .iter()
                            .map(|(k, v)| format!("{k}: {v}"))
                            .collect::<Vec<String>>()
                            .join(", ");
                        write!(f, " {v} ")?;
                    }
                    _ => {
                        writeln!(f)?;
                        for (k, v) in attrs {
                            writeln!(f, "{}{k}: {v},", base_indent)?;
                        }
                    }
                }
                write!(f, "}}")
            }
            Object::Builtin(_) => write!(f, "<builtin_{}>", self.calculate_hash()),
            Object::Return(v) => write!(f, "{v}"),
            Object::Type(t) => write!(f, "{t}"),
            Object::Error(e) => write!(f, "{e}"),
            Object::Nil => write!(f, "nil"),
            Object::Void => Ok(()),
        }
    }
}
