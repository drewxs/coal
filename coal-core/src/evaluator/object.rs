use std::{
    cmp::Ordering,
    collections::HashMap,
    fmt,
    hash::{DefaultHasher, Hash, Hasher},
    ops::{Add, Div, Mul, Rem, Sub},
};

use crate::{
    Expr, Literal, ParserError, Span, Stmt, Type, Var, F32, F64, I128, I32, I64, U32, U64,
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
    Builtin(Builtin),
    Return(Box<Object>),
    Error(RuntimeError),
    Nil,
}

pub const TRUE: Object = Object::Bool(true);
pub const FALSE: Object = Object::Bool(false);

impl Object {
    pub fn is_truthy(&self) -> bool {
        !matches!(self, Object::Bool(false) | Object::Nil)
    }

    pub fn call(&mut self, name: &str, args: &[Object], span: &Span) -> Option<Object> {
        if let Err(e) = self.validate_call_args(name, args, span) {
            return Some(e);
        }

        match self {
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

                    if let Some(Object::U64(idx)) = args[0].cast(&U64) {
                        data.get(idx as usize).cloned().or(Some(Object::Nil))
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
            _ => Some(Object::Error(RuntimeError::new(
                RuntimeErrorKind::MethodNotFound(name.to_owned()),
                *span,
            ))),
        }
    }

    fn validate_call_args(&self, name: &str, args: &[Object], span: &Span) -> Result<(), Object> {
        if let Some(method) = Type::from(self).sig(name) {
            for (arg, t) in args.iter().zip(method.args_t.iter()) {
                if Type::from(arg) != *t {
                    if arg.cast(t).is_some() {
                        continue;
                    }

                    let method_args_t = method
                        .args_t
                        .iter()
                        .map(|arg| arg.to_string())
                        .collect::<Vec<_>>()
                        .join(", ");
                    let args_t = args
                        .iter()
                        .map(|arg| Type::from(arg).to_string())
                        .collect::<Vec<_>>()
                        .join(", ");

                    return Err(Object::Error(RuntimeError::new(
                        RuntimeErrorKind::InvalidArguments(method_args_t, args_t),
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

    pub fn int_div(self, rhs: Self) -> Option<Object> {
        match Object::promote(&self, &rhs) {
            (Object::U32(lhs), Object::U32(rhs)) => Some(Object::U32(lhs / rhs)),
            (Object::U64(lhs), Object::U64(rhs)) => Some(Object::U64(lhs / rhs)),
            (Object::I32(lhs), Object::I32(rhs)) => Some(Object::I32(lhs / rhs)),
            (Object::I64(lhs), Object::I64(rhs)) => Some(Object::I64(lhs / rhs)),
            (Object::I128(lhs), Object::I128(rhs)) => Some(Object::I128(lhs / rhs)),
            (Object::F32(lhs), Object::F32(rhs)) => Some(Object::F32((lhs / rhs).floor())),
            (Object::F64(lhs), Object::F64(rhs)) => Some(Object::F64((lhs / rhs).floor())),
            _ => None,
        }
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
            Literal::List(l, t) => Object::List {
                data: l.iter().map(Object::from).collect(),
                t: t.clone(),
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
            Object::List { data, .. } => data.hash(state),
            Object::Map { .. } => "map".hash(state),
            Object::Fn { name, .. } => name.hash(state),
            Object::Builtin(b) => b.func.hash(state),
            Object::Nil => {}
            Object::Return(v) => v.hash(state),
            Object::Error(e) => e.hash(state),
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
        match (self, other) {
            (Object::Bool(lhs), Object::Bool(rhs)) => lhs.partial_cmp(rhs),
            (Object::Str(lhs), Object::Str(rhs)) => lhs.partial_cmp(rhs),
            (Object::U32(lhs), Object::U32(rhs)) => lhs.partial_cmp(rhs),
            (Object::U64(lhs), Object::U64(rhs)) => lhs.partial_cmp(rhs),
            (Object::I32(lhs), Object::I32(rhs)) => lhs.partial_cmp(rhs),
            (Object::I64(lhs), Object::I64(rhs)) => lhs.partial_cmp(rhs),
            (Object::I128(lhs), Object::I128(rhs)) => lhs.partial_cmp(rhs),
            (Object::F32(lhs), Object::F32(rhs)) => lhs.partial_cmp(rhs),
            (Object::F64(lhs), Object::F64(rhs)) => lhs.partial_cmp(rhs),
            _ => None,
        }
    }
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Object::U32(i) => write!(f, "{i}"),
            Object::U64(i) => write!(f, "{i}"),
            Object::I32(i) => write!(f, "{i}"),
            Object::I64(i) => write!(f, "{i}"),
            Object::I128(i) => write!(f, "{i}"),
            Object::F32(x) => write!(f, "{x:?}"),
            Object::F64(x) => write!(f, "{x:?}"),
            Object::Str(s) => write!(f, "{s}"),
            Object::Bool(b) => write!(f, "{b}"),
            Object::List { data, .. } => {
                let items = data
                    .iter()
                    .map(|x| match x {
                        Object::Str(s) => format!("\"{s}\""),
                        _ => x.to_string(),
                    })
                    .collect::<Vec<_>>()
                    .join(", ");

                write!(f, "[{items}]")
            }
            Object::Map { data, .. } => write!(f, "{data:?}"),
            Object::Fn { .. } => write!(f, "<fn_{}>", self.calculate_hash()),
            Object::Builtin(_) => write!(f, "<builtin_{}>", self.calculate_hash()),
            Object::Return(v) => write!(f, "{v}"),
            Object::Error(e) => write!(f, "{e}"),
            Object::Nil => write!(f, "nil"),
        }
    }
}
