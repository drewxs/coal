use std::{
    cmp::Ordering,
    collections::HashMap,
    fmt,
    hash::{DefaultHasher, Hash, Hasher},
    ops::{Add, Div, Mul, Rem, Sub},
    rc::Rc,
};

use coal_core::{BaseType, Expr, F32, F64, I32, I64, I128, Literal, U32, U64, ast, indent};

use crate::{Builtin, Closure, CompiledFunc, Constant, Func, Struct};

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
    List(Vec<Rc<Object>>),
    Map(HashMap<Rc<Object>, Rc<Object>>),
    Builtin(Builtin),
    Func(Func),
    CompiledFunc(CompiledFunc),
    Closure(Closure),
    Struct(Struct),
    Return(Rc<Object>),
    Type(BaseType),
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
        matches!(self, Object::Func { .. } | Object::Closure { .. })
    }

    pub fn is_int(&self) -> bool {
        matches!(
            self,
            Object::U32(_) | Object::U64(_) | Object::I32(_) | Object::I64(_) | Object::I128(_)
        )
    }

    pub fn is_float(&self) -> bool {
        matches!(self, Object::F32(_) | Object::F64(_))
    }

    pub fn is_numeric(&self) -> bool {
        self.is_int() || self.is_float()
    }

    pub fn is_hashable(&self) -> bool {
        self.is_int() || matches!(self, Object::Bool(_) | Object::Str(_))
    }

    pub fn call(&mut self, name: &str, args: &[Rc<Object>]) -> Option<Rc<Object>> {
        if name == "to_s" {
            return Some(Rc::new(Object::Str(self.to_string())));
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
                "len" => Some(Rc::new(Object::U64(s.len() as u64))),
                "split" => {
                    if let Object::Str(ch) = &*args[0] {
                        Some(Rc::new(Object::List(
                            s.split(ch.as_str())
                                .map(|s| Rc::new(Object::Str(s.to_owned())))
                                .collect(),
                        )))
                    } else {
                        None
                    }
                }
                _ => None,
            },
            Object::List(data) => match name {
                "len" => Some(Rc::new(Object::U64(data.len() as u64))),
                "push" => {
                    data.push(Rc::new((*args[0]).clone()));
                    None
                }
                "pop" => data.pop(),
                "get" => {
                    let idx = match *args[0] {
                        Object::I64(i) => i,
                        _ => match args[0].cast(&I64) {
                            Some(Object::I64(i)) => i,
                            _ => return None,
                        },
                    };

                    if idx < 0 {
                        data.get((data.len() as i64 + idx) as usize)
                            .cloned()
                            .or(Some(Rc::new(Object::Nil)))
                    } else {
                        data.get(idx as usize)
                            .cloned()
                            .or(Some(Rc::new(Object::Nil)))
                    }
                }
                "first" => data.first().cloned().or(Some(Rc::new(Object::Nil))),
                "last" => data.last().cloned().or(Some(Rc::new(Object::Nil))),
                "join" => {
                    let Object::Str(sep) = &*args[0] else {
                        return None;
                    };

                    let result = data
                        .iter()
                        .map(|item| item.to_string())
                        .collect::<Vec<_>>()
                        .join(sep);

                    Some(Rc::new(Object::Str(result)))
                }
                "clear" => {
                    data.clear();
                    None
                }
                _ => None,
            },
            Object::Map(data) => match name {
                "len" => Some(Rc::new(Object::U64(data.len() as u64))),
                "get" => data.get(&args[0]).cloned().or(Some(Rc::new(Object::Nil))),
                "remove" => {
                    data.remove(&args[0]);
                    None
                }
                "clear" => {
                    data.clear();
                    None
                }
                _ => None,
            },
            _ => None,
        }
    }

    pub fn calculate_hash(&self) -> String {
        let mut hasher = DefaultHasher::new();
        self.hash(&mut hasher);
        let hash = hasher.finish();
        format!("0x{hash:x?}")
    }

    pub fn cast(&self, to: &BaseType) -> Option<Object> {
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

            (Object::List(_), _) => Some(self.clone()),
            (Object::Map(_), _) => Some(self.clone()),

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
        if b { TRUE } else { FALSE }
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
            Literal::List(l) => {
                Object::List(l.data.iter().map(|e| Rc::new(Object::from(e))).collect())
            }
            Literal::Map(m) => Object::Map(
                m.data
                    .iter()
                    .map(|(k, v)| (Rc::new(Object::from(k)), Rc::new(Object::from(v))))
                    .collect(),
            ),
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

impl From<&ast::Func> for Object {
    fn from(value: &ast::Func) -> Self {
        Object::Func(Func {
            name: value.name.clone(),
            args: value.args.iter().map(|a| a.name.clone()).collect(),
            body: value.body.clone(),
        })
    }
}

impl From<Constant> for Object {
    fn from(c: Constant) -> Self {
        match c {
            Constant::U32(i) => Object::U32(i),
            Constant::U64(i) => Object::U64(i),
            Constant::I32(i) => Object::I32(i),
            Constant::I64(i) => Object::I64(i),
            Constant::I128(i) => Object::I128(i),
            Constant::F32(f) => Object::F32(f),
            Constant::F64(f) => Object::F64(f),
            Constant::Str(s) => Object::Str(s),
            Constant::Func(f) => Object::CompiledFunc(f),
        }
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
            Object::List(data) => {
                data.len().hash(state);
                data.hash(state);
            }
            Object::Map(data) => {
                data.len().hash(state);
                for (k, v) in data {
                    k.hash(state);
                    v.hash(state);
                }
            }
            Object::Builtin(b) => b.hash(state),
            Object::Func(f) => f.hash(state),
            Object::CompiledFunc(f) => f.hash(state),
            Object::Closure(cl) => cl.hash(state),
            Object::Struct(s) => s.hash(state),
            Object::Return(v) => v.hash(state),
            Object::Type(t) => t.hash(state),
            Object::Nil => 0.hash(state),
            Object::Void => 0.hash(state),
        }
    }
}

impl Add for Object {
    type Output = Object;

    fn add(self, rhs: Self) -> Self::Output {
        match Object::promote(&self, &rhs) {
            (Object::Str(s1), Object::Str(s2)) => Object::Str(format!("{s1}{s2}")),
            (Object::U32(lhs), Object::U32(rhs)) => Object::U32(lhs + rhs),
            (Object::U64(lhs), Object::U64(rhs)) => Object::U64(lhs + rhs),
            (Object::I32(lhs), Object::I32(rhs)) => Object::I32(lhs + rhs),
            (Object::I64(lhs), Object::I64(rhs)) => Object::I64(lhs + rhs),
            (Object::I128(lhs), Object::I128(rhs)) => Object::I128(lhs + rhs),
            (Object::F32(lhs), Object::F32(rhs)) => Object::F32(lhs + rhs),
            (Object::F64(lhs), Object::F64(rhs)) => Object::F64(lhs + rhs),
            _ => unreachable!(),
        }
    }
}

impl Sub for Object {
    type Output = Object;

    fn sub(self, rhs: Self) -> Self::Output {
        match Object::promote(&self, &rhs) {
            (Object::U32(lhs), Object::U32(rhs)) => Object::U32(lhs - rhs),
            (Object::U64(lhs), Object::U64(rhs)) => Object::U64(lhs - rhs),
            (Object::I32(lhs), Object::I32(rhs)) => Object::I32(lhs - rhs),
            (Object::I64(lhs), Object::I64(rhs)) => Object::I64(lhs - rhs),
            (Object::I128(lhs), Object::I128(rhs)) => Object::I128(lhs - rhs),
            (Object::F32(lhs), Object::F32(rhs)) => Object::F32(lhs - rhs),
            (Object::F64(lhs), Object::F64(rhs)) => Object::F64(lhs - rhs),
            _ => unreachable!(),
        }
    }
}

impl Mul for Object {
    type Output = Object;

    fn mul(self, rhs: Self) -> Self::Output {
        match Object::promote(&self, &rhs) {
            (Object::Str(lhs), Object::U32(rhs)) => Object::Str(lhs.repeat(rhs as usize)),
            (Object::Str(lhs), Object::U64(rhs)) => Object::Str(lhs.repeat(rhs as usize)),
            (Object::Str(lhs), Object::I32(rhs)) => Object::Str(lhs.repeat(rhs as usize)),
            (Object::Str(lhs), Object::I64(rhs)) => Object::Str(lhs.repeat(rhs as usize)),
            (Object::Str(lhs), Object::I128(rhs)) => Object::Str(lhs.repeat(rhs as usize)),
            (Object::U32(lhs), Object::U32(rhs)) => Object::U32(lhs * rhs),
            (Object::U64(lhs), Object::U64(rhs)) => Object::U64(lhs * rhs),
            (Object::I32(lhs), Object::I32(rhs)) => Object::I32(lhs * rhs),
            (Object::I64(lhs), Object::I64(rhs)) => Object::I64(lhs * rhs),
            (Object::I128(lhs), Object::I128(rhs)) => Object::I128(lhs * rhs),
            (Object::F32(lhs), Object::F32(rhs)) => Object::F32(lhs * rhs),
            (Object::F64(lhs), Object::F64(rhs)) => Object::F64(lhs * rhs),
            _ => unreachable!(),
        }
    }
}

impl Div for Object {
    type Output = Object;

    fn div(self, rhs: Self) -> Self::Output {
        match Object::promote(&self, &rhs) {
            (Object::U32(lhs), Object::U32(rhs)) => Object::U32(lhs / rhs),
            (Object::U64(lhs), Object::U64(rhs)) => Object::U64(lhs / rhs),
            (Object::I32(lhs), Object::I32(rhs)) => Object::I32(lhs / rhs),
            (Object::I64(lhs), Object::I64(rhs)) => Object::I64(lhs / rhs),
            (Object::I128(lhs), Object::I128(rhs)) => Object::I128(lhs / rhs),
            (Object::F32(lhs), Object::F32(rhs)) => Object::F32(lhs / rhs),
            (Object::F64(lhs), Object::F64(rhs)) => Object::F64(lhs / rhs),
            _ => unreachable!(),
        }
    }
}

impl Rem for Object {
    type Output = Object;

    fn rem(self, rhs: Self) -> Self::Output {
        match Object::promote(&self, &rhs) {
            (Object::U32(lhs), Object::U32(rhs)) => Object::U32(lhs % rhs),
            (Object::U64(lhs), Object::U64(rhs)) => Object::U64(lhs % rhs),
            (Object::I32(lhs), Object::I32(rhs)) => Object::I32(lhs % rhs),
            (Object::I64(lhs), Object::I64(rhs)) => Object::I64(lhs % rhs),
            (Object::I128(lhs), Object::I128(rhs)) => Object::I128(lhs % rhs),
            _ => unreachable!(),
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
            Object::List(data) => {
                if data.is_empty() {
                    write!(f, "[]")
                } else {
                    let data_str = data
                        .iter()
                        .map(|e| e.to_string())
                        .collect::<Vec<_>>()
                        .join(", ");
                    let width = data_str.len() + base_indent.len();

                    if width > 80 {
                        writeln!(f, "[")?;
                        for item in data {
                            writeln!(f, "{base_indent}{item},")?;
                        }
                        write!(f, "]")
                    } else {
                        write!(f, "[")?;
                        for (i, item) in data.iter().enumerate() {
                            write!(f, "{item}")?;
                            if i < data.len() - 1 {
                                write!(f, ", ")?;
                            }
                        }
                        write!(f, "]")
                    }
                }
            }
            Object::Map(data) => match data.len() {
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
                        writeln!(f, "{base_indent}{k}: {v}")?;
                    }
                    write!(f, "}}")
                }
            },
            Object::Builtin(_) => write!(f, "<builtin_{}>", self.calculate_hash()),
            Object::Func { .. } => write!(f, "<fn_{}>", self.calculate_hash()),
            Object::CompiledFunc(_) => write!(f, "<compiled_fn_{}>", self.calculate_hash()),
            Object::Closure { .. } => write!(f, "<closure_{}>", self.calculate_hash()),
            Object::Struct(Struct { name, attrs, .. }) => {
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
                            writeln!(f, "{base_indent}{k}: {v},")?;
                        }
                    }
                }
                write!(f, "}}")
            }
            Object::Return(v) => write!(f, "{v}"),
            Object::Type(t) => write!(f, "{t}"),
            Object::Nil => write!(f, "nil"),
            Object::Void => Ok(()),
        }
    }
}
