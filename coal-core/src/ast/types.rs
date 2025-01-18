use std::fmt;

use crate::{Object, TokenKind};

use super::{Expr, Literal, Prefix};

#[derive(Clone, Debug, Hash, PartialEq, Default)]
pub enum Type {
    Bool,
    Str,
    Num(Num),
    List(Box<Type>),
    Map(Box<(Type, Type)>),
    Fn(Vec<Type>, Box<Type>),
    Range,
    UserDefined(String, Box<Type>),
    Nil,
    Any,
    Void,
    #[default]
    Unknown,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Num {
    U32,
    U64,
    I32,
    I64,
    I128,
    F32,
    F64,
}

pub const U32: Type = Type::Num(Num::U32);
pub const U64: Type = Type::Num(Num::U64);
pub const I32: Type = Type::Num(Num::I32);
pub const I64: Type = Type::Num(Num::I64);
pub const I128: Type = Type::Num(Num::I128);
pub const F32: Type = Type::Num(Num::F32);
pub const F64: Type = Type::Num(Num::F64);

#[derive(Clone, Debug, Hash, PartialEq)]
pub struct MethodSignature {
    pub args_t: Vec<Type>,
    pub ret_t: Type,
}

impl MethodSignature {
    pub fn new(args_t: &[Type], ret_t: Type) -> Self {
        MethodSignature {
            args_t: args_t.to_owned(),
            ret_t,
        }
    }

    pub fn args_str(&self) -> String {
        self.args_t
            .iter()
            .map(|arg| arg.to_string())
            .collect::<Vec<String>>()
            .join(", ")
    }
}

impl Type {
    pub fn is_numeric(&self) -> bool {
        matches!(self, Type::Num(_))
    }

    pub fn is_hashable(&self) -> bool {
        matches!(self, Type::Bool | Type::Str | Type::Num(_))
    }

    pub fn is_defined(&self) -> bool {
        match self {
            Type::List(t) => t.is_defined(),
            Type::Map(t) => t.0.is_defined() && t.1.is_defined(),
            Type::Unknown => false,
            _ => true,
        }
    }

    pub fn is_composite(&self) -> bool {
        match self {
            Type::UserDefined(_, t) => t.is_composite(),
            Type::List(_) | Type::Map(_) | Type::Fn(_, _) => true,
            _ => false,
        }
    }

    pub fn extract(&self) -> &Self {
        match self {
            Type::List(t) => t,
            t => t,
        }
    }

    pub fn sig(&self, method: &str) -> Option<MethodSignature> {
        match self {
            Type::Num(_) => self.num_sig(method),
            Type::Str => self.str_sig(method),
            Type::List(t) => self.list_sig(method, t),
            Type::Map(t) => self.map_sig(method, t),
            _ => None,
        }
    }

    fn num_sig(&self, method: &str) -> Option<MethodSignature> {
        match method {
            "to_s" => Some(MethodSignature::new(&[], Type::Str)),
            _ => None,
        }
    }

    fn str_sig(&self, method: &str) -> Option<MethodSignature> {
        match method {
            "len" => Some(MethodSignature::new(&[], U64)),
            "split" => Some(MethodSignature::new(
                &[Type::Str],
                Type::List(Box::new(Type::Str)),
            )),
            _ => None,
        }
    }

    fn list_sig(&self, method: &str, t: &Type) -> Option<MethodSignature> {
        match method {
            "len" => Some(MethodSignature::new(&[], U64)),
            "push" => Some(MethodSignature::new(&[t.clone()], Type::Void)),
            "pop" => Some(MethodSignature::new(&[], t.clone())),
            "get" => Some(MethodSignature::new(&[I64], t.clone())),
            "first" => Some(MethodSignature::new(&[], t.clone())),
            "last" => Some(MethodSignature::new(&[], t.clone())),
            "join" => Some(MethodSignature::new(&[Type::Str], Type::Str)),
            "map" => Some(MethodSignature::new(
                &[Type::Fn(vec![t.clone()], Box::new(t.clone()))],
                Type::List(Box::new(Type::Unknown)),
            )),
            _ => None,
        }
    }

    fn map_sig(&self, method: &str, (kt, vt): &(Type, Type)) -> Option<MethodSignature> {
        match method {
            "get" => Some(MethodSignature::new(&[kt.clone()], vt.clone())),
            _ => None,
        }
    }
}

impl From<&Object> for Type {
    fn from(obj: &Object) -> Self {
        match obj {
            Object::U32(_) => U32,
            Object::U64(_) => U64,
            Object::I32(_) => I32,
            Object::I64(_) => I64,
            Object::I128(_) => I128,
            Object::F32(_) => F32,
            Object::F64(_) => F64,
            Object::Str(_) => Type::Str,
            Object::Bool(_) => Type::Bool,
            Object::List { t, .. } => Type::List(Box::new(t.to_owned())),
            Object::Map { t, .. } => Type::Map(Box::new(t.to_owned())),
            Object::Fn { args, ret_t, .. } | Object::Closure { args, ret_t, .. } => Type::Fn(
                args.iter().map(|arg| arg.t.to_owned()).collect(),
                Box::new(ret_t.to_owned()),
            ),
            Object::Nil => Type::Nil,
            _ => Type::Void,
        }
    }
}

impl From<&Literal> for Type {
    fn from(literal: &Literal) -> Self {
        match literal {
            Literal::Str(_) => Type::Str,
            Literal::U32(_) => U32,
            Literal::U64(_) => U64,
            Literal::I32(_) => I32,
            Literal::I64(_) => I64,
            Literal::I128(_) => I128,
            Literal::F32(_) => F32,
            Literal::F64(_) => F64,
            Literal::Bool(_) => Type::Bool,
            Literal::List(_, t, _) => Type::List(Box::new(t.to_owned())),
            Literal::Map(_, t) => Type::Map(Box::new(t.to_owned())),
            Literal::Nil => Type::Nil,
        }
    }
}

impl TryFrom<&TokenKind> for Type {
    type Error = Type;

    fn try_from(token: &TokenKind) -> Result<Self, Self::Error> {
        match token {
            TokenKind::Ident(name) => match name.as_str() {
                "u32" => Ok(U32),
                "u64" => Ok(U64),
                "i32" => Ok(I32),
                "i64" => Ok(I64),
                "i128" => Ok(I128),
                "f32" => Ok(F32),
                "f64" => Ok(F64),
                "str" => Ok(Type::Str),
                "bool" => Ok(Type::Bool),
                "range" => Ok(Type::Range),
                "nil" => Ok(Type::Nil),
                "any" => Ok(Type::Any),
                "void" => Ok(Type::Void),
                _ => Err(Type::Unknown),
            },
            TokenKind::U32(_) => Ok(U32),
            TokenKind::U64(_) => Ok(U64),
            TokenKind::I32(_) => Ok(I32),
            TokenKind::I64(_) => Ok(I64),
            TokenKind::I128(_) => Ok(I128),
            TokenKind::F32(_) => Ok(F32),
            TokenKind::F64(_) => Ok(F64),
            _ => Err(Type::Unknown),
        }
    }
}

impl TryFrom<&Expr> for Type {
    type Error = Type;

    fn try_from(expr: &Expr) -> Result<Self, Self::Error> {
        match expr {
            Expr::Ident(_, t, _) => Ok(t.to_owned()),
            Expr::Literal(l, _) => Ok(Type::from(l)),
            Expr::Prefix(prefix, rhs, _) => match prefix {
                Prefix::Not => Ok(Type::Bool),
                _ => Type::try_from(&**rhs),
            },
            Expr::Infix(_, lhs, rhs, _) => infer_infix_type(lhs, rhs),
            Expr::Index(expr, _, _) => match Type::try_from(&**expr) {
                Ok(Type::List(t)) => Ok(*t),
                Ok(Type::Map(t)) => Ok((*t).1),
                Ok(t) | Err(t) => Err(t),
            },
            Expr::Range(_, _, _) => Ok(U64),
            Expr::Fn { args, ret_t, .. } | Expr::Closure { args, ret_t, .. } => Ok(Type::Fn(
                args.iter().map(|arg| arg.t.to_owned()).collect(),
                Box::new(ret_t.to_owned()),
            )),
            Expr::Call { ret_t, .. } => Ok(ret_t.to_owned()),
            Expr::MethodCall { ret_t, .. } => Ok(ret_t.to_owned()),
            _ => Err(Type::Unknown),
        }
    }
}

fn infer_infix_type(lhs: &Expr, rhs: &Expr) -> Result<Type, Type> {
    let lhs_t = Type::try_from(lhs)?;
    let rhs_t = Type::try_from(rhs)?;

    if let Type::Num(lhs_num) = &lhs_t {
        if let Type::Num(rhs_num) = &rhs_t {
            if lhs_t == I32 && rhs_t == U64 || lhs_t == U64 && rhs_t == I32 {
                return Ok(I64);
            }
            return Ok(Type::Num(lhs_num.max(rhs_num).clone()));
        }
    }

    Err(Type::Unknown)
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Type::Bool => write!(f, "bool"),
            Type::Str => write!(f, "str"),
            &U32 => write!(f, "u32"),
            &U64 => write!(f, "u64"),
            &I32 => write!(f, "i32"),
            &I64 => write!(f, "i64"),
            &I128 => write!(f, "i128"),
            &F32 => write!(f, "f32"),
            &F64 => write!(f, "f64"),
            Type::List(t) => write!(f, "list[{t}]"),
            Type::Map(t) => write!(f, "map[{}, {}]", t.0, t.1),
            Type::Fn(args, ret_t) => {
                let args_str = args
                    .iter()
                    .map(|arg| format!("{arg}"))
                    .collect::<Vec<String>>()
                    .join(", ");
                if **ret_t == Type::Void {
                    write!(f, "Fn({args_str})")
                } else {
                    write!(f, "Fn({args_str}) -> {ret_t}")
                }
            }
            Type::Range => write!(f, "range"),
            Type::UserDefined(name, _) => write!(f, "{name}"),
            Type::Nil => write!(f, "nil"),
            Type::Any => write!(f, "any"),
            Type::Void => write!(f, "void"),
            Type::Unknown => write!(f, "unknown"),
        }
    }
}
