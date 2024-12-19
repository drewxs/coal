use std::{borrow::Borrow, fmt};

use crate::{Object, Token};

use super::{Expr, Literal};

#[derive(Clone, Debug, Default, PartialEq)]
pub enum Type {
    Bool,
    Str,
    Num(Num),
    List(Box<Type>),
    Map(Box<(Type, Type)>),
    Fn(Vec<Type>, Box<Type>),
    UserDefined(String),
    #[default]
    Nil,
    Void,
    Unknown,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
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
            Object::List { t, .. } => Type::List(Box::new(t.clone())),
            Object::Map { t, .. } => Type::Map(Box::new(t.clone())),
            Object::Fn { args, ret_t, .. } => Type::Fn(
                args.iter().map(|arg| arg.t.clone()).collect(),
                Box::new(ret_t.clone()),
            ),
            Object::Nil => Type::Nil,
            _ => Type::Void,
        }
    }
}

impl TryFrom<&Token> for Type {
    type Error = String;

    fn try_from(token: &Token) -> Result<Self, Self::Error> {
        match token {
            Token::Ident(name) => match name.as_str() {
                "u32" => Ok(U32),
                "u64" => Ok(U64),
                "i32" => Ok(I32),
                "i64" => Ok(I64),
                "i128" => Ok(I128),
                "f32" => Ok(F32),
                "f64" => Ok(F64),
                "str" => Ok(Type::Str),
                "bool" => Ok(Type::Bool),
                _ => Ok(Type::UserDefined(name.clone())),
            },
            Token::U32(_) => Ok(U32),
            Token::U64(_) => Ok(U64),
            Token::I32(_) => Ok(I32),
            Token::I64(_) => Ok(I64),
            Token::I128(_) => Ok(I128),
            Token::F32(_) => Ok(F32),
            Token::F64(_) => Ok(F64),
            _ => Err(String::from("invalid type token")),
        }
    }
}

impl TryFrom<&Expr> for Type {
    type Error = String;

    fn try_from(literal: &Expr) -> Result<Self, Self::Error> {
        match literal {
            Expr::Literal(Literal::Str(_), _) => Ok(Type::Str),
            Expr::Literal(Literal::U32(_), _) => Ok(U32),
            Expr::Literal(Literal::U64(_), _) => Ok(U64),
            Expr::Literal(Literal::I32(_), _) => Ok(I32),
            Expr::Literal(Literal::I64(_), _) => Ok(I64),
            Expr::Literal(Literal::I128(_), _) => Ok(I128),
            Expr::Literal(Literal::F32(_), _) => Ok(F32),
            Expr::Literal(Literal::F64(_), _) => Ok(F64),
            Expr::Literal(Literal::Bool(_), _) => Ok(Type::Bool),
            Expr::Infix(_, lhs, rhs, _) => {
                infer_infix_type(lhs, rhs).ok_or(String::from("unable to infer type"))
            }
            Expr::Prefix(_, rhs, _) => Type::try_from(Borrow::<Expr>::borrow(rhs)),
            Expr::Ident(ident, _) => Ok(Type::UserDefined(ident.name())),
            Expr::Fn { ret_t, .. } => Ok(ret_t.clone()),
            Expr::Call { func, .. } => Type::try_from(Borrow::<Expr>::borrow(func)),
            _ => Err(String::from("invaild literal")),
        }
    }
}

fn infer_infix_type(lhs: &Expr, rhs: &Expr) -> Option<Type> {
    let lhs_t = infer_expr_type(lhs)?;
    let rhs_t = infer_expr_type(rhs)?;

    match lhs_t {
        Type::Num(lhs_num) => match rhs_t {
            Type::Num(rhs_num) => Some(Type::Num(lhs_num.max(rhs_num))),
            _ => None,
        },
        _ => None,
    }
}

fn infer_expr_type(expr: &Expr) -> Option<Type> {
    match expr {
        Expr::Infix(_, lhs, rhs, _) => infer_infix_type(lhs, rhs),
        Expr::Prefix(_, rhs, _) => match rhs.borrow() {
            Expr::Infix(_, i_lhs, i_rhs, _) => infer_infix_type(i_lhs, i_rhs),
            Expr::Literal(Literal::I64(_), _) => Some(I64),
            _ => None,
        },
        Expr::Literal(Literal::I64(_), _) => Some(I64),
        Expr::Call { func, .. } => infer_expr_type(func),
        Expr::Fn { ret_t, .. } => Some(ret_t.clone()),
        _ => None,
    }
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
            Type::Map(t) => {
                let (k, v) = t.as_ref();
                write!(f, "map[{k}, {v}]")
            }
            Type::Fn(args, ret_t) => {
                let args_str = args
                    .iter()
                    .map(|arg| format!("{arg}"))
                    .collect::<Vec<String>>()
                    .join(", ");
                write!(f, "Fn({args_str}) -> {ret_t}")
            }
            Type::UserDefined(name) => write!(f, "{}", name),
            Type::Nil => write!(f, "nil"),
            Type::Void => write!(f, "void"),
            Type::Unknown => write!(f, "unknown"),
        }
    }
}
