use std::{borrow::Borrow, fmt};

use crate::{Object, Span, Token};

use super::{Expr, Ident, Infix, Literal, Stmt};

#[derive(Clone, Debug, PartialEq, Default)]
pub enum Type {
    Str,
    I64,
    F64,
    Bool,
    List(Box<Type>),
    Map(Box<(Type, Type)>),
    Function(Vec<Ident>, Vec<Stmt>),
    UserDefined(String),
    #[default]
    Nil,
    Unknown,
}

impl From<&Object> for Type {
    fn from(obj: &Object) -> Self {
        match obj {
            Object::I64(_) => Type::I64,
            Object::F64(_) => Type::F64,
            Object::Str(_) => Type::Str,
            Object::Bool(_) => Type::Bool,
            Object::List { t, .. } => Type::List(Box::new(t.clone())),
            Object::Map { t, .. } => Type::Map(Box::new(t.clone())),
            Object::Fn { ret_t, .. } => ret_t.clone(),
            _ => Type::Nil,
        }
    }
}

impl TryFrom<&Token> for Type {
    type Error = String;

    fn try_from(token: &Token) -> Result<Self, Self::Error> {
        match token {
            Token::Ident(name) => match name.as_str() {
                "i64" => Ok(Type::I64),
                "f64" => Ok(Type::F64),
                "str" => Ok(Type::Str),
                "bool" => Ok(Type::Bool),
                _ => Ok(Type::UserDefined(name.clone())),
            },
            _ => Err(String::from("invalid type token")),
        }
    }
}

impl TryFrom<&Expr> for Type {
    type Error = String;

    fn try_from(literal: &Expr) -> Result<Self, Self::Error> {
        match literal {
            Expr::Literal(Literal::Str(_), _) => Ok(Type::Str),
            Expr::Literal(Literal::I64(_), _) => Ok(Type::I64),
            Expr::Literal(Literal::F64(_), _) => Ok(Type::F64),
            Expr::Literal(Literal::Bool(_), _) => Ok(Type::Bool),
            Expr::Infix(op, lhs, rhs, span) => {
                infer_infix_type(op, lhs, rhs, span).ok_or(String::from("unable to infer type"))
            }
            Expr::Prefix(_, rhs, _) => Type::try_from(Borrow::<Expr>::borrow(rhs)),
            Expr::Ident(ident, _) => Ok(Type::UserDefined(ident.name())),
            Expr::Fn { ret_t, .. } => Ok(ret_t.clone()),
            Expr::Call { func, .. } => Type::try_from(Borrow::<Expr>::borrow(func)),
            _ => Err(String::from("invaild literal")),
        }
    }
}

fn infer_infix_type(op: &Infix, lhs: &Expr, rhs: &Expr, _span: &Span) -> Option<Type> {
    let lhs_t = infer_expr_type(lhs)?;
    let rhs_t = infer_expr_type(rhs)?;

    match lhs_t {
        Type::I64 => match rhs_t {
            Type::I64 => match op {
                Infix::Div => Some(Type::F64),
                _ => Some(Type::I64),
            },
            Type::F64 => Some(Type::F64),
            _ => None,
        },
        Type::F64 => match rhs_t {
            Type::I64 | Type::F64 => Some(Type::F64),
            _ => None,
        },
        _ => None,
    }
}

fn infer_expr_type(expr: &Expr) -> Option<Type> {
    match expr {
        Expr::Infix(op, lhs, rhs, span) => infer_infix_type(op, lhs, rhs, span),
        Expr::Prefix(_, rhs, _) => match rhs.borrow() {
            Expr::Infix(i_op, i_lhs, i_rhs, i_span) => infer_infix_type(i_op, i_lhs, i_rhs, i_span),
            Expr::Literal(Literal::I64(_), _) => Some(Type::I64),
            _ => None,
        },
        Expr::Literal(Literal::I64(_), _) => Some(Type::I64),
        Expr::Call { func, .. } => infer_expr_type(func),
        Expr::Fn { ret_t, .. } => Some(ret_t.clone()),
        _ => None,
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Type::Str => write!(f, "str"),
            Type::I64 => write!(f, "i64"),
            Type::F64 => write!(f, "f64"),
            Type::Bool => write!(f, "bool"),
            Type::List(t) => write!(f, "list[{t}]"),
            Type::Map(t) => {
                let (k, v) = t.as_ref();
                write!(f, "map[{k}, {v}]")
            }
            Type::Function(idents, stmts) => write!(f, "fn({idents:?}) {{{stmts:?}}}"),
            Type::UserDefined(name) => write!(f, "{}", name),
            Type::Nil => write!(f, "nil"),
            Type::Unknown => write!(f, "unknown"),
        }
    }
}
