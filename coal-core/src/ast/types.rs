use std::{borrow::Borrow, fmt};

use crate::{Object, Token};

use super::{Expr, Ident, Infix, Literal, Stmt};

#[derive(Clone, Debug, PartialEq, Default)]
pub enum Type {
    Str,
    Int,
    Float,
    Bool,
    List(Box<Type>),
    Map(Box<(Type, Type)>),
    Function(Vec<Ident>, Vec<Stmt>),
    UserDefined(String),
    #[default]
    Nil,
}

impl From<&Object> for Type {
    fn from(obj: &Object) -> Self {
        match obj {
            Object::Int(_) => Type::Int,
            Object::Float(_) => Type::Float,
            Object::Str(_) => Type::Str,
            Object::Bool(_) => Type::Bool,
            Object::List { t, .. } => Type::List(Box::new(t.clone())),
            Object::Map { t, .. } => Type::Map(Box::new(t.clone())),
            Object::Fn { t: ret_t, .. } => ret_t.clone(),
            _ => Type::Nil,
        }
    }
}

impl TryFrom<&Token> for Type {
    type Error = String;

    fn try_from(token: &Token) -> Result<Self, Self::Error> {
        match token {
            Token::Ident(name) => match name.as_str() {
                "int" => Ok(Type::Int),
                "float" => Ok(Type::Float),
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
            Expr::Literal(Literal::Int(_), _) => Ok(Type::Int),
            Expr::Literal(Literal::Float(_), _) => Ok(Type::Float),
            Expr::Literal(Literal::Bool(_), _) => Ok(Type::Bool),
            Expr::Infix(op, lhs, _, _) => {
                if *op == Infix::Div {
                    Ok(Type::Float)
                } else {
                    Type::try_from(Borrow::<Expr>::borrow(lhs))
                }
            }
            Expr::Prefix(_, rhs, _) => Type::try_from(Borrow::<Expr>::borrow(rhs)),
            Expr::Ident(ident, _) => Ok(Type::UserDefined(ident.name())),
            Expr::Fn { ret_t, .. } => Ok(ret_t.clone()),
            Expr::Call { func, .. } => Type::try_from(Borrow::<Expr>::borrow(func)),
            _ => Err(String::from("invaild literal")),
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Type::Str => write!(f, "str"),
            Type::Int => write!(f, "int"),
            Type::Float => write!(f, "float"),
            Type::Bool => write!(f, "bool"),
            Type::List(t) => write!(f, "list[{t}]"),
            Type::Map(t) => {
                let (k, v) = t.as_ref();
                write!(f, "map[{k}, {v}]")
            }
            Type::Function(idents, stmts) => write!(f, "fn({idents:?}) {{{stmts:?}}}"),
            Type::UserDefined(name) => write!(f, "{}", name),
            Type::Nil => write!(f, "nil"),
        }
    }
}
