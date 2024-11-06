use std::fmt;

use crate::Token;

use super::{Expr, Ident, Literal, Stmt};

#[derive(Clone, Debug, PartialEq, Default)]
pub enum Type {
    String,
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

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::String => write!(f, "str"),
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

impl TryFrom<&Token> for Type {
    type Error = String;

    fn try_from(token: &Token) -> Result<Self, Self::Error> {
        match token {
            Token::IntType => Ok(Type::Int),
            Token::FloatType => Ok(Type::Float),
            Token::StrType => Ok(Type::String),
            Token::BoolType => Ok(Type::Bool),
            Token::Ident(name) => Ok(Type::UserDefined(name.clone())),
            _ => Err(String::from("invalid type token")),
        }
    }
}

impl TryFrom<&Expr> for Type {
    type Error = String;

    fn try_from(literal: &Expr) -> Result<Self, Self::Error> {
        match literal {
            Expr::Literal(Literal::Str(_)) => Ok(Type::String),
            Expr::Literal(Literal::Int(_)) => Ok(Type::Int),
            Expr::Literal(Literal::Float(_)) => Ok(Type::Float),
            Expr::Literal(Literal::Bool(_)) => Ok(Type::Bool),
            Expr::Ident(ident) => Ok(Type::UserDefined(ident.name())),
            _ => Err(String::from("invaild literal")),
        }
    }
}
