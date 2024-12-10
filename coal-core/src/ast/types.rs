use std::{borrow::Borrow, fmt};

use crate::{Object, Span, Token};

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
            Expr::Infix(op, lhs, rhs, span) => Ok(infer_infix_type(op, lhs, rhs, span)),
            Expr::Prefix(_, rhs, _) => Type::try_from(Borrow::<Expr>::borrow(rhs)),
            Expr::Ident(ident, _) => Ok(Type::UserDefined(ident.name())),
            Expr::Fn { ret_t, .. } => Ok(ret_t.clone()),
            Expr::Call { func, .. } => Type::try_from(Borrow::<Expr>::borrow(func)),
            _ => Err(String::from("invaild literal")),
        }
    }
}

fn infer_infix_type(op: &Infix, lhs: &Expr, rhs: &Expr, _span: &Span) -> Type {
    if op == &Infix::Div {
        return Type::Float;
    }

    let lhs_t = infer_expr_type(lhs);
    let rhs_t = infer_expr_type(rhs);

    if lhs_t == Type::Int && rhs_t == Type::Int {
        return Type::Int;
    }

    Type::Float
}

fn infer_expr_type(expr: &Expr) -> Type {
    match expr {
        Expr::Infix(op, lhs, rhs, span) => {
            let t = infer_infix_type(op, lhs, rhs, span);
            if t == Type::Int {
                return Type::Int;
            }
        }
        Expr::Prefix(_, rhs, _) => match rhs.borrow() {
            Expr::Infix(i_op, i_lhs, i_rhs, i_span) => {
                let t = infer_infix_type(i_op, i_lhs, i_rhs, i_span);
                if t == Type::Int {
                    return Type::Int;
                }
            }
            Expr::Literal(Literal::Int(_), _) => return Type::Int,
            _ => {}
        },
        Expr::Literal(Literal::Int(_), _) => return Type::Int,
        _ => {}
    }

    Type::Float
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
