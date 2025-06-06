use std::fmt;

use crate::TokenKind;

use super::{Expr, Func, Literal, Prefix, Stmt, Struct, StructDecl};

#[derive(Clone, Debug, Hash, PartialEq, Default)]
pub enum Type {
    Bool,
    Str,
    Num(Num),
    List(Box<Type>),
    Map(Box<(Type, Type)>),
    Fn {
        args_t: Vec<Type>,
        ret_t: Box<Type>,
        uses_self: bool,
    },
    Range,
    UserDefined(String, Box<Type>),
    StructDecl(String, Vec<(String, Type, bool)>, Vec<(String, Type)>),
    Struct(String, Vec<(String, Type)>),
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
    pub uses_self: bool,
}

impl MethodSignature {
    pub fn new(args_t: &[Type], ret_t: Type, uses_self: bool) -> Self {
        MethodSignature {
            args_t: args_t.to_owned(),
            ret_t,
            uses_self,
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

    pub fn is_int(&self) -> bool {
        matches!(self, &U32 | &U64 | &I32 | &I64 | &I128)
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
            Type::List(_)
            | Type::Map(_)
            | Type::Fn { .. }
            | Type::StructDecl(_, _, _)
            | Type::Struct(_, _) => true,
            _ => false,
        }
    }

    pub fn is_indexable(&self) -> bool {
        match self {
            Type::List(_) | Type::Map(_) => true,
            Type::UserDefined(_, t) => t.is_indexable(),
            _ => false,
        }
    }

    pub fn name(&self) -> String {
        match self {
            Type::UserDefined(name, _) => name.to_owned(),
            Type::StructDecl(name, _, _) => name.to_owned(),
            Type::Struct(name, _) => name.to_owned(),
            _ => self.to_string(),
        }
    }

    pub fn extract(&self) -> &Self {
        match self {
            Type::List(t) => t,
            t => t,
        }
    }

    pub fn into_struct_t(self) -> Option<Type> {
        if let Type::StructDecl(name, attrs, fns) = self {
            Some(Type::Struct(
                name.to_owned(),
                attrs
                    .iter()
                    .map(|(s, t, _)| (s.clone(), t.clone()))
                    .chain(fns)
                    .collect(),
            ))
        } else {
            None
        }
    }

    pub fn partial_eq(&self, other: &Type) -> bool {
        if let (Type::Struct(a_name, a_attrs), Type::Struct(b_name, b_attrs)) = (self, other) {
            if a_name != b_name {
                return false;
            }
            for (_, attr_t) in a_attrs {
                if !matches!(attr_t, Type::Fn { .. }) && a_name != b_name && a_attrs != b_attrs {
                    return false;
                }
            }
            return true;
        }

        self == other
    }

    pub fn sig(&self, method: &str) -> Option<MethodSignature> {
        match self {
            Type::Num(_) => self.num_sig(method),
            Type::Str => self.str_sig(method),
            Type::List(t) => self.list_sig(method, t),
            Type::Map(t) => self.map_sig(method, t),
            Type::Struct(_, attrs) => self.struct_sig(method, attrs),
            Type::StructDecl(_, _, funcs) => self.struct_sig(method, funcs),
            _ => self.global_sig(method),
        }
    }

    fn global_sig(&self, method: &str) -> Option<MethodSignature> {
        match method {
            "to_s" => Some(MethodSignature::new(&[], Type::Str, false)),
            _ => None,
        }
    }

    fn num_sig(&self, method: &str) -> Option<MethodSignature> {
        self.global_sig(method)
    }

    fn str_sig(&self, method: &str) -> Option<MethodSignature> {
        match method {
            "len" => Some(MethodSignature::new(&[], U64, false)),
            "split" => Some(MethodSignature::new(
                &[Type::Str],
                Type::List(Box::new(Type::Str)),
                false,
            )),
            _ => None,
        }
    }

    fn list_sig(&self, method: &str, t: &Type) -> Option<MethodSignature> {
        match method {
            "len" => Some(MethodSignature::new(&[], U64, false)),
            "push" => Some(MethodSignature::new(
                std::slice::from_ref(t),
                Type::Void,
                false,
            )),
            "pop" => Some(MethodSignature::new(&[], t.clone(), false)),
            "get" => Some(MethodSignature::new(&[I64], t.clone(), false)),
            "first" => Some(MethodSignature::new(&[], t.clone(), false)),
            "last" => Some(MethodSignature::new(&[], t.clone(), false)),
            "join" => Some(MethodSignature::new(&[Type::Str], Type::Str, false)),
            "map" => Some(MethodSignature::new(
                &[Type::Fn {
                    args_t: vec![t.clone()],
                    ret_t: Box::new(t.clone()),
                    uses_self: false,
                }],
                Type::List(Box::new(Type::Unknown)),
                false,
            )),
            "clear" => Some(MethodSignature::new(&[], Type::Void, false)),
            _ => None,
        }
    }

    fn map_sig(&self, method: &str, (kt, vt): &(Type, Type)) -> Option<MethodSignature> {
        match method {
            "len" => Some(MethodSignature::new(&[], U64, false)),
            "get" => Some(MethodSignature::new(
                std::slice::from_ref(kt),
                vt.clone(),
                false,
            )),
            "remove" => Some(MethodSignature::new(
                std::slice::from_ref(kt),
                Type::Void,
                false,
            )),
            "clear" => Some(MethodSignature::new(&[], Type::Void, false)),
            _ => None,
        }
    }

    fn struct_sig(&self, method: &str, attrs: &Vec<(String, Type)>) -> Option<MethodSignature> {
        for (name, t) in attrs {
            if name == method
                && let Type::Fn {
                    args_t,
                    ret_t,
                    uses_self,
                } = t
            {
                return Some(MethodSignature::new(args_t, *ret_t.clone(), *uses_self));
            }
        }
        None
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
            Literal::List(l) => Type::List(Box::new(l.t.to_owned())),
            Literal::Map(m) => Type::Map(Box::new(m.t.to_owned())),
            Literal::Nil => Type::Nil,
        }
    }
}

impl From<&Vec<Stmt>> for Type {
    fn from(stmts: &Vec<Stmt>) -> Self {
        let mut ret_t = Type::Void;

        for stmt in stmts {
            match stmt {
                Stmt::Return(expr) => {
                    if let Ok(t) = Type::try_from(expr) {
                        ret_t = t;
                    }
                }
                Stmt::Expr(expr) if matches!(expr, Expr::If { .. }) => {
                    if let Ok(t) = Type::try_from(expr) {
                        ret_t = t;
                    }
                }
                _ => {}
            }
        }

        ret_t
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
            Expr::Index(expr, idx, _) => match Type::try_from(&**expr) {
                Ok(Type::List(t)) => match &**idx {
                    Expr::Index(i, _, _) => Type::try_from(&*i.clone()),
                    _ => Ok(*t.clone()),
                },
                Ok(Type::Map(t)) => Ok((*t).1),
                Ok(t) | Err(t) => Err(t),
            },
            Expr::Range(_, _, _) => Ok(U64),
            Expr::Fn(f @ Func { .. }) => Ok(f.into()),
            Expr::Closure { args, ret_t, .. } => Ok(Type::Fn {
                args_t: args.iter().map(|arg| arg.t.to_owned()).collect(),
                ret_t: Box::new(ret_t.to_owned()),
                uses_self: false,
            }),
            Expr::Struct(s, _) => Ok(s.into()),
            Expr::Call { ret_t, .. } => Ok(ret_t.to_owned()),
            Expr::MethodCall { ret_t, .. } => Ok(ret_t.to_owned()),
            Expr::AttrAccess { t, .. } => Ok(t.to_owned()),
            _ => Err(Type::Unknown),
        }
    }
}

fn infer_infix_type(lhs: &Expr, rhs: &Expr) -> Result<Type, Type> {
    let lhs_t = Type::try_from(lhs)?;
    let rhs_t = Type::try_from(rhs)?;

    if let Type::Num(lhs_num) = &lhs_t
        && let Type::Num(rhs_num) = &rhs_t
    {
        if lhs_t == I32 && rhs_t == U64 || lhs_t == U64 && rhs_t == I32 {
            return Ok(I64);
        }
        return Ok(Type::Num(lhs_num.max(rhs_num).clone()));
    }

    Err(Type::Unknown)
}

impl From<&Func> for Type {
    fn from(f: &Func) -> Self {
        Type::Fn {
            args_t: f.args.iter().map(|arg| arg.t.to_owned()).collect(),
            ret_t: Box::new(f.ret_t.to_owned()),
            uses_self: f.uses_self(),
        }
    }
}

impl From<&StructDecl> for Type {
    fn from(s: &StructDecl) -> Self {
        let attrs = s
            .attrs
            .iter()
            .map(|(p, v)| (p.name.clone(), p.t.clone(), v.is_some()))
            .collect();
        let fns = s
            .funcs
            .iter()
            .map(|f| (f.name.clone(), Type::from(f)))
            .collect();
        Type::StructDecl(s.name.to_owned(), attrs, fns)
    }
}

impl From<&Struct> for Type {
    fn from(s: &Struct) -> Self {
        Type::Struct(
            s.name.to_owned(),
            s.state
                .iter()
                .map(|(k, v)| {
                    let v_t = Type::try_from(v).unwrap_or_default();
                    (k.clone(), v_t)
                })
                .collect(),
        )
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
            Type::Map(t) => write!(f, "map[{}, {}]", t.0, t.1),
            Type::Fn { args_t, ret_t, .. } => {
                let args_str = args_t
                    .iter()
                    .map(|arg| format!("{arg}"))
                    .collect::<Vec<String>>()
                    .join(", ");
                write!(f, "Fn({args_str}) -> {ret_t}")
            }
            Type::Range => write!(f, "range"),
            Type::UserDefined(name, _) => write!(f, "{name}"),
            Type::StructDecl(name, _, _) => write!(f, "{name}"),
            Type::Struct(name, _) => write!(f, "{name}"),
            Type::Nil => write!(f, "nil"),
            Type::Any => write!(f, "any"),
            Type::Void => write!(f, "void"),
            Type::Unknown => write!(f, "unknown"),
        }
    }
}
