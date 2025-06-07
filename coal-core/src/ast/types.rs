use std::{fmt, iter, vec};

use crate::TokenKind;

use super::{Expr, Func, Literal, Prefix, Stmt, Struct, StructDecl};

#[derive(Copy, Clone, Debug, Hash, PartialEq)]
pub enum Arity {
    Fixed(u8),
    Variable,
}

/// A base type. Can be modeled as a type-level function with 0 or more arguments.
#[derive(Clone, Debug, Hash, PartialEq, Default)]
pub enum BaseType {
    Bool,
    Str,
    Num(Num),
    List, // arity 1
    Map,  // arity 2
    Fn,   // arity n, first type argument is return type
    Range,
    Aliased(String, Box<ResolvedType>),
    // for now, we only support arity 0 structs
    Struct(String, Vec<(String, Box<ResolvedType>)>),
    Nil,
    Any,
    Void,
    #[default]
    Unknown,
}

/// A fully resolved type for a particular term, including all type arguments.
/// For example, in list[list[str]], the base type is list, and the only arg is list[str].
#[derive(Clone, Debug, Hash, PartialEq, Default)]
pub struct ResolvedType {
    pub base: BaseType,
    pub args: Vec<ResolvedType>,
}

pub type TypeIdentifier = String;

/// A tree of type identifiers. Allows us to represent nested
/// types like list[map[bool, MyCustomList[i32]]]
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TypeIdentifierTree {
    pub root: TypeIdentifier,
    pub children: Vec<TypeIdentifierTree>,
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

pub const U32: BaseType = BaseType::Num(Num::U32);
pub const U64: BaseType = BaseType::Num(Num::U64);
pub const I32: BaseType = BaseType::Num(Num::I32);
pub const I64: BaseType = BaseType::Num(Num::I64);
pub const I128: BaseType = BaseType::Num(Num::I128);
pub const F32: BaseType = BaseType::Num(Num::F32);
pub const F64: BaseType = BaseType::Num(Num::F64);

#[derive(Clone, Debug, Hash, PartialEq)]
pub struct MethodSignature {
    pub args_t: Vec<ResolvedType>,
    pub ret_t: ResolvedType,
    pub uses_self: bool,
}

impl MethodSignature {
    pub fn new(args_t: &[ResolvedType], ret_t: ResolvedType, uses_self: bool) -> Self {
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

impl BaseType {
    pub const fn arity(&self) -> Arity {
        match self {
            Self::Fn => Arity::Variable,
            Self::List => Arity::Fixed(1),
            Self::Map => Arity::Fixed(2),
            _ => Arity::Fixed(0),
        }
    }

    pub fn is_numeric(&self) -> bool {
        matches!(self, BaseType::Num(_))
    }

    pub fn is_int(&self) -> bool {
        matches!(self, &U32 | &U64 | &I32 | &I64 | &I128)
    }

    pub fn is_hashable(&self) -> bool {
        matches!(self, BaseType::Bool | BaseType::Str | BaseType::Num(_))
    }

    pub fn is_composite(&self) -> bool {
        self.arity() != Arity::Fixed(0)
    }

    pub fn is_indexable(&self) -> bool {
        match self.arity() {
            Arity::Fixed(0) | Arity::Variable => false,
            Arity::Fixed(_) => true,
        }
    }

    pub fn name(&self) -> String {
        match self {
            BaseType::Aliased(name, _) => name.to_owned(),
            BaseType::Struct(name, _) => name.to_owned(),
            _ => self.to_string(),
        }
    }

    pub fn partial_eq(&self, other: &BaseType) -> bool {
        if let (BaseType::Struct(a_name, a_attrs), BaseType::Struct(b_name, b_attrs)) =
            (self, other)
        {
            if a_name != b_name {
                return false;
            }
            for (_, attr_t) in a_attrs {
                if !matches!(attr_t.base, BaseType::Fn) && a_name != b_name && a_attrs != b_attrs {
                    return false;
                }
            }
            return true;
        }

        self == other
    }
}

impl TryFrom<BaseType> for ResolvedType {
    type Error = String;

    fn try_from(base: BaseType) -> Result<Self, Self::Error> {
        match base.arity() {
            Arity::Fixed(0) => Ok(ResolvedType { base, args: vec![] }),
            Arity::Fixed(_) => Err("Cannot auto-convert fixed arity type".to_string()),
            Arity::Variable => Err("Cannot auto-convert variable arity type".to_string()),
        }
    }
}

impl From<&Literal> for ResolvedType {
    fn from(literal: &Literal) -> Self {
        match literal {
            Literal::Str(_) => BaseType::Str.try_into().unwrap(),
            Literal::U32(_) => U32.try_into().unwrap(),
            Literal::U64(_) => U64.try_into().unwrap(),
            Literal::I32(_) => I32.try_into().unwrap(),
            Literal::I64(_) => I64.try_into().unwrap(),
            Literal::I128(_) => I128.try_into().unwrap(),
            Literal::F32(_) => F32.try_into().unwrap(),
            Literal::F64(_) => F64.try_into().unwrap(),
            Literal::Bool(_) => BaseType::Bool.try_into().unwrap(),
            Literal::List(l) => ResolvedType {
                base: BaseType::List,
                args: vec![l.t.to_owned()],
            },
            Literal::Map(m) => ResolvedType {
                base: BaseType::Map,
                args: vec![m.t.0.to_owned(), m.t.1.to_owned()],
            },
            Literal::Nil => BaseType::Nil.try_into().unwrap(),
        }
    }
}

impl From<&Vec<Stmt>> for ResolvedType {
    fn from(stmts: &Vec<Stmt>) -> Self {
        let mut ret_t: ResolvedType = BaseType::Void.try_into().unwrap();

        for stmt in stmts {
            match stmt {
                Stmt::Return(expr) => {
                    if let Ok(t) = ResolvedType::try_from(expr) {
                        ret_t = t;
                    }
                }
                Stmt::Expr(expr) if matches!(expr, Expr::If { .. }) => {
                    if let Ok(t) = ResolvedType::try_from(expr) {
                        ret_t = t;
                    }
                }
                _ => {}
            }
        }

        ret_t
    }
}

impl TryFrom<&TokenKind> for BaseType {
    type Error = BaseType;

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
                "str" => Ok(BaseType::Str),
                "bool" => Ok(BaseType::Bool),
                "range" => Ok(BaseType::Range),
                "nil" => Ok(BaseType::Nil),
                "any" => Ok(BaseType::Any),
                "void" => Ok(BaseType::Void),
                _ => Err(BaseType::Unknown),
            },
            TokenKind::U32(_) => Ok(U32),
            TokenKind::U64(_) => Ok(U64),
            TokenKind::I32(_) => Ok(I32),
            TokenKind::I64(_) => Ok(I64),
            TokenKind::I128(_) => Ok(I128),
            TokenKind::F32(_) => Ok(F32),
            TokenKind::F64(_) => Ok(F64),
            _ => Err(BaseType::Unknown),
        }
    }
}

impl TryFrom<&Expr> for ResolvedType {
    type Error = ResolvedType;

    fn try_from(expr: &Expr) -> Result<Self, Self::Error> {
        match expr {
            Expr::Ident(_, t, _) => Ok(t.to_owned()),
            Expr::Literal(l, _) => Ok(ResolvedType::from(l)),
            Expr::Prefix(prefix, rhs, _) => match prefix {
                Prefix::Not => Ok(BaseType::Bool.try_into().unwrap()),
                _ => ResolvedType::try_from(&**rhs),
            },
            Expr::Infix(_, lhs, rhs, _) => infer_infix_type(lhs, rhs),
            Expr::Index(expr, idx, _) => match ResolvedType::try_from(&**expr) {
                Ok(ResolvedType {
                    base: BaseType::List,
                    args,
                }) => match &**idx {
                    Expr::Index(i, _, _) => ResolvedType::try_from(&*i.clone()),
                    _ => Ok(args[0].clone()),
                },
                Ok(ResolvedType {
                    base: BaseType::Map,
                    args,
                }) => Ok(args[1].clone()),
                Ok(t) | Err(t) => Err(t),
            },
            Expr::Range(_, _, _) => Ok(U64.try_into().unwrap()),
            Expr::Fn(f @ Func { .. }) => Ok(f.into()),
            Expr::Closure { args, ret_t, .. } => Ok(ResolvedType {
                base: BaseType::Fn,
                args: iter::once(ret_t.clone())
                    .chain(args.iter().map(|arg| arg.t.to_owned()))
                    .collect(),
            }),
            Expr::Struct(s, _) => Ok(s.into()),
            Expr::Call { ret_t, .. } => Ok(ret_t.to_owned()),
            Expr::MethodCall { ret_t, .. } => Ok(ret_t.to_owned()),
            Expr::AttrAccess { t, .. } => Ok(t.to_owned()),
            _ => Err(ResolvedType::default()),
        }
    }
}

fn infer_infix_type(lhs: &Expr, rhs: &Expr) -> Result<ResolvedType, ResolvedType> {
    let lhs_t = ResolvedType::try_from(lhs)?;
    let rhs_t = ResolvedType::try_from(rhs)?;

    if let BaseType::Num(lhs_num) = &lhs_t.base
        && let BaseType::Num(rhs_num) = &rhs_t.base
    {
        if lhs_t.base == I32 && rhs_t.base == U64 || lhs_t.base == U64 && rhs_t.base == I32 {
            return Ok(I64.try_into().unwrap());
        }
        return Ok(BaseType::Num(lhs_num.max(rhs_num).clone())
            .try_into()
            .unwrap());
    }

    Err(ResolvedType::default())
}

impl From<&Func> for ResolvedType {
    fn from(f: &Func) -> Self {
        ResolvedType {
            base: BaseType::Fn,
            args: iter::once(f.ret_t.clone())
                .chain(f.args.iter().map(|arg| arg.t.to_owned()))
                .collect(),
        }
    }
}

impl From<&StructDecl> for BaseType {
    fn from(s: &StructDecl) -> Self {
        let attrs = s
            .attrs
            .iter()
            .map(|(p, _)| (p.name.clone(), Box::new(p.t.clone())))
            .collect();
        BaseType::Struct(s.name.to_owned(), attrs)
    }
}

impl From<&Struct> for ResolvedType {
    fn from(s: &Struct) -> Self {
        ResolvedType {
            base: BaseType::Struct(
                s.name.to_owned(),
                s.state
                    .iter()
                    .map(|(k, v)| {
                        let v_t = ResolvedType::try_from(v).unwrap_or_default();
                        (k.clone(), Box::new(v_t))
                    })
                    .collect(),
            ),
            // We only support arity 0 structs for now
            args: vec![],
        }
    }
}

impl fmt::Display for BaseType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            BaseType::Bool => write!(f, "bool"),
            BaseType::Str => write!(f, "str"),
            &U32 => write!(f, "u32"),
            &U64 => write!(f, "u64"),
            &I32 => write!(f, "i32"),
            &I64 => write!(f, "i64"),
            &I128 => write!(f, "i128"),
            &F32 => write!(f, "f32"),
            &F64 => write!(f, "f64"),
            BaseType::List => write!(f, "list"),
            BaseType::Map => write!(f, "map"),
            BaseType::Fn => write!(f, "Fn"),
            BaseType::Range => write!(f, "range"),
            BaseType::Aliased(name, _) => write!(f, "{name}"),
            BaseType::Struct(name, _) => write!(f, "{name}"),
            BaseType::Nil => write!(f, "nil"),
            BaseType::Any => write!(f, "any"),
            BaseType::Void => write!(f, "void"),
            BaseType::Unknown => write!(f, "unknown"),
        }
    }
}

impl fmt::Display for ResolvedType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.base {
            base if self.args.is_empty() => write!(f, "{base}"),
            BaseType::Fn => {
                let args_str = self
                    .args
                    .iter()
                    .skip(1)
                    .map(|arg| format!("{arg}"))
                    .collect::<Vec<String>>()
                    .join(", ");
                let ret_t = self.args[0].to_string();
                write!(f, "({args_str}) -> {ret_t}")
            }
            // format the rest like list[str] or map[str, i32]
            base => {
                let args_str = self
                    .args
                    .iter()
                    .map(|arg| format!("{arg}"))
                    .collect::<Vec<String>>()
                    .join(", ");
                write!(f, "{base}[{args_str}]")
            }
        }
    }
}

impl ResolvedType {
    pub fn is_defined(&self) -> bool {
        match self.base {
            BaseType::List => self.args[0].is_defined(),
            BaseType::Map => self.args[0].is_defined() && self.args[1].is_defined(),
            BaseType::Unknown => false,
            _ => true,
        }
    }

    pub fn extract(&self) -> &Self {
        match &self.base {
            BaseType::List => self.args[0].extract(),
            _ => self,
        }
    }

    pub fn sig(&self, method: &str) -> Option<MethodSignature> {
        match self.base {
            BaseType::Num(_) => self.num_sig(method),
            BaseType::Str => self.str_sig(method),
            BaseType::List => self.list_sig(method, &self.args[0]),
            BaseType::Map => self.map_sig(method, &(self.args[0].clone(), self.args[1].clone())),
            BaseType::Struct(_, ref attrs) => self.struct_sig(method, attrs),
            _ => self.global_sig(method),
        }
    }

    fn global_sig(&self, method: &str) -> Option<MethodSignature> {
        match method {
            "to_s" => Some(MethodSignature::new(
                &[],
                BaseType::Str.try_into().unwrap(),
                false,
            )),
            _ => None,
        }
    }

    fn num_sig(&self, method: &str) -> Option<MethodSignature> {
        self.global_sig(method)
    }

    fn str_sig(&self, method: &str) -> Option<MethodSignature> {
        match method {
            "len" => Some(MethodSignature::new(&[], U64.try_into().unwrap(), false)),
            "split" => Some(MethodSignature::new(
                &[BaseType::Str.try_into().unwrap()],
                ResolvedType {
                    base: BaseType::List,
                    args: vec![BaseType::Str.try_into().unwrap()],
                },
                false,
            )),
            _ => None,
        }
    }

    fn list_sig(&self, method: &str, t: &ResolvedType) -> Option<MethodSignature> {
        match method {
            "len" => Some(MethodSignature::new(&[], U64.try_into().unwrap(), false)),
            "push" => Some(MethodSignature::new(
                std::slice::from_ref(t),
                BaseType::Void.try_into().unwrap(),
                false,
            )),
            "pop" => Some(MethodSignature::new(&[], t.clone(), false)),
            "get" => Some(MethodSignature::new(
                &[I64.try_into().unwrap()],
                t.clone(),
                false,
            )),
            "first" => Some(MethodSignature::new(&[], t.clone(), false)),
            "last" => Some(MethodSignature::new(&[], t.clone(), false)),
            "join" => Some(MethodSignature::new(
                &[BaseType::Str.try_into().unwrap()],
                BaseType::Str.try_into().unwrap(),
                false,
            )),
            "map" => Some(MethodSignature::new(
                &[ResolvedType {
                    base: BaseType::Fn,
                    args: vec![t.clone(), t.clone()],
                }],
                ResolvedType {
                    base: BaseType::List,
                    args: vec![BaseType::Unknown.try_into().unwrap()],
                },
                false,
            )),
            "clear" => Some(MethodSignature::new(
                &[],
                BaseType::Void.try_into().unwrap(),
                false,
            )),
            _ => None,
        }
    }

    fn map_sig(
        &self,
        method: &str,
        (kt, vt): &(ResolvedType, ResolvedType),
    ) -> Option<MethodSignature> {
        match method {
            "len" => Some(MethodSignature::new(&[], U64.try_into().unwrap(), false)),
            "get" => Some(MethodSignature::new(
                std::slice::from_ref(kt),
                vt.clone(),
                false,
            )),
            "remove" => Some(MethodSignature::new(
                std::slice::from_ref(kt),
                BaseType::Void.try_into().unwrap(),
                false,
            )),
            "clear" => Some(MethodSignature::new(
                &[],
                BaseType::Void.try_into().unwrap(),
                false,
            )),
            _ => None,
        }
    }

    fn struct_sig(
        &self,
        method: &str,
        attrs: &Vec<(String, Box<ResolvedType>)>,
    ) -> Option<MethodSignature> {
        for (name, t) in attrs {
            if name == method && BaseType::Fn == t.base {
                return Some(MethodSignature::new(
                    &t.args[1..],
                    t.args[0].clone(),
                    // TODO: store uses_self in the Fn BaseType
                    false,
                ));
            }
        }
        None
    }
}

impl From<ResolvedType> for TypeIdentifierTree {
    fn from(resolved: ResolvedType) -> Self {
        TypeIdentifierTree {
            root: resolved.base.to_string(),
            children: resolved
                .args
                .into_iter()
                .map(TypeIdentifierTree::from)
                .collect(),
        }
    }
}
