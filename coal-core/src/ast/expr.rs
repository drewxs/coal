use std::fmt;

use crate::{ParserError, ParserErrorKind, Span, indent};

use super::{BaseType, Func, Ident, Infix, Literal, Param, Prefix, ResolvedType, Stmt, Struct};

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Ident(Ident, ResolvedType, Span),
    Literal(Literal, Span),
    Prefix(Prefix, Box<Expr>, Span),
    Infix(Infix, Box<Expr>, Box<Expr>, Span),
    Index(Box<Expr>, Box<Expr>, Span),
    Range(Box<Expr>, Box<Expr>, Span),
    If {
        cond: Box<Expr>,
        then: Vec<Stmt>,
        elifs: Vec<ElifExpr>,
        alt: Option<Vec<Stmt>>,
        span: Span,
    },
    While {
        cond: Box<Expr>,
        body: Vec<Stmt>,
        span: Span,
    },
    Iter {
        ident: Ident,
        expr: Box<Expr>,
        body: Vec<Stmt>,
        span: Span,
    },
    Fn(Func),
    Closure {
        args: Vec<Param>,
        ret_t: ResolvedType,
        body: Vec<Stmt>,
        span: Span,
    },
    Struct(Struct, Span),
    Call {
        name: String,
        args: Vec<Expr>,
        ret_t: ResolvedType,
        span: Span,
    },
    MethodCall {
        lhs: Box<Expr>,
        name: String,
        args: Vec<Expr>,
        ret_t: ResolvedType,
        span: Span,
    },
    AttrAccess {
        lhs: Box<Expr>,
        name: String,
        t: ResolvedType,
        span: Span,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub struct ElifExpr {
    pub cond: Box<Expr>,
    pub then: Vec<Stmt>,
}

impl Expr {
    pub fn span(&self) -> Span {
        match self {
            Expr::Ident(_, _, span) => *span,
            Expr::Literal(_, span) => *span,
            Expr::Prefix(_, _, span) => *span,
            Expr::Infix(_, _, _, span) => *span,
            Expr::Index(_, _, span) => *span,
            Expr::Range(_, _, span) => *span,
            Expr::If { span, .. } => *span,
            Expr::While { span, .. } => *span,
            Expr::Iter { span, .. } => *span,
            Expr::Fn(Func { span, .. }) => *span,
            Expr::Closure { span, .. } => *span,
            Expr::Struct(_, span) => *span,
            Expr::Call { span, .. } => *span,
            Expr::MethodCall { span, .. } => *span,
            Expr::AttrAccess { span, .. } => *span,
        }
    }

    pub fn is_indexable(&self) -> bool {
        match self {
            Expr::Ident(_, t, _) => t.base.is_indexable(),
            Expr::Literal(l, _) => l.is_indexable(),
            Expr::Prefix(_, rhs, _) => rhs.is_indexable(),
            Expr::Infix(_, lhs, rhs, _) => lhs.is_indexable() && rhs.is_indexable(),
            Expr::Index(lhs, _, _) => lhs.is_indexable(),
            Expr::Call { ret_t, .. } => ret_t.base.is_indexable(),
            _ => false,
        }
    }

    pub fn is_fn(&self) -> bool {
        matches!(self, Expr::Fn(_) | Expr::Closure { .. })
    }

    pub fn ret_t(&self, expected: &ResolvedType, last: bool) -> Result<ResolvedType, ParserError> {
        match self {
            Expr::If {
                then, elifs, alt, ..
            } => {
                let mut returning = None;
                for stmt in then {
                    let t = stmt.ret_t(expected, last)?;
                    if t.base != BaseType::Void && t != *expected {
                        return Err(ParserError::new(
                            ParserErrorKind::TypeMismatch(
                                expected.clone().into(),
                                t.clone().into(),
                            ),
                            self.span(),
                        ));
                    }
                    returning = Some((t, stmt.clone()));
                }
                for elif in elifs {
                    for stmt in &elif.then {
                        let t = stmt.ret_t(expected, last)?;
                        if t != *expected {
                            return Err(ParserError::new(
                                ParserErrorKind::TypeMismatch(
                                    expected.clone().into(),
                                    t.clone().into(),
                                ),
                                self.span(),
                            ));
                        }
                    }
                }
                if let Some(alt) = alt {
                    for stmt in alt {
                        let t = stmt.ret_t(expected, last)?;
                        if let Some((rt, if_then)) = &returning
                            && rt != expected
                        {
                            return Err(ParserError::new(
                                ParserErrorKind::TypeMismatch(
                                    expected.clone().into(),
                                    rt.clone().into(),
                                ),
                                if_then.span(),
                            ));
                        }
                        if t != *expected {
                            return Err(ParserError::new(
                                ParserErrorKind::TypeMismatch(
                                    expected.clone().into(),
                                    t.clone().into(),
                                ),
                                stmt.span(),
                            ));
                        }
                    }
                } else if last && expected.base != BaseType::Void {
                    return Err(ParserError::new(
                        ParserErrorKind::MissingElseClause,
                        self.span(),
                    ));
                }
                Ok(BaseType::Void.try_into().unwrap())
            }
            Expr::While { body, .. } => {
                for stmt in body {
                    let t = stmt.ret_t(expected, last)?;
                    if t.base != BaseType::Void && t != *expected {
                        return Err(ParserError::new(
                            ParserErrorKind::TypeMismatch(
                                expected.clone().into(),
                                t.clone().into(),
                            ),
                            self.span(),
                        ));
                    }
                }
                Ok(BaseType::Void.try_into().unwrap())
            }
            _ => Ok(BaseType::Void.try_into().unwrap()),
        }
    }

    pub fn ret_stmts(&self) -> Vec<Stmt> {
        let mut rets = vec![];
        match self {
            Expr::If {
                then, elifs, alt, ..
            } => {
                for stmt in then {
                    rets.extend(stmt.ret_stmts());
                }
                for elif in elifs {
                    for stmt in &elif.then {
                        rets.extend(stmt.ret_stmts());
                    }
                }
                if let Some(alt) = alt {
                    for stmt in alt {
                        rets.extend(stmt.ret_stmts());
                    }
                }
            }
            Expr::While { body, .. } => {
                for stmt in body {
                    rets.extend(stmt.ret_stmts());
                }
            }
            Expr::Iter { expr, .. } => {
                rets.extend(expr.ret_stmts());
            }
            _ => {}
        }
        rets
    }

    pub fn cast(&self, to: &ResolvedType) -> Expr {
        match self {
            Expr::Literal(l, _) => Expr::Literal(l.cast(to), self.span()),
            _ => self.clone(),
        }
    }

    pub fn fmt_with_indent(
        &self,
        f: &mut fmt::Formatter,
        indent_level: usize,
        inner: bool,
    ) -> fmt::Result {
        let base_indent = indent(indent_level);

        match self {
            Expr::Ident(ident, _, _) => write!(f, "{ident}"),
            Expr::Literal(l, _) => {
                if l.is_composite() {
                    l.fmt_with_indent(f, indent_level, inner)
                } else {
                    write!(f, "{l}")
                }
            }
            Expr::Prefix(prefix, expr, _) => write!(f, "{prefix}{expr}"),
            Expr::Infix(infix, lhs, rhs, _) => write!(f, "{lhs} {infix} {rhs}"),
            Expr::Index(lhs, rhs, _) => write!(f, "{lhs}[{rhs}]"),
            Expr::Range(start, end, _) => write!(f, "{start}..{end}"),
            Expr::If {
                cond,
                then,
                elifs,
                alt,
                ..
            } => {
                writeln!(f, "{base_indent}if {cond} {{")?;
                for stmt in then {
                    stmt.fmt_with_indent(f, indent_level + 1)?;
                }
                for ElifExpr { cond, then } in elifs {
                    writeln!(f, "{base_indent}}} elif {cond} {{")?;
                    for stmt in then {
                        stmt.fmt_with_indent(f, indent_level + 1)?;
                    }
                }
                if let Some(else_block) = alt {
                    writeln!(f, "{base_indent}}} else {{")?;
                    for stmt in else_block {
                        stmt.fmt_with_indent(f, indent_level + 1)?;
                    }
                }
                writeln!(f, "{base_indent}}}")
            }
            Expr::While { cond, body, .. } => {
                writeln!(f, "{base_indent}while {cond} {{")?;
                for stmt in body {
                    stmt.fmt_with_indent(f, indent_level + 1)?;
                }
                writeln!(f, "{base_indent}}}")
            }
            Expr::Iter {
                ident, expr, body, ..
            } => {
                writeln!(f, "{base_indent}for {ident} in {expr} {{")?;
                for stmt in body {
                    stmt.fmt_with_indent(f, indent_level + 1)?;
                }
                writeln!(f, "{base_indent}}}")
            }
            Expr::Fn(func) => func.fmt_with_indent(f, indent_level),
            Expr::Closure { args, body, .. } => {
                let args = args
                    .iter()
                    .map(|arg| format!("{arg}"))
                    .collect::<Vec<String>>()
                    .join(", ");
                writeln!(f, "|{args}| {{")?;
                for stmt in body {
                    stmt.fmt_with_indent(f, indent_level + 1)?;
                }
                write!(f, "{base_indent}}}")
            }
            Expr::Struct(s, _) => s.fmt_with_indent(f, indent_level),
            Expr::Call { name, args, .. } => {
                write!(f, "{name}(")?;
                for (i, arg) in args.iter().enumerate() {
                    arg.fmt_with_indent(f, indent_level, true)?;
                    if i < args.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, ")")
            }
            Expr::MethodCall {
                lhs, name, args, ..
            } => {
                write!(f, "{lhs}.{name}(")?;
                for (i, arg) in args.iter().enumerate() {
                    arg.fmt_with_indent(f, indent_level, true)?;
                    if i < args.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, ")")
            }
            Expr::AttrAccess { lhs, name, .. } => {
                write!(f, "{lhs}.{name}")
            }
        }
    }
}

impl TryInto<usize> for Expr {
    type Error = ();

    fn try_into(self) -> Result<usize, Self::Error> {
        match self {
            Expr::Literal(l, _) => Ok(l.try_into()?),
            _ => Err(()),
        }
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.fmt_with_indent(f, 0, false)
    }
}
