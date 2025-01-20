use std::fmt;

use crate::{ParserError, ParserErrorKind, Span};

use super::{Ident, Infix, Literal, Prefix, Stmt, Type, Var};

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Ident(Ident, Type, Span),
    Literal(Literal, Span),
    Prefix(Prefix, Box<Expr>, Span),
    Infix(Infix, Box<Expr>, Box<Expr>, Span),
    Index(Box<Expr>, Box<Expr>, Span),
    Range(Box<Expr>, Box<Expr>, Span),
    If {
        cond: Box<Expr>,
        then: Vec<Stmt>,
        elifs: Vec<IfExpr>,
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
    Fn {
        name: String,
        args: Vec<Var>,
        ret_t: Type,
        body: Vec<Stmt>,
        span: Span,
    },
    Closure {
        args: Vec<Var>,
        ret_t: Type,
        body: Vec<Stmt>,
        span: Span,
    },
    Call {
        name: String,
        args: Vec<Expr>,
        ret_t: Type,
        span: Span,
    },
    MethodCall {
        lhs: Box<Expr>,
        name: String,
        args: Vec<Expr>,
        ret_t: Type,
        span: Span,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub struct IfExpr {
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
            Expr::Fn { span, .. } => *span,
            Expr::Closure { span, .. } => *span,
            Expr::Call { span, .. } => *span,
            Expr::MethodCall { span, .. } => *span,
        }
    }

    pub fn is_indexable(&self) -> bool {
        match self {
            Expr::Ident(_, t, _) => t.is_indexable(),
            Expr::Literal(l, _) => l.is_indexable(),
            Expr::Prefix(_, rhs, _) => rhs.is_indexable(),
            Expr::Infix(_, lhs, rhs, _) => lhs.is_indexable() && rhs.is_indexable(),
            Expr::Index(lhs, _, _) => lhs.is_indexable(),
            Expr::Call { ret_t, .. } => ret_t.is_indexable(),
            _ => false,
        }
    }

    pub fn ret_t(&self, expected: &Type, last: bool) -> Result<Type, ParserError> {
        match self {
            Expr::If {
                then, elifs, alt, ..
            } => {
                let mut returning = None;
                for stmt in then {
                    let t = stmt.ret_t(expected, last)?;
                    if t != Type::Void && t != *expected {
                        return Err(ParserError::new(
                            ParserErrorKind::TypeMismatch(expected.clone(), t.clone()),
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
                                ParserErrorKind::TypeMismatch(expected.clone(), t.clone()),
                                self.span(),
                            ));
                        }
                    }
                }
                if let Some(alt) = alt {
                    for stmt in alt {
                        let t = stmt.ret_t(expected, last)?;
                        if let Some((rt, if_then)) = &returning {
                            if rt != expected {
                                return Err(ParserError::new(
                                    ParserErrorKind::TypeMismatch(expected.clone(), rt.clone()),
                                    if_then.span(),
                                ));
                            }
                        }
                        if t != *expected {
                            return Err(ParserError::new(
                                ParserErrorKind::TypeMismatch(expected.clone(), t.clone()),
                                stmt.span(),
                            ));
                        }
                    }
                } else if last && *expected != Type::Void {
                    return Err(ParserError::new(
                        ParserErrorKind::MissingElseClause,
                        self.span(),
                    ));
                }
                Ok(Type::Void)
            }
            Expr::While { body, .. } => {
                for stmt in body {
                    let t = stmt.ret_t(expected, last)?;
                    if t != Type::Void && t != *expected {
                        return Err(ParserError::new(
                            ParserErrorKind::TypeMismatch(expected.clone(), t.clone()),
                            self.span(),
                        ));
                    }
                }
                Ok(Type::Void)
            }
            _ => Ok(Type::Void),
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

    pub fn cast(&self, to: &Type) -> Expr {
        match self {
            Expr::Literal(l, _) => Expr::Literal(l.cast(to), self.span()),
            _ => self.clone(),
        }
    }

    pub fn fmt_inner(&self, f: &mut fmt::Formatter, indent_level: usize) -> fmt::Result {
        match self {
            Expr::Literal(literal, _) => literal.fmt_with_indent(f, indent_level, false),
            _ => self.fmt_with_indent(f, indent_level),
        }
    }

    pub fn fmt_inner_start(&self, f: &mut fmt::Formatter, indent_level: usize) -> fmt::Result {
        match self {
            Expr::Literal(literal, _) => literal.fmt_with_indent(f, indent_level, true),
            _ => self.fmt_with_indent(f, indent_level),
        }
    }

    pub fn fmt_with_indent(&self, f: &mut fmt::Formatter, indent_level: usize) -> fmt::Result {
        let indent = "    ".repeat(indent_level);

        match self {
            Expr::Ident(ident, _, _) => write!(f, "{ident}"),
            Expr::Literal(l, _) => {
                if l.is_composite() {
                    l.fmt_with_indent(f, indent_level, true)
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
                writeln!(f, "{}if {cond} {{", indent)?;
                for stmt in then {
                    stmt.fmt_with_indent(f, indent_level + 1)?;
                }
                for elif in elifs {
                    writeln!(f, "{}}} elif {} {{", indent, elif.cond)?;
                    for stmt in &elif.then {
                        stmt.fmt_with_indent(f, indent_level + 1)?;
                    }
                }
                if let Some(else_block) = alt {
                    writeln!(f, "{}}} else {{", indent)?;
                    for stmt in else_block {
                        stmt.fmt_with_indent(f, indent_level + 1)?;
                    }
                }
                writeln!(f, "{indent}}}")
            }
            Expr::While { cond, body, .. } => {
                writeln!(f, "{}while {cond} {{", indent)?;
                for stmt in body {
                    stmt.fmt_with_indent(f, indent_level + 1)?;
                }
                writeln!(f, "{indent}}}")
            }
            Expr::Iter {
                ident, expr, body, ..
            } => {
                writeln!(f, "{}for {ident} in {expr} {{", indent)?;
                for stmt in body {
                    stmt.fmt_with_indent(f, indent_level + 1)?;
                }
                writeln!(f, "{indent}}}")
            }
            Expr::Fn {
                name,
                args,
                ret_t,
                body,
                ..
            } => {
                write!(f, "{}fn {name}(", indent)?;
                let args = args
                    .iter()
                    .map(|arg| format!("{arg}"))
                    .collect::<Vec<String>>()
                    .join(", ");
                if *ret_t == Type::Void {
                    writeln!(f, "{args}) {{")?;
                } else {
                    writeln!(f, "{args}) -> {ret_t} {{")?;
                }
                for stmt in body {
                    stmt.fmt_with_indent(f, indent_level + 1)?;
                }
                writeln!(f, "{}}}", indent)
            }
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
                write!(f, "{}}}", indent)
            }
            Expr::Call { name, args, .. } => {
                write!(f, "{name}(")?;
                for (i, arg) in args.iter().enumerate() {
                    arg.fmt_inner_start(f, indent_level)?;
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
                    arg.fmt_inner_start(f, indent_level)?;
                    if i < args.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, ")")
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
        self.fmt_with_indent(f, 0)
    }
}
