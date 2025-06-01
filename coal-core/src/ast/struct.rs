use std::{
    fmt,
    hash::{Hash, Hasher},
};

use crate::indent;

use super::{Expr, Func, Param};

#[derive(Clone, Debug, PartialEq)]
pub struct Struct {
    pub name: String,
    pub state: Vec<(String, Expr)>,
}

impl Struct {
    pub fn new(name: &str, state: &[(String, Expr)]) -> Self {
        Struct {
            name: name.to_owned(),
            state: state.to_owned(),
        }
    }

    pub fn get(&self, key: &str) -> Option<&Expr> {
        self.state
            .iter()
            .find(|(attr, _)| attr == key)
            .map(|(_, v)| v)
    }

    pub fn fmt_with_indent(&self, f: &mut fmt::Formatter, indent_level: usize) -> fmt::Result {
        let base_indent = indent(indent_level);
        let inner_indent = indent(indent_level + 1);
        let Struct { name, state } = self;

        match state.len() {
            0 => {
                write!(f, "{name} {{}}")
            }
            1 if !state[0].1.is_fn() => {
                let (k, v) = &state[0];
                write!(f, "{name} {{ {k}: {v} }}")
            }
            _ => {
                writeln!(f, "{name} {{")?;
                for (k, v) in state {
                    write!(f, "{inner_indent}{k}: ")?;
                    v.fmt_with_indent(f, indent_level + 1, true)?;
                    writeln!(f, ",")?;
                }
                write!(f, "{base_indent}}}")
            }
        }
    }
}

impl fmt::Display for Struct {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.fmt_with_indent(f, 0)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct StructDecl {
    pub name: String,
    pub attrs: Vec<(Param, Option<Expr>)>,
    pub funcs: Vec<Func>,
}

impl StructDecl {
    pub fn new(name: &str, attrs: &[(Param, Option<Expr>)], funcs: &[Func]) -> Self {
        StructDecl {
            name: name.to_owned(),
            attrs: attrs.to_owned(),
            funcs: funcs.to_owned(),
        }
    }

    pub fn fmt_with_indent(&self, f: &mut fmt::Formatter, indent_level: usize) -> fmt::Result {
        let base_indent = indent(indent_level);

        let StructDecl { name, attrs, funcs } = self;

        if attrs.is_empty() {
            return writeln!(f, "{base_indent}struct {name} {{}}");
        }

        writeln!(f, "{base_indent}struct {name} {{")?;
        for (attr, default) in attrs {
            attr.fmt_with_indent(f, indent_level + 1)?;
            if let Some(val) = default {
                write!(f, " = ")?;
                val.fmt_with_indent(f, 0, true)?;
            }
            writeln!(f, ",")?;
        }
        if !attrs.is_empty() && !funcs.is_empty() {
            writeln!(f)?;
        }
        for (i, func) in funcs.iter().enumerate() {
            func.fmt_with_indent(f, indent_level + 1)?;
            if i < funcs.len() - 1 {
                writeln!(f)?;
            }
        }
        writeln!(f, "{base_indent}}}")
    }
}

impl Hash for StructDecl {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.name.hash(state);
        self.attrs.len().hash(state);
        for (p, _) in &self.attrs {
            p.hash(state);
        }
        self.funcs.len().hash(state);
        for f in &self.funcs {
            f.hash(state);
        }
    }
}

impl fmt::Display for StructDecl {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.fmt_with_indent(f, 0)
    }
}
