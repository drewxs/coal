use std::fmt;

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

        if state.is_empty() {
            return write!(f, "{name} {{}}");
        }

        writeln!(f, "{name} {{")?;
        for (k, v) in state {
            write!(f, "{}{k}: ", inner_indent)?;
            v.fmt_with_indent(f, indent_level + 1)?;
            writeln!(f, ",")?;
        }
        write!(f, "{}}}", base_indent)
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
            return writeln!(f, "{}struct {name} {{}}", base_indent);
        }

        writeln!(f, "{}struct {name} {{", base_indent)?;
        for (attr, default) in attrs {
            attr.fmt_with_indent(f, indent_level + 1)?;
            if let Some(val) = default {
                write!(f, " = ")?;
                val.fmt_with_indent(f, 0)?;
            }
            writeln!(f, ";")?;
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
        writeln!(f, "{}}}", base_indent)
    }
}

impl fmt::Display for StructDecl {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.fmt_with_indent(f, 0)
    }
}
