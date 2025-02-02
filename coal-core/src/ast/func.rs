use std::fmt;

use crate::{indent, Span};

use super::{Param, Stmt, Type};

#[derive(Clone, Debug, PartialEq)]
pub struct Func {
    pub name: String,
    pub args: Vec<Param>,
    pub ret_t: Type,
    pub body: Vec<Stmt>,
    pub span: Span,
}

impl Func {
    pub fn fmt_with_indent(&self, f: &mut fmt::Formatter, indent_level: usize) -> fmt::Result {
        let base_indent = indent(indent_level);
        let Func {
            name,
            args,
            ret_t,
            body,
            ..
        } = self;

        write!(f, "{}fn {name}(", base_indent)?;
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
        writeln!(f, "{}}}", base_indent)
    }
}

impl fmt::Display for Func {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.fmt_with_indent(f, 0)
    }
}
