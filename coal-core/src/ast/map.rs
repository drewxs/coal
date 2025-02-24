use std::fmt;

use crate::indent;

use super::{Expr, Type};

#[derive(Clone, Debug, PartialEq)]
pub struct Map {
    pub data: Vec<(Expr, Expr)>,
    pub t: (Type, Type),
}

impl Map {
    pub fn new(data: &[(Expr, Expr)], t: (Type, Type)) -> Self {
        Map {
            data: data.to_owned(),
            t,
        }
    }

    pub fn fmt_with_indent(
        &self,
        f: &mut fmt::Formatter,
        indent_level: usize,
        inner: bool,
    ) -> fmt::Result {
        let base_indent = indent(indent_level);
        let inner_indent = if inner { "" } else { &base_indent };

        match self.data.len() {
            0 => write!(f, "{}{{}}", inner_indent),
            1 => {
                let (k, v) = self.data.first().unwrap();
                write!(f, "{}{{{k}: {v}}}", inner_indent)
            }
            _ => {
                writeln!(f, "{}{{", inner_indent)?;
                for (k, v) in &self.data {
                    k.fmt_inner(f, indent_level + 1)?;
                    writeln!(f, ": {v},")?;
                }
                write!(f, "{}}}", base_indent)
            }
        }
    }
}

impl Default for Map {
    fn default() -> Self {
        Map {
            data: vec![],
            t: (Type::Unknown, Type::Unknown),
        }
    }
}

impl fmt::Display for Map {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.fmt_with_indent(f, 0, false)
    }
}
