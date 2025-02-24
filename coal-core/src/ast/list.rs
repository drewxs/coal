use std::fmt;

use crate::indent;

use super::{Expr, Type};

#[derive(Clone, Debug, PartialEq)]
pub struct List {
    pub data: Vec<Expr>,
    pub t: Type,
    pub repeat: Option<Box<Expr>>,
}

impl List {
    pub fn new(data: &[Expr], t: Type) -> Self {
        List {
            data: data.to_owned(),
            t,
            repeat: None,
        }
    }

    pub fn new_repeat(data: &[Expr], t: Type, repeat: Box<Expr>) -> Self {
        List {
            data: data.to_owned(),
            t,
            repeat: Some(repeat),
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

        if let (Some(n), Some(e)) = (&self.repeat, self.data.first()) {
            write!(f, "{}[{e}; {n}]", inner_indent)
        } else {
            match self.data.len() {
                0 => write!(f, "{}[]", inner_indent),
                1 | 2 => {
                    let v = self
                        .data
                        .iter()
                        .map(|x| x.to_string())
                        .collect::<Vec<_>>()
                        .join(", ");
                    write!(f, "{}[{v}]", inner_indent)
                }
                _ => {
                    writeln!(f, "{}[", inner_indent)?;
                    for item in &self.data {
                        item.fmt_inner(f, indent_level + 1)?;
                        writeln!(f, ",")?;
                    }
                    write!(f, "{}]", base_indent)
                }
            }
        }
    }
}

impl Default for List {
    fn default() -> Self {
        List {
            data: vec![],
            t: Type::Unknown,
            repeat: None,
        }
    }
}

impl fmt::Display for List {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.fmt_with_indent(f, 0, false)
    }
}
