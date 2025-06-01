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

        if self.data.is_empty() {
            write!(f, "{inner_indent}{{}}")
        } else {
            let data_str = self
                .data
                .iter()
                .map(|(k, v)| format!("{k}: {v}"))
                .collect::<Vec<_>>()
                .join(", ");
            let width = data_str.len() + inner_indent.len();

            if width > 40 {
                writeln!(f, "{inner_indent}{{")?;
                let nested_indent = indent(indent_level + 1);
                for (k, v) in &self.data {
                    write!(f, "{nested_indent}{k}: ")?;
                    v.fmt_with_indent(f, indent_level + 1, true)?;
                    writeln!(f, ",")?;
                }
                write!(f, "{base_indent}}}")
            } else {
                write!(f, "{inner_indent}{{")?;
                for (i, (k, v)) in self.data.iter().enumerate() {
                    write!(f, "{k}: ")?;
                    v.fmt_with_indent(f, indent_level + 1, true)?;
                    if i < self.data.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, "{base_indent}}}")
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
