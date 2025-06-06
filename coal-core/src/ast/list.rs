use std::{
    fmt,
    ops::{Deref, DerefMut, Index, IndexMut},
};

use crate::indent;

use super::{Expr, Type};

#[derive(Clone, Debug, PartialEq, Default)]
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
            write!(f, "{inner_indent}[{e}; {n}]")
        } else if self.data.is_empty() {
            write!(f, "{inner_indent}[]")
        } else {
            let data_str = self
                .data
                .iter()
                .map(|e| e.to_string())
                .collect::<Vec<_>>()
                .join(", ");
            let width = data_str.len() + inner_indent.len();

            if width > 40 {
                writeln!(f, "{inner_indent}[")?;
                let nested_indent = indent(indent_level + 1);
                for item in &self.data {
                    write!(f, "{nested_indent}")?;
                    item.fmt_with_indent(f, indent_level + 1, true)?;
                    writeln!(f, ",")?;
                }
                write!(f, "{base_indent}]")
            } else {
                write!(f, "{inner_indent}[")?;
                for (i, item) in self.data.iter().enumerate() {
                    item.fmt_with_indent(f, indent_level + 1, true)?;
                    if i < self.data.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, "{inner_indent}]")
            }
        }
    }
}

impl Deref for List {
    type Target = Vec<Expr>;

    fn deref(&self) -> &Vec<Expr> {
        &self.data
    }
}

impl DerefMut for List {
    fn deref_mut(&mut self) -> &mut Vec<Expr> {
        &mut self.data
    }
}

impl Index<usize> for List {
    type Output = Expr;

    fn index(&self, index: usize) -> &Self::Output {
        &self.data[index]
    }
}

impl IndexMut<usize> for List {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.data[index]
    }
}

impl fmt::Display for List {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.fmt_with_indent(f, 0, false)
    }
}
