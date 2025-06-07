use std::{
    fmt,
    hash::{Hash, Hasher},
};

use crate::{ResolvedType, indent};

#[derive(Clone, Debug, PartialEq)]
pub struct Param {
    pub name: String,
    pub t: ResolvedType,
}

impl Param {
    pub fn new(name: &str, t: ResolvedType) -> Param {
        Param {
            name: name.to_string(),
            t,
        }
    }
}

impl Hash for Param {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.name.hash(state);
        self.t.hash(state);
    }
}

impl Param {
    pub fn fmt_with_indent(&self, f: &mut fmt::Formatter, indent_level: usize) -> fmt::Result {
        if self.name == "self" {
            write!(f, "{}self", indent(indent_level))
        } else {
            write!(f, "{}{}: {}", indent(indent_level), self.name, self.t)
        }
    }
}

impl fmt::Display for Param {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.fmt_with_indent(f, 0)
    }
}
