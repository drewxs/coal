use std::{
    fmt,
    hash::{Hash, Hasher},
};

use crate::Type;

#[derive(Clone, Debug, PartialEq)]
pub struct Param {
    pub name: String,
    pub t: Type,
}

impl Param {
    pub fn new(name: &str, t: Type) -> Param {
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

impl fmt::Display for Param {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}: {}", self.name, self.t)
    }
}
