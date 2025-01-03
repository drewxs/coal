use std::fmt;

use crate::Type;

#[derive(Clone, Debug, PartialEq)]
pub struct Var {
    pub name: String,
    pub t: Type,
}

impl Var {
    pub fn new(name: &str, t: Type) -> Var {
        Var {
            name: name.to_string(),
            t,
        }
    }
}

impl fmt::Display for Var {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}: {}", self.name, self.t)
    }
}
