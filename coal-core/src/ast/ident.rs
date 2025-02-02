use std::fmt;

use crate::{indent, TokenKind};

#[derive(Clone, Debug, PartialEq)]
pub struct Ident(pub String);

impl Ident {
    pub fn name(&self) -> String {
        self.0.clone()
    }

    pub fn fmt_with_indent(&self, f: &mut fmt::Formatter, indent_level: usize) -> fmt::Result {
        write!(f, "{}{}", indent(indent_level), self.0)
    }
}

impl From<&str> for Ident {
    fn from(name: &str) -> Self {
        Ident(name.to_string())
    }
}

impl TryFrom<&TokenKind> for Ident {
    type Error = String;

    fn try_from(token: &TokenKind) -> Result<Ident, Self::Error> {
        match token {
            TokenKind::Ident(ident) => Ok(Ident(ident.clone())),
            _ => Err(String::from("invalid identifier")),
        }
    }
}

impl fmt::Display for Ident {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.fmt_with_indent(f, 0)
    }
}
