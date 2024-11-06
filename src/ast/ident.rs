use std::fmt;

use crate::Token;

#[derive(Clone, Debug, PartialEq)]
pub struct Ident(pub String);

impl Ident {
    pub fn name(&self) -> String {
        self.0.clone()
    }
}

impl fmt::Display for Ident {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl TryFrom<&Token> for Ident {
    type Error = String;

    fn try_from(token: &Token) -> Result<Ident, Self::Error> {
        match token {
            Token::Ident(ident) => Ok(Ident(ident.clone())),
            _ => Err(String::from("invalid identifier")),
        }
    }
}
