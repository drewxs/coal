use crate::Token;

#[derive(Clone, Debug, PartialEq)]
pub enum Prefix {
    Plus,
    Minus,
    Not,
}

impl TryFrom<&Token> for Prefix {
    type Error = String;

    fn try_from(token: &Token) -> Result<Self, Self::Error> {
        match token {
            Token::Bang => Ok(Prefix::Not),
            Token::Plus => Ok(Prefix::Plus),
            Token::Minus => Ok(Prefix::Minus),
            _ => Err(format!("invalid prefix token: {token:?}")),
        }
    }
}
