use crate::Token;

#[derive(Clone, Debug, PartialEq)]
pub enum Infix {
    Plus,
    Minus,
    Mul,
    Div,
    Mod,
    EQ,
    NEQ,
    LT,
    LTE,
    GT,
    GTE,
}

impl TryFrom<&Token> for Infix {
    type Error = String;

    fn try_from(token: &Token) -> Result<Self, Self::Error> {
        match token {
            Token::Plus => Ok(Infix::Plus),
            Token::Minus => Ok(Infix::Minus),
            Token::Asterisk => Ok(Infix::Mul),
            Token::Slash => Ok(Infix::Div),
            Token::Modulo => Ok(Infix::Mod),
            Token::EQ => Ok(Infix::EQ),
            Token::NEQ => Ok(Infix::NEQ),
            Token::LT => Ok(Infix::LT),
            Token::LTE => Ok(Infix::LTE),
            Token::GT => Ok(Infix::GT),
            Token::GTE => Ok(Infix::GTE),
            _ => Err(format!("invalid infix token: {token:?}")),
        }
    }
}
