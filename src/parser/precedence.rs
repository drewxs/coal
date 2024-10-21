use crate::Token;

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum Precedence {
    Lowest,
    Eq,      // ==
    Cmp,     // > | < | >= | <=
    Sum,     // +
    Product, // *
    Prefix,  // -x or !x
    Call,    // f(x)
    Index,   // vec[index]
}

impl From<&Token> for Precedence {
    fn from(token: &Token) -> Self {
        match token {
            Token::EQ | Token::NEQ => Precedence::Eq,
            Token::LT | Token::LTE | Token::GT | Token::GTE => Precedence::Cmp,
            Token::Plus | Token::Minus => Precedence::Sum,
            Token::Asterisk | Token::Slash | Token::Modulo => Precedence::Product,
            Token::Lbracket => Precedence::Index,
            Token::Lparen => Precedence::Call,
            _ => Precedence::Lowest,
        }
    }
}
