use crate::Token;

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Default)]
pub enum Precedence {
    #[default]
    Lowest,
    Eq,      // ==
    Cmp,     // > | < | >= | <=
    Sum,     // +
    Product, // *
    Prefix,  // -x or !x
    Call,    // f(x)
    Index,   // list[index]
}

impl From<&Token> for Precedence {
    fn from(token: &Token) -> Self {
        match token {
            Token::EQ | Token::NEQ => Precedence::Eq,
            Token::LT | Token::LTE | Token::GT | Token::GTE => Precedence::Cmp,
            Token::Add | Token::Sub => Precedence::Sum,
            Token::Mul | Token::Div | Token::IntDiv | Token::Rem => Precedence::Product,
            Token::Lbracket => Precedence::Index,
            Token::Lparen => Precedence::Call,
            _ => Precedence::Lowest,
        }
    }
}
