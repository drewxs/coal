use crate::TokenKind;

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

impl From<&TokenKind> for Precedence {
    fn from(token: &TokenKind) -> Self {
        match token {
            TokenKind::EQ | TokenKind::NEQ => Precedence::Eq,
            TokenKind::LT | TokenKind::LTE | TokenKind::GT | TokenKind::GTE | TokenKind::Range => {
                Precedence::Cmp
            }
            TokenKind::Add | TokenKind::Sub => Precedence::Sum,
            TokenKind::Mul | TokenKind::Div | TokenKind::Rem => Precedence::Product,
            TokenKind::Lbracket => Precedence::Index,
            TokenKind::Lparen | TokenKind::Dot => Precedence::Call,
            _ => Precedence::Lowest,
        }
    }
}
