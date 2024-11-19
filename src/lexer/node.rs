use crate::Token;

pub type Position = (usize, usize);

#[derive(Clone, Debug, Default)]
pub struct Node {
    pub token: Token,
    pub pos: Position,
}

impl Node {
    pub fn new(token: Token, pos: Position) -> Self {
        Node { token, pos }
    }
}
