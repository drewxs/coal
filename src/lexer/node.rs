use crate::Token;

pub type Position = (usize, usize);

#[derive(Clone, Debug)]
pub struct Node {
    pub token: Token,
    pub pos: Position,
}
