use std::{
    fmt,
    ops::{Deref, DerefMut, Index, IndexMut},
};

use bincode::{Decode, Encode};

use super::{Opcode, make};

#[derive(Clone, Debug, PartialEq, Default, Encode, Decode)]
pub struct Instructions(pub Vec<u8>);

impl Instructions {
    pub fn new(opcode: Opcode, operands: &[usize]) -> Self {
        Self(make(opcode, operands))
    }
}

impl From<&[Instructions]> for Instructions {
    fn from(value: &[Instructions]) -> Self {
        Instructions(
            value
                .iter()
                .map(|i| i.0.clone())
                .collect::<Vec<Vec<u8>>>()
                .concat(),
        )
    }
}

impl Deref for Instructions {
    type Target = Vec<u8>;

    fn deref(&self) -> &Vec<u8> {
        &self.0
    }
}

impl DerefMut for Instructions {
    fn deref_mut(&mut self) -> &mut Vec<u8> {
        &mut self.0
    }
}

impl Index<usize> for Instructions {
    type Output = u8;

    fn index(&self, index: usize) -> &Self::Output {
        &self.0[index]
    }
}

impl IndexMut<usize> for Instructions {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.0[index]
    }
}

impl fmt::Display for Instructions {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            self.0
                .iter()
                .map(|b| format!("{b:02X}"))
                .collect::<Vec<String>>()
                .join(" ")
        )
    }
}

impl From<Vec<Instructions>> for Instructions {
    fn from(value: Vec<Instructions>) -> Self {
        Instructions(
            value
                .iter()
                .map(|i| i.0.clone())
                .collect::<Vec<Vec<u8>>>()
                .concat(),
        )
    }
}
