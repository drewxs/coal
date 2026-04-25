use std::{
    fmt,
    ops::{Deref, DerefMut, Index, IndexMut},
};

use rkyv::{Archive, Deserialize, Serialize};

use super::{Opcode, make};

#[derive(Clone, PartialEq, Default, Archive, Serialize, Deserialize)]
pub struct Instructions(pub Vec<u8>);

impl Instructions {
    pub fn new(opcode: Opcode, operands: &[usize]) -> Self {
        Self(make(opcode, operands))
    }
}

impl fmt::Debug for Instructions {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let opcodes = self
            .0
            .iter()
            .map(|&b| {
                if b <= 36 {
                    format!("{:?}", Opcode::from(b))
                } else {
                    format!("{b:#X}")
                }
            })
            .collect::<Vec<String>>();
        write!(f, "{opcodes:?}")
    }
}

impl From<Vec<Instructions>> for Instructions {
    fn from(ins: Vec<Instructions>) -> Self {
        Instructions(
            ins.into_iter()
                .map(|i| i.0)
                .collect::<Vec<Vec<u8>>>()
                .concat(),
        )
    }
}

impl From<Vec<(Opcode, Vec<usize>)>> for Instructions {
    fn from(ins: Vec<(Opcode, Vec<usize>)>) -> Self {
        Instructions(
            ins.iter()
                .map(|(opcode, operands)| Instructions::new(*opcode, operands).0)
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
    fn deref_mut(&mut self) -> &mut Self::Target {
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
