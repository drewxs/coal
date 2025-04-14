#[cfg(test)]
mod tests;

use std::{
    fmt,
    ops::{Deref, DerefMut, Index, IndexMut},
    rc::Rc,
};

use coal_core::{Infix, Prefix};
use coal_objects::Object;

#[repr(u8)]
#[derive(Copy, Clone, Debug, PartialEq, Default)]
pub enum Opcode {
    #[default]
    Nil = 0,

    Const,
    Pop,

    True,
    False,

    Add,
    Sub,
    Mul,
    Div,
    Rem,

    EQ,
    NEQ,
    LT,
    LTE,
    GT,
    GTE,

    Minus,
    Bang,

    Jump,
    JumpIfNot,

    GetGlobal,
    SetGlobal,

    List,
    Hash,
    Index,

    Call,

    RetVal,
    Ret,

    GetLocal,
    SetLocal,

    GetBuiltin,

    Closure,
    CurrClosure,

    GetFree,
}

impl Opcode {
    pub fn operand_widths(&self) -> Vec<u8> {
        match self {
            Opcode::Nil => vec![],
            Opcode::Const => vec![2],
            Opcode::Pop => vec![],
            Opcode::True => vec![],
            Opcode::False => vec![],
            Opcode::Add => vec![],
            Opcode::Sub => vec![],
            Opcode::Mul => vec![],
            Opcode::Div => vec![],
            Opcode::Rem => vec![],
            Opcode::EQ => vec![],
            Opcode::NEQ => vec![],
            Opcode::LT => vec![],
            Opcode::LTE => vec![],
            Opcode::GT => vec![],
            Opcode::GTE => vec![],
            Opcode::Minus => vec![],
            Opcode::Bang => vec![],
            Opcode::Jump => vec![2],
            Opcode::JumpIfNot => vec![2],
            Opcode::GetGlobal => vec![2],
            Opcode::SetGlobal => vec![2],
            Opcode::List => vec![2],
            Opcode::Hash => vec![2],
            Opcode::Index => vec![],
            Opcode::Call => vec![1],
            Opcode::RetVal => vec![],
            Opcode::Ret => vec![],
            Opcode::GetLocal => vec![1],
            Opcode::SetLocal => vec![1],
            Opcode::GetBuiltin => vec![1],
            Opcode::Closure => vec![2, 1], // [const index, num free vars]
            Opcode::CurrClosure => vec![1],
            Opcode::GetFree => vec![],
        }
    }
}

impl From<u8> for Opcode {
    fn from(value: u8) -> Self {
        unsafe { std::mem::transmute(value) }
    }
}

impl From<bool> for Opcode {
    fn from(value: bool) -> Self {
        if value { Opcode::True } else { Opcode::False }
    }
}

impl From<&Prefix> for Opcode {
    fn from(value: &Prefix) -> Self {
        match value {
            Prefix::Minus => Opcode::Minus,
            Prefix::Not => Opcode::Bang,
        }
    }
}

impl From<&Infix> for Opcode {
    fn from(value: &Infix) -> Self {
        match value {
            Infix::Add => Opcode::Add,
            Infix::Sub => Opcode::Sub,
            Infix::Mul => Opcode::Mul,
            Infix::Div => Opcode::Div,
            Infix::EQ => Opcode::EQ,
            Infix::NEQ => Opcode::NEQ,
            Infix::LT => Opcode::LT,
            Infix::LTE => Opcode::LTE,
            Infix::GT => Opcode::GT,
            Infix::GTE => Opcode::GTE,
            Infix::Rem => Opcode::Rem,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Default)]
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
                .map(|b| format!("{:02X}", b))
                .collect::<Vec<String>>()
                .join(" ")
        )
    }
}

#[derive(Clone, Debug)]
pub struct Bytecode {
    pub instructions: Instructions,
    pub constants: Vec<Rc<Object>>,
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

pub fn make(opcode: Opcode, operands: &[usize]) -> Vec<u8> {
    let op_widths = opcode.operand_widths();
    let len = op_widths.iter().map(|&b| b as usize).sum::<usize>() + 1;

    let mut instruction = vec![0; len];
    instruction[0] = opcode as u8;

    let mut offset = 1;
    for (o, w) in operands.iter().zip(op_widths) {
        match w {
            2 => {
                instruction[offset] = (o >> 8) as u8;
                instruction[offset + 1] = *o as u8;
            }
            1 => instruction[offset] = *o as u8,
            _ => {}
        }
        offset += w as usize;
    }

    instruction
}

pub fn read_operands(opcode: &Opcode, ins: &[u8]) -> (Vec<usize>, usize) {
    let widths = opcode.operand_widths();

    let mut operands = vec![];
    let mut offset = 0;

    for w in widths {
        match w {
            2 => {
                operands.push(read_u16(&ins[offset..offset + 2]) as usize);
                offset += 2;
            }
            1 => {
                operands.push(ins[offset] as usize);
                offset += 1;
            }
            _ => {}
        }
    }

    (operands, offset)
}

pub fn read_u16(ins: &[u8]) -> u16 {
    u16::from_be_bytes(ins[0..2].try_into().unwrap())
}
