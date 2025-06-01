use coal_core::{Infix, Prefix};

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
    JumpFalse,

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
            Opcode::Const => vec![2], // [index]
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
            Opcode::Jump => vec![2],      // [pos]
            Opcode::JumpFalse => vec![2], // [pos]
            Opcode::GetGlobal => vec![2], // [index]
            Opcode::SetGlobal => vec![2], // [index]
            Opcode::List => vec![2],      // [count]
            Opcode::Hash => vec![2],      // [count]
            Opcode::Index => vec![],
            Opcode::Call => vec![1], // [num args]
            Opcode::RetVal => vec![],
            Opcode::Ret => vec![],
            Opcode::GetLocal => vec![1],    // [index]
            Opcode::SetLocal => vec![1],    // [index]
            Opcode::GetBuiltin => vec![1],  // [index]
            Opcode::Closure => vec![2, 1],  // [const index, num free vars]
            Opcode::CurrClosure => vec![1], // [index]
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
