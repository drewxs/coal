use std::rc::Rc;

use bincode::{Decode, Encode, decode_from_slice};

use coal_objects::Constant;

use super::Instructions;

#[derive(Clone, Debug, Default, Encode, Decode)]
pub struct Bytecode {
    pub instructions: Instructions,
    pub constants: Vec<Rc<Constant>>,
}

impl From<Vec<u8>> for Bytecode {
    fn from(bytes: Vec<u8>) -> Self {
        let config = bincode::config::standard();
        decode_from_slice(&bytes, config).unwrap().0
    }
}
