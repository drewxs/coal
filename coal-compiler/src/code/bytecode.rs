use std::rc::Rc;

use rkyv::{Archive, Deserialize, Serialize, from_bytes, rancor};

use coal_objects::Constant;

use super::Instructions;

#[derive(Clone, Debug, Default, Archive, Serialize, Deserialize)]
pub struct Bytecode {
    pub instructions: Instructions,
    pub constants: Vec<Rc<Constant>>,
}

impl From<Vec<u8>> for Bytecode {
    fn from(bytes: Vec<u8>) -> Self {
        from_bytes::<Self, rancor::Error>(&bytes).unwrap()
    }
}
