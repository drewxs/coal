use std::rc::Rc;

use rkyv::{Archive, Deserialize, Serialize};

use super::{CompiledFunc, Object};

#[derive(Clone, Debug, PartialEq, Archive, Serialize, Deserialize)]
pub enum Constant {
    U32(u32),
    U64(u64),
    I32(i32),
    I64(i64),
    I128(i128),
    F32(f32),
    F64(f64),
    Str(String),
    Func(CompiledFunc),
}

impl From<Object> for Constant {
    fn from(o: Object) -> Self {
        match o {
            Object::U32(u) => Constant::U32(u),
            Object::U64(u) => Constant::U64(u),
            Object::I32(i) => Constant::I32(i),
            Object::I64(i) => Constant::I64(i),
            Object::I128(i) => Constant::I128(i),
            Object::F32(f) => Constant::F32(f),
            Object::F64(f) => Constant::F64(f),
            Object::Str(s) => Constant::Str(Rc::try_unwrap(s).unwrap_or_else(|r| (*r).clone())),
            Object::CompiledFunc(f) => {
                Constant::Func(Rc::try_unwrap(f).unwrap_or_else(|r| (*r).clone()))
            }
            _ => panic!("invalid constant"),
        }
    }
}
