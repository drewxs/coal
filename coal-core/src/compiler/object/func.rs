use std::{
    hash::{Hash, Hasher},
    rc::Rc,
};

use crate::{Param, Stmt, Type};

use super::Object;

#[derive(Clone, Debug, PartialEq)]
pub struct Func {
    pub name: String,
    pub args: Vec<Param>,
    pub body: Vec<Stmt>,
    pub ret_t: Type,
}

impl Hash for Func {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.name.hash(state);
        self.args.len().hash(state);
        self.args.hash(state);
        self.ret_t.hash(state);
    }
}

#[derive(Clone, Debug, PartialEq, Default)]
pub struct CompiledFunc {
    pub instructions: Vec<u8>,
    pub n_locals: usize,
    pub n_params: usize,
}

impl Hash for CompiledFunc {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.instructions.len().hash(state);
        self.instructions.hash(state);
        self.n_locals.hash(state);
        self.n_params.hash(state);
    }
}

#[derive(Clone, Debug, PartialEq, Default)]
pub struct Closure {
    pub func: Rc<CompiledFunc>,
    pub free: Vec<Rc<Object>>,
}

impl Hash for Closure {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.func.instructions.len().hash(state);
        self.func.instructions.hash(state);
        self.func.n_locals.hash(state);
        self.func.n_params.hash(state);
        self.free.len().hash(state);
        self.free.hash(state);
    }
}
