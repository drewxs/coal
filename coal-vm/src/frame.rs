use coal_compiler::Instructions;
use coal_objects::Closure;

#[derive(Clone, Debug, Default)]
pub struct Frame {
    pub cl: Closure,
    pub ip: i32,
    pub base_ptr: usize,
}

impl Frame {
    pub fn new(cl: Closure, base_ptr: usize) -> Self {
        Frame {
            cl,
            ip: -1,
            base_ptr,
        }
    }

    pub fn instructions(&self) -> Instructions {
        Instructions(self.cl.func.instructions.clone())
    }
}
