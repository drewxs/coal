use std::rc::Rc;

use coal_objects::Closure;

#[derive(Clone, Debug)]
pub struct Frame {
    pub cl: Rc<Closure>,
    pub ip: i32,
    pub base_ptr: usize,
}

impl Default for Frame {
    fn default() -> Self {
        Frame {
            cl: Rc::new(Closure::default()),
            ip: -1,
            base_ptr: 0,
        }
    }
}

impl Frame {
    pub fn new(cl: Rc<Closure>, base_ptr: usize) -> Self {
        Frame {
            cl,
            ip: -1,
            base_ptr,
        }
    }

    pub fn instructions(&self) -> &[u8] {
        &self.cl.func.instructions
    }
}
