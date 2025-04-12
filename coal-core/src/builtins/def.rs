use std::rc::Rc;

use crate::{Builtin, Object};

pub trait Def {
    fn name() -> &'static str;
    fn def() -> Builtin;
    fn func(args: &[Rc<Object>]) -> Option<Rc<Object>>;
}
