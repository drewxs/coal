use crate::{Builtin, Object};

pub trait Def {
    fn name() -> &'static str;
    fn def() -> Builtin;
    fn func(args: &[Object]) -> Option<Object>;
}
