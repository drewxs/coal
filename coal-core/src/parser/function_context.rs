use crate::StructDecl;

#[derive(PartialEq)]
pub enum FunctionContext<'a> {
    Standard,
    Struct(&'a StructDecl),
}
