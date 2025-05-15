mod bytecode;
mod instructions;
mod opcode;
mod serialization;

#[cfg(test)]
mod tests;

pub use bytecode::*;
pub use instructions::*;
pub use opcode::*;
pub use serialization::*;
