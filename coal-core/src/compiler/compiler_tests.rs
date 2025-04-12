use crate::Object;

use super::{
    code::{Instructions, Opcode},
    Compiler,
};

fn test(tests: &[(&str, &[Object], Instructions)]) {
    for (input, constants, instructions) in tests {
        let mut compiler = Compiler::new();
        compiler.compile(input).unwrap();
        let bytecode = compiler.bytecode();

        assert_eq!(
            *constants,
            bytecode
                .constants
                .into_iter()
                .map(|rc| (*rc).clone())
                .collect::<Vec<Object>>()
        );

        assert_eq!(instructions, &bytecode.instructions);
    }
}

#[test]
fn test_arithmetic() {
    test(&[(
        "1 + 2",
        &[Object::I32(1), Object::I32(2)],
        Instructions::from(vec![
            Instructions::new(Opcode::Const, &[0]),
            Instructions::new(Opcode::Const, &[1]),
            Instructions::new(Opcode::Add, &[]),
            Instructions::new(Opcode::Pop, &[]),
        ]),
    )]);
}
