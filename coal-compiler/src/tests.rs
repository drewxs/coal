use coal_objects::Object;

use super::{Compiler, Instructions, Opcode};

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

#[test]
fn test_bools() {
    test(&[
        (
            "true",
            &[],
            Instructions::from(vec![
                Instructions::new(Opcode::True, &[]),
                Instructions::new(Opcode::Pop, &[]),
            ]),
        ),
        (
            "false",
            &[],
            Instructions::from(vec![
                Instructions::new(Opcode::False, &[]),
                Instructions::new(Opcode::Pop, &[]),
            ]),
        ),
        (
            "1 > 2",
            &[Object::I32(1), Object::I32(2)],
            Instructions::from(vec![
                Instructions::new(Opcode::Const, &[0]),
                Instructions::new(Opcode::Const, &[1]),
                Instructions::new(Opcode::GT, &[]),
                Instructions::new(Opcode::Pop, &[]),
            ]),
        ),
        (
            "1 < 2",
            &[Object::I32(1), Object::I32(2)],
            Instructions::from(vec![
                Instructions::new(Opcode::Const, &[0]),
                Instructions::new(Opcode::Const, &[1]),
                Instructions::new(Opcode::LT, &[]),
                Instructions::new(Opcode::Pop, &[]),
            ]),
        ),
        (
            "1 == 2",
            &[Object::I32(1), Object::I32(2)],
            Instructions::from(vec![
                Instructions::new(Opcode::Const, &[0]),
                Instructions::new(Opcode::Const, &[1]),
                Instructions::new(Opcode::EQ, &[]),
                Instructions::new(Opcode::Pop, &[]),
            ]),
        ),
        (
            "1 != 2",
            &[Object::I32(1), Object::I32(2)],
            Instructions::from(vec![
                Instructions::new(Opcode::Const, &[0]),
                Instructions::new(Opcode::Const, &[1]),
                Instructions::new(Opcode::NEQ, &[]),
                Instructions::new(Opcode::Pop, &[]),
            ]),
        ),
        (
            "true == false",
            &[],
            Instructions::from(vec![
                Instructions::new(Opcode::True, &[]),
                Instructions::new(Opcode::False, &[]),
                Instructions::new(Opcode::EQ, &[]),
                Instructions::new(Opcode::Pop, &[]),
            ]),
        ),
        (
            "true != false",
            &[],
            Instructions::from(vec![
                Instructions::new(Opcode::True, &[]),
                Instructions::new(Opcode::False, &[]),
                Instructions::new(Opcode::NEQ, &[]),
                Instructions::new(Opcode::Pop, &[]),
            ]),
        ),
        (
            "!true",
            &[],
            Instructions::from(vec![
                Instructions::new(Opcode::True, &[]),
                Instructions::new(Opcode::Bang, &[]),
                Instructions::new(Opcode::Pop, &[]),
            ]),
        ),
    ]);
}
