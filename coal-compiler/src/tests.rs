use std::vec;

use coal_objects::Constant;

use super::{Compiler, Instructions, Opcode};

fn test(tests: &[(&str, &[Constant], Instructions)]) {
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
                .collect::<Vec<Constant>>()
        );

        assert_eq!(instructions, &bytecode.instructions);
    }
}

#[test]
fn test_compile_arithmetic() {
    test(&[(
        "1 + 2",
        &[Constant::I32(1), Constant::I32(2)],
        Instructions::from(vec![
            Instructions::new(Opcode::Const, &[0]),
            Instructions::new(Opcode::Const, &[1]),
            Instructions::new(Opcode::Add, &[]),
            Instructions::new(Opcode::Pop, &[]),
        ]),
    )]);
}

#[test]
fn test_compile_bools() {
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
            &[Constant::I32(1), Constant::I32(2)],
            Instructions::from(vec![
                Instructions::new(Opcode::Const, &[0]),
                Instructions::new(Opcode::Const, &[1]),
                Instructions::new(Opcode::GT, &[]),
                Instructions::new(Opcode::Pop, &[]),
            ]),
        ),
        (
            "1 < 2",
            &[Constant::I32(1), Constant::I32(2)],
            Instructions::from(vec![
                Instructions::new(Opcode::Const, &[0]),
                Instructions::new(Opcode::Const, &[1]),
                Instructions::new(Opcode::LT, &[]),
                Instructions::new(Opcode::Pop, &[]),
            ]),
        ),
        (
            "1 == 2",
            &[Constant::I32(1), Constant::I32(2)],
            Instructions::from(vec![
                Instructions::new(Opcode::Const, &[0]),
                Instructions::new(Opcode::Const, &[1]),
                Instructions::new(Opcode::EQ, &[]),
                Instructions::new(Opcode::Pop, &[]),
            ]),
        ),
        (
            "1 != 2",
            &[Constant::I32(1), Constant::I32(2)],
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

#[test]
fn test_compile_global_let_stmts() {
    test(&[
        (
            r#"
                let one = 1;
                let two = 2;
            "#,
            &[Constant::I32(1), Constant::I32(2)],
            Instructions::from(vec![
                Instructions::new(Opcode::Const, &[0]),
                Instructions::new(Opcode::SetGlobal, &[0]),
                Instructions::new(Opcode::Const, &[1]),
                Instructions::new(Opcode::SetGlobal, &[1]),
            ]),
        ),
        (
            r#"
                let one = 1;
                one;
            "#,
            &[Constant::I32(1)],
            Instructions::from(vec![
                Instructions::new(Opcode::Const, &[0]),
                Instructions::new(Opcode::SetGlobal, &[0]),
                Instructions::new(Opcode::GetGlobal, &[0]),
                Instructions::new(Opcode::Pop, &[]),
            ]),
        ),
        (
            r#"
                let one = 1;
                let two = one;
                two;
            "#,
            &[Constant::I32(1)],
            Instructions::from(vec![
                Instructions::new(Opcode::Const, &[0]),
                Instructions::new(Opcode::SetGlobal, &[0]),
                Instructions::new(Opcode::GetGlobal, &[0]),
                Instructions::new(Opcode::SetGlobal, &[1]),
                Instructions::new(Opcode::GetGlobal, &[1]),
                Instructions::new(Opcode::Pop, &[]),
            ]),
        ),
    ]);
}

#[test]
fn test_compile_conditionals() {
    test(&[
        (
            "if true { 1 }",
            &[Constant::I32(1)],
            Instructions::from(vec![
                // 0000
                (Opcode::True, vec![]),
                // 0001
                (Opcode::JumpIfNot, vec![10]),
                // 0004
                (Opcode::Const, vec![0]),
                // 0007
                (Opcode::Jump, vec![11]),
                // 0010
                (Opcode::Nil, vec![]),
                // 0011
                (Opcode::Pop, vec![]),
            ]),
        ),
        (
            "if true { 1 } else { 2 }",
            &[Constant::I32(1), Constant::I32(2)],
            Instructions::from(vec![
                // 0000
                (Opcode::True, vec![]),
                // 0001
                (Opcode::JumpIfNot, vec![10]),
                // 0004
                (Opcode::Const, vec![0]),
                // 0007
                (Opcode::Jump, vec![13]),
                // 0010
                (Opcode::Const, vec![1]),
                // 0013
                (Opcode::Pop, vec![]),
            ]),
        ),
    ]);
}
