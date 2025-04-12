use super::code::{make, read_operands, Instructions, Opcode};

fn test_opcodes(tests: &[(Opcode, &[usize], &[u8])]) {
    for (op, operands, expected) in tests {
        let instruction = Instructions::new(*op, &operands);

        for (i, byte) in expected.iter().enumerate() {
            assert_eq!(instruction[i] as u8, *byte);
        }
    }
}

fn test_opcodes_str(tests: &[(Opcode, &[usize], &str)]) {
    for (op, operands, expected) in tests {
        let instruction = Instructions::new(*op, &operands);
        assert_eq!(expected.to_string(), instruction.to_string());
    }
}

#[test]
fn test_instructions() {
    test_opcodes(&[
        (Opcode::Const, &[65534], &[Opcode::Const as u8, 255, 254]),
        (Opcode::Add, &[], &[Opcode::Add as u8]),
    ]);
}

#[test]
fn test_instructions_str() {
    test_opcodes_str(&[
        (Opcode::Const, &[1], "01 00 01"),
        (Opcode::Const, &[2], "01 00 02"),
        (Opcode::Const, &[65535], "01 FF FF"),
    ]);
}

#[test]
fn test_read_operands() {
    let tests: Vec<(Opcode, &[usize], usize)> = vec![
        (Opcode::Const, &[65535], 2),
        (Opcode::GetLocal, &[255], 1),
        (Opcode::Closure, &[65535, 255], 3),
    ];

    for (op, expected_operands, expected_bytes) in tests {
        let ins = make(op, &expected_operands);
        let (operands_read, actual_bytes) = read_operands(&op, &ins[1..]);

        assert_eq!(expected_operands, operands_read);
        assert_eq!(expected_bytes, actual_bytes)
    }
}
