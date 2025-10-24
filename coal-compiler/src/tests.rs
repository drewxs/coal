use std::{rc::Rc, vec};

use coal_objects::{CompiledFunc, Constant};

use crate::{Symbol, SymbolScope, SymbolTable};

use super::{Compiler, Instructions, Opcode};

fn test(tests: &[(&str, &[Constant], Instructions)]) {
    for (input, constants, instructions) in tests {
        let mut compiler = Compiler::new();
        compiler.compile(input).unwrap();
        let bytecode = compiler.bytecode();

        for (expected, actual) in constants.iter().zip(bytecode.constants.iter()) {
            if *expected != **actual {
                panic!(
                    "input: {input}\nexpected: {constants:?}\nactual: {:?}",
                    bytecode.constants
                );
            }
        }

        for (expected, actual) in instructions.iter().zip(bytecode.instructions.iter()) {
            if expected != actual {
                panic!(
                    "input: {input}\nexpected: {instructions:?}\nactual: {:?}",
                    bytecode.instructions
                );
            }
        }
    }
}

#[test]
fn test_compile_arithmetic() {
    test(&[(
        "1 + 2",
        &[Constant::I32(1), Constant::I32(2)],
        Instructions::from(vec![
            (Opcode::Const, vec![0]),
            (Opcode::Const, vec![1]),
            (Opcode::Add, vec![]),
            (Opcode::Pop, vec![]),
        ]),
    )]);
}

#[test]
fn test_compile_bools() {
    test(&[
        (
            "true",
            &[],
            Instructions::from(vec![(Opcode::True, vec![]), (Opcode::Pop, vec![])]),
        ),
        (
            "false",
            &[],
            Instructions::from(vec![(Opcode::False, vec![]), (Opcode::Pop, vec![])]),
        ),
        (
            "1 > 2",
            &[Constant::I32(1), Constant::I32(2)],
            Instructions::from(vec![
                (Opcode::Const, vec![0]),
                (Opcode::Const, vec![1]),
                (Opcode::GT, vec![]),
                (Opcode::Pop, vec![]),
            ]),
        ),
        (
            "1 < 2",
            &[Constant::I32(1), Constant::I32(2)],
            Instructions::from(vec![
                (Opcode::Const, vec![0]),
                (Opcode::Const, vec![1]),
                (Opcode::LT, vec![]),
                (Opcode::Pop, vec![]),
            ]),
        ),
        (
            "1 == 2",
            &[Constant::I32(1), Constant::I32(2)],
            Instructions::from(vec![
                (Opcode::Const, vec![0]),
                (Opcode::Const, vec![1]),
                (Opcode::EQ, vec![]),
                (Opcode::Pop, vec![]),
            ]),
        ),
        (
            "1 != 2",
            &[Constant::I32(1), Constant::I32(2)],
            Instructions::from(vec![
                (Opcode::Const, vec![0]),
                (Opcode::Const, vec![1]),
                (Opcode::NEQ, vec![]),
                (Opcode::Pop, vec![]),
            ]),
        ),
        (
            "true == false",
            &[],
            Instructions::from(vec![
                (Opcode::True, vec![]),
                (Opcode::False, vec![]),
                (Opcode::EQ, vec![]),
                (Opcode::Pop, vec![]),
            ]),
        ),
        (
            "true != false",
            &[],
            Instructions::from(vec![
                (Opcode::True, vec![]),
                (Opcode::False, vec![]),
                (Opcode::NEQ, vec![]),
                (Opcode::Pop, vec![]),
            ]),
        ),
        (
            "!true",
            &[],
            Instructions::from(vec![
                (Opcode::True, vec![]),
                (Opcode::Bang, vec![]),
                (Opcode::Pop, vec![]),
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
                (Opcode::Const, vec![0]),
                (Opcode::SetGlobal, vec![0]),
                (Opcode::Const, vec![1]),
                (Opcode::SetGlobal, vec![1]),
            ]),
        ),
        (
            r#"
                let one = 1;
                one;
            "#,
            &[Constant::I32(1)],
            Instructions::from(vec![
                (Opcode::Const, vec![0]),
                (Opcode::SetGlobal, vec![0]),
                (Opcode::GetGlobal, vec![0]),
                (Opcode::Pop, vec![]),
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
                (Opcode::Const, vec![0]),
                (Opcode::SetGlobal, vec![0]),
                (Opcode::GetGlobal, vec![0]),
                (Opcode::SetGlobal, vec![1]),
                (Opcode::GetGlobal, vec![1]),
                (Opcode::Pop, vec![]),
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
                (Opcode::JumpFalse, vec![10]),
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
            "if false { 1 }",
            &[Constant::I32(1)],
            Instructions::from(vec![
                // 0000
                (Opcode::False, vec![]),
                // 0001
                (Opcode::JumpFalse, vec![10]),
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
                (Opcode::JumpFalse, vec![10]),
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
        (
            "if false { 1 } elif true { 2 }",
            &[Constant::I32(1), Constant::I32(2)],
            Instructions::from(vec![
                // 0000
                (Opcode::False, vec![]),
                // 0001
                (Opcode::JumpFalse, vec![10]),
                // 0004
                (Opcode::Const, vec![0]),
                // 0007
                (Opcode::Jump, vec![21]),
                // 0010
                (Opcode::True, vec![]),
                // 0011
                (Opcode::JumpFalse, vec![20]),
                // 0014
                (Opcode::Const, vec![1]),
                // 0017
                (Opcode::Jump, vec![21]),
                // 0020
                (Opcode::Nil, vec![]),
                // 0021
                (Opcode::Pop, vec![]),
            ]),
        ),
        (
            r#"
            if false { 1 }
            elif true { 2 }
            else { 3 }
            "#,
            &[Constant::I32(1), Constant::I32(2), Constant::I32(3)],
            Instructions::from(vec![
                // 0000
                (Opcode::False, vec![]),
                // 0001
                (Opcode::JumpFalse, vec![10]),
                // 0004
                (Opcode::Const, vec![0]),
                // 0007
                (Opcode::Jump, vec![23]),
                // 0010
                (Opcode::True, vec![]),
                // 0011
                (Opcode::JumpFalse, vec![20]),
                // 0014
                (Opcode::Const, vec![1]),
                // 0017
                (Opcode::Jump, vec![23]),
                // 0020
                (Opcode::Const, vec![2]),
                // 0023
                (Opcode::Pop, vec![]),
            ]),
        ),
    ]);
}

#[test]
fn test_compile_index() {
    test(&[
        (
            "[1, 2, 3][0]",
            &[
                Constant::I32(1),
                Constant::I32(2),
                Constant::I32(3),
                Constant::I32(0),
            ],
            Instructions::from(vec![
                (Opcode::Const, vec![0]),
                (Opcode::Const, vec![1]),
                (Opcode::Const, vec![2]),
                (Opcode::List, vec![3]),
                (Opcode::Const, vec![3]),
                (Opcode::Index, vec![]),
                (Opcode::Pop, vec![]),
            ]),
        ),
        (
            "[1, 2, 3][1 + 1]",
            &[
                Constant::I32(1),
                Constant::I32(2),
                Constant::I32(3),
                Constant::I32(1),
                Constant::I32(1),
            ],
            Instructions::from(vec![
                (Opcode::Const, vec![0]),
                (Opcode::Const, vec![1]),
                (Opcode::Const, vec![2]),
                (Opcode::List, vec![3]),
                (Opcode::Const, vec![3]),
                (Opcode::Const, vec![4]),
                (Opcode::Add, vec![]),
                (Opcode::Index, vec![]),
                (Opcode::Pop, vec![]),
            ]),
        ),
        (
            "{1: 2}[2 - 1]",
            &[
                Constant::I32(1),
                Constant::I32(2),
                Constant::I32(2),
                Constant::I32(1),
            ],
            Instructions::from(vec![
                (Opcode::Const, vec![0]),
                (Opcode::Const, vec![1]),
                (Opcode::Hash, vec![2]),
                (Opcode::Const, vec![2]),
                (Opcode::Const, vec![3]),
                (Opcode::Sub, vec![]),
                (Opcode::Index, vec![]),
                (Opcode::Pop, vec![]),
            ]),
        ),
    ]);
}

#[test]
fn test_compiler_scopes() {
    let mut c = Compiler::new();
    assert_eq!(c.scope_idx, 0);

    c.emit(Opcode::Mul, &[]);
    c.enter_scope();
    assert_eq!(c.scope_idx, 1);

    c.emit(Opcode::Sub, &[]);
    assert_eq!(c.scopes[c.scope_idx].instructions.len(), 1);
    let last = &c.scopes[c.scope_idx].last_instruction;
    assert_eq!(last.opcode, Opcode::Sub);
    c.leave_scope();
    assert_eq!(c.scope_idx, 0);

    c.emit(Opcode::Add, &[]);
    assert_eq!(c.scopes[c.scope_idx].instructions.len(), 2);
    let last = &c.scopes[c.scope_idx].last_instruction;
    assert_eq!(last.opcode, Opcode::Add);
    let prev = &c.scopes[c.scope_idx].prev_instruction;
    assert_eq!(prev.opcode, Opcode::Mul);
}

#[test]
fn test_resolve_free() {
    let mut global = SymbolTable::new();
    global.define("a");
    global.define("b");

    let mut first_local = SymbolTable::new_enclosed(&global);
    first_local.define("c");
    first_local.define("d");

    let mut second_local = SymbolTable::new_enclosed(&first_local);
    second_local.define("e");
    second_local.define("f");

    for (name, table, expected_symbols, expected_free) in [
        (
            "first_local",
            &first_local,
            vec![
                Rc::new(Symbol {
                    name: String::from("a"),
                    scope: SymbolScope::Global,
                    idx: 0,
                }),
                Rc::new(Symbol {
                    name: String::from("b"),
                    scope: SymbolScope::Global,
                    idx: 1,
                }),
                Rc::new(Symbol {
                    name: String::from("c"),
                    scope: SymbolScope::Local,
                    idx: 0,
                }),
                Rc::new(Symbol {
                    name: String::from("d"),
                    scope: SymbolScope::Local,
                    idx: 1,
                }),
            ],
            vec![],
        ),
        (
            "second_local",
            &second_local,
            vec![
                Rc::new(Symbol {
                    name: String::from("a"),
                    scope: SymbolScope::Global,
                    idx: 0,
                }),
                Rc::new(Symbol {
                    name: String::from("b"),
                    scope: SymbolScope::Global,
                    idx: 1,
                }),
                Rc::new(Symbol {
                    name: String::from("c"),
                    scope: SymbolScope::Free,
                    idx: 0,
                }),
                Rc::new(Symbol {
                    name: String::from("d"),
                    scope: SymbolScope::Free,
                    idx: 1,
                }),
                Rc::new(Symbol {
                    name: String::from("e"),
                    scope: SymbolScope::Local,
                    idx: 0,
                }),
                Rc::new(Symbol {
                    name: String::from("f"),
                    scope: SymbolScope::Local,
                    idx: 1,
                }),
            ],
            vec![
                Rc::new(Symbol {
                    name: String::from("c"),
                    scope: SymbolScope::Local,
                    idx: 0,
                }),
                Rc::new(Symbol {
                    name: String::from("d"),
                    scope: SymbolScope::Local,
                    idx: 1,
                }),
            ],
        ),
    ] {
        for sym in expected_symbols {
            if let Some(s) = table.get(&sym.name) {
                if s != sym {
                    panic!("expected symbol {sym:?} in table {name}, got {s:?}");
                }
            } else {
                panic!("expected symbol {sym:?} in table {name}");
            }
        }

        assert_eq!(*table.free.borrow(), expected_free);
    }
}

#[test]
fn test_compile_functions() {
    test(&[(
        r#"fn f() { return 1 + 2; }"#,
        &[
            Constant::I32(1),
            Constant::I32(2),
            Constant::Func(CompiledFunc {
                instructions: vec![1, 0, 0, 1, 0, 1, 5, 26],
                n_locals: 0,
                n_params: 0,
            }),
        ],
        Instructions::from(vec![
            (Opcode::Closure, vec![2, 0]),
            (Opcode::SetGlobal, vec![0]),
            (Opcode::Nil, vec![]),
            (Opcode::Nil, vec![]),
        ]),
    )]);
}
