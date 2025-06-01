use coal_compiler::Compiler;
use coal_objects::{FALSE, Object, TRUE};

use crate::VM;

fn test(tests: &[(&str, Object)]) {
    for (input, expected) in tests {
        let mut compiler = Compiler::new();
        let bytecode = compiler.compile(input).unwrap();

        let mut vm = VM::from(bytecode);
        vm.run().unwrap();

        assert_eq!(*expected, *vm.last_stack_obj());
    }
}

#[test]
fn test_run_literal() {
    test(&[
        ("5", Object::I32(5)),
        ("3.14", Object::F64(3.14)),
        ("true", TRUE),
        (r#""foo""#, Object::Str(String::from("foo"))),
    ]);
}

#[test]
fn test_run_prefix() {
    test(&[
        ("-5", Object::I32(-5)),
        ("-10", Object::I32(-10)),
        ("!true", FALSE),
        ("!!true", TRUE),
        ("!false", TRUE),
        ("!!false", FALSE),
        ("!5", FALSE),
        ("!!5", TRUE),
    ]);
}

#[test]
fn test_run_infix() {
    test(&[
        ("(7 + 2 * 3 / 2) % 3", Object::I32(1)),
        ("1 + 2 * 3 + 4 / 5", Object::I32(7)),
        ("(1 + 2 * 3 + 4 / 5) * 2 + -10", Object::I32(4)),
        ("true == true", TRUE),
        ("false == false", TRUE),
        ("true == false", FALSE),
        ("true != false", TRUE),
        ("(1 < 2) == true", TRUE),
        ("(1 > 2) != false", FALSE),
        (r#""foo" + "bar""#, Object::Str(String::from("foobar"))),
        (r#""foo" == "foo""#, TRUE),
        (r#""a" * 3"#, Object::Str(String::from("aaa"))),
        (r#""a" < "b""#, TRUE),
    ]);
}

#[test]
fn test_run_bool() {
    test(&[
        ("true", TRUE),
        ("false", FALSE),
        ("1 < 2", TRUE),
        ("1 > 2", FALSE),
        ("1 < 1", FALSE),
        ("1 > 1", FALSE),
        ("1 == 1", TRUE),
        ("1 != 1", FALSE),
        ("1 == 2", FALSE),
        ("1 != 2", TRUE),
        ("true == true", TRUE),
        ("false == false", TRUE),
        ("true == false", FALSE),
        ("true != false", TRUE),
        ("false != true", TRUE),
        ("(1 < 2) == true", TRUE),
        ("(1 < 2) == false", FALSE),
        ("(1 > 2) == true", FALSE),
        ("(1 > 2) == false", TRUE),
        ("!true", FALSE),
        ("!false", TRUE),
        ("!5", FALSE),
        ("!!true", TRUE),
        ("!!false", FALSE),
        ("!!5", TRUE),
    ]);
}

#[test]
fn test_run_global_let_stmts() {
    test(&[
        ("let one = 1; one", Object::I32(1)),
        ("let one = 1; let two = 2; one + two", Object::I32(3)),
        (
            "let one = 1; let two = one + one; one + two",
            Object::I32(3),
        ),
    ]);
}

#[test]
fn test_run_assign_stmts() {
    test(&[
        ("let x = 1; x = 2; x", Object::I32(2)),
        ("let x = 1; let y = 2; x = x + y; x", Object::I32(3)),
    ]);
}

#[test]
fn test_run_op_assign_stmts() {
    test(&[
        ("let x = 1; x += 2; x", Object::I32(3)),
        (
            "let x = 1; let y = 2; let z = 1; z += x + y; z",
            Object::I32(4),
        ),
    ]);
}

#[test]
fn test_run_str_exprs() {
    test(&[
        (r#""foo""#, Object::Str(String::from("foo"))),
        (r#""foo" + "bar""#, Object::Str(String::from("foobar"))),
        (r#""foo" == "foo""#, TRUE),
        (r#""a" * 3"#, Object::Str(String::from("aaa"))),
        (r#""a" < "b""#, TRUE),
    ]);
}

#[test]
fn test_run_conditionals() {
    test(&[
        ("if true { 1 }", Object::I32(1)),
        ("if false { 1 }", Object::Nil),
        (
            r#"
            let x = 1;
            let y = 2;
            if 2 * 3 > 5 {
                x + 2
            } else {
                y + 3
            }
            "#,
            Object::I32(3),
        ),
        (
            r#"
            let x = 1;
            let y = 2;
            if x < y {
                x += 9;
            } else {
                y *= 5;
            }
            x != y
            "#,
            TRUE,
        ),
    ]);
}
