use crate::{Compiler, VM};

use super::{Object, FALSE, TRUE};

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
