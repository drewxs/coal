use coal_compiler::Compiler;
use coal_objects::{FALSE, Object, TRUE};

use crate::VM;

fn test(tests: &[(&str, Object)]) {
    for (input, expected) in tests {
        let mut compiler = Compiler::new();
        let bytecode = compiler
            .compile(input)
            .unwrap_or_else(|err| panic!("failed to compile:\n{input}\nerrors:\n{err:?}"));

        let mut vm = VM::from(bytecode);
        vm.run();

        let actual = vm.last_stack_obj();
        if *expected != *actual {
            panic!("input: {input}\nexpected: {expected}\nactual: {actual}",);
        }
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
                x + 2;
            } else {
                y + 3;
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
        (
            r#"
            if true { 1 }
            elif 1 < 2 { 2 }
            else { 3 }
            "#,
            Object::I32(1),
        ),
        (
            r#"
            if false { 1 }
            elif 1 < 2 { 2 }
            else { 3 }
            "#,
            Object::I32(2),
        ),
        (
            r#"
            if false { 1 }
            elif 1 > 2 { 2 }
            else { 3 }
            "#,
            Object::I32(3),
        ),
        (
            r#"
            if false { 1 }
            elif 1 > 2 { 2 }
            elif 1 < 2 { 3 }
            else { 4 }
            "#,
            Object::I32(3),
        ),
    ]);
}

#[test]
fn test_run_index_exprs() {
    test(&[
        ("[1, 2, 3][0]", Object::I32(1)),
        ("[1, 2, 3][1 + 1]", Object::I32(3)),
        ("[[1, 1, 1]][0][0]", Object::I32(1)),
        ("[1, 2, 3][99]", Object::Nil),
        ("[][0]", Object::Nil),
        ("[][-1]", Object::Nil),
        ("[1, 2][-1]", Object::I32(2)),
        ("[1, 2][-2]", Object::I32(1)),
        ("{1: 1, 2: 2}[1]", Object::I32(1)),
        ("{1: 1, 2: 2}[2]", Object::I32(2)),
        ("{1: 1}[0]", Object::Nil),
        ("let m: map[i32, i32] = {}; m[0]", Object::Nil),
        (r#""asdf"[0]"#, Object::Str(String::from("a"))),
        (r#""asdf"[2]"#, Object::Str(String::from("d"))),
        (r#""asdf"[4]"#, Object::Nil),
        (r#""asdf"[-1]"#, Object::Str(String::from("f"))),
    ]);
}

#[test]
fn test_run_functions_without_args() {
    test(&[
        (
            r#"
            fn five_plus_ten() -> i32 {
                return 5 + 10;
            }
            five_plus_ten();
            "#,
            Object::I32(15),
        ),
        (
            r#"
            fn one() -> i32 {
                return 1;
            }
            fn two() -> i32 {
                return 2;
            }
            one() + two();
            "#,
            Object::I32(3),
        ),
    ]);
}

#[test]
fn test_run_closure() {
    test(&[
        (
            r#"
            fn f() -> i32 {
                return 1;
            }
            f();
            "#,
            Object::I32(1),
        ),
        (
            r#"
            fn f(i: i32) -> i32 {
                return i;
            }
            f(1);
            "#,
            Object::I32(1),
        ),
        (
            r#"
            fn f(i: i32) -> i32 {
                return i + 1;
            }
            f(1);
            "#,
            Object::I32(2),
        ),
    ]);
}

#[test]
fn test_run_while_loops() {
    test(&[
        (
            r#"
            let x: i32 = 0;
            while x < 10 {
                x += 1;
            }
            x
            "#,
            Object::I32(10),
        ),
        (
            r#"
            let sum: i32 = 0;
            let i: i32 = 0;
            while i < 5 {
                sum += i;
                i += 1;
            }
            sum
            "#,
            Object::I32(10),
        ),
        (
            r#"
            let x: i32 = 0;
            while x < 1000 {
                x += 1;
            }
            x
            "#,
            Object::I32(1000),
        ),
        (
            r#"
            let x: i32 = 10;
            while x > 0 {
                x -= 1;
            }
            x
            "#,
            Object::I32(0),
        ),
    ]);
}

#[test]
fn test_run_for_range_loops() {
    test(&[
        (
            r#"
            let sum: i32 = 0;
            for i in 0..5 {
                sum += i;
            }
            sum
            "#,
            Object::I32(10),
        ),
        (
            r#"
            let sum: i32 = 0;
            for i in 0..10 {
                sum += i;
            }
            sum
            "#,
            Object::I32(45),
        ),
        (
            r#"
            let count: i32 = 0;
            for i in 0..100 {
                count += 1;
            }
            count
            "#,
            Object::I32(100),
        ),
        (
            r#"
            let last: i32 = 0;
            for i in 5..10 {
                last = i;
            }
            last
            "#,
            Object::I32(9),
        ),
    ]);
}

#[test]
fn test_run_for_list_loops() {
    test(&[
        (
            r#"
            let sum: i32 = 0;
            for x in [1, 2, 3, 4, 5] {
                sum += x;
            }
            sum;
            "#,
            Object::I32(15),
        ),
        (
            r#"
            let sum: i32 = 0;
            for x in [10, 20, 30] {
                sum += x;
            }
            sum;
            "#,
            Object::I32(60),
        ),
        (
            r#"
            let count: i32 = 0;
            for x in [1, 2, 3, 4, 5] {
                count += 1;
            }
            count;
            "#,
            Object::I32(5),
        ),
        (
            r#"
            let last: i32 = 0;
            for x in [5, 10, 15, 20] {
                last = x;
            }
            last;
            "#,
            Object::I32(20),
        ),
    ]);
}

#[test]
fn test_run_loops_in_scope() {
    test(&[
        (
            r#"
            fn f() {
                let x: i32 = 0;
                while x < 10 {
                    x += 1;
                }
                return x;
            }
            f();
            "#,
            Object::I32(10),
        ),
        (
            r#"
            fn f() {
                let sum: i32 = 0;
                for i in 0..5 {
                    sum += i;
                }
                return sum;
            }
            f();
            "#,
            Object::I32(10),
        ),
        (
            r#"
            fn f() {
                let sum: i32 = 0;
                for x in [1, 2, 3, 4] {
                    sum += x;
                }
                return sum;
            }
            f();
            "#,
            Object::I32(10),
        ),
    ]);
}

#[test]
fn test_run_fib() {
    test(&[(
        r#"
            fn fib(n: u32) -> i128 {
                let x: i128 = 0;
                let y: i128 = 1;

                for i in 0..n {
                    let tmp: i128 = x;
                    x = y;
                    y = tmp + y;
                }

                return x;
            }
            fib(10);
            "#,
        Object::I128(55),
    )]);
}
