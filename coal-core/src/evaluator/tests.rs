use std::assert_matches::assert_matches;

use test::Bencher;

use crate::{Expr, Ident, Infix, Stmt, Var, I32};

use super::{Evaluator, Object, FALSE, TRUE};

#[test]
fn test_eval_literal() {
    let tests = vec![
        ("5", Some(Object::I32(5))),
        ("3.14", Some(Object::F64(3.14))),
        ("true", Some(TRUE)),
        (r#""foo""#, Some(Object::Str(String::from("foo")))),
    ];

    let mut evaluator = Evaluator::default();

    for (expr, expected) in tests {
        assert_eq!(expected, evaluator.eval(expr));
    }
}

#[test]
fn test_eval_str_interpolation() {
    let tests = vec![
        (
            r#""not {!false} or {!!false}""#,
            Some(Object::Str(String::from("not true or false"))),
        ),
        (
            r#""(1 + \"{-false})""#,
            Some(Object::Str(String::from(r#"(1 + "0)"#))),
        ),
    ];

    let mut evaluator = Evaluator::default();

    for (expr, expected) in tests {
        assert_eq!(expected, evaluator.eval(expr));
    }
}

#[test]
fn test_eval_prefix() {
    let tests = vec![
        ("-5", Some(Object::I32(-5))),
        ("-10", Some(Object::I32(-10))),
        ("!true", Some(FALSE)),
        ("!!true", Some(TRUE)),
        ("!false", Some(TRUE)),
        ("!!false", Some(FALSE)),
        ("!5", Some(FALSE)),
        ("!!5", Some(TRUE)),
    ];

    let mut evaluator = Evaluator::default();

    for (expr, expected) in tests {
        assert_eq!(expected, evaluator.eval(expr));
    }
}

#[test]
fn test_eval_infix() {
    let tests = vec![
        ("(7 + 2 * 3 / 2) % 3", Some(Object::I32(1))),
        ("1 + 2 * 3 + 4 / 5", Some(Object::I32(7))),
        ("(1 + 2 * 3 + 4 / 5) * 2 + -10", Some(Object::I32(4))),
        ("8 // 3", Some(Object::I32(2))),
        ("10.4 // 2", Some(Object::F64(5.0))),
        ("true == true", Some(TRUE)),
        ("false == false", Some(TRUE)),
        ("true == false", Some(FALSE)),
        ("true != false", Some(TRUE)),
        ("(1 < 2) == true", Some(TRUE)),
        ("(1 > 2) != false", Some(FALSE)),
        (
            r#""foo" + "bar""#,
            Some(Object::Str(String::from("foobar"))),
        ),
        (r#""foo" == "foo""#, Some(TRUE)),
        (r#""a" * 3"#, Some(Object::Str(String::from("aaa")))),
        (r#""a" < "b""#, Some(TRUE)),
    ];

    let mut evaluator = Evaluator::default();

    for (expr, expected) in tests {
        assert_eq!(expected, evaluator.eval(expr));
    }
}

#[test]
fn test_eval_infix_invalid() {
    let tests = vec![
        r#""a" - "b""#,
        r#""a" / 3.14"#,
        r#"10 + "x""#,
        r#"10 - "x""#,
        r#"10 * "x""#,
        r#"10 / "x""#,
        r#"10 % "x""#,
        r#""x" + 10"#,
        r#""x" - 10"#,
        r#""x" / 10"#,
        r#""x" % 10"#,
    ];
    for input in tests {
        assert_matches!(Evaluator::default().eval(input), Some(Object::Error { .. }));
    }
}

#[test]
fn test_eval_if() {
    let tests = vec![
        ("if true { 10 }", Some(Object::I32(10))),
        ("if false { 10 }", None),
        ("if 1 { 10 }", Some(Object::I32(10))),
        ("if 1 < 2 { 10 }", Some(Object::I32(10))),
        ("if 1 > 2 { 10 }", None),
        ("if 1 > 2 { 10 } else { 20 }", Some(Object::I32(20))),
        ("if 1 < 2 { 10 } else { 20 }", Some(Object::I32(10))),
        (
            r#"if 1 < 2 {
                let x = 999;
                return 10;
            } else {
                return 20;
            }"#,
            Some(Object::I32(10)),
        ),
        (
            r#"if 1 > 2 {
                return 1;
            } else {
                return 2;
                return 3;
            }"#,
            Some(Object::I32(2)),
        ),
    ];

    let mut evaluator = Evaluator::default();

    for (expr, expected) in tests {
        assert_eq!(expected, evaluator.eval(expr));
    }
}

#[test]
fn test_eval_nested_if() {
    let tests = vec![
        (
            r#"if true {
                if true {
                    return 1;
                }
                return 2;
            } else {
                return 3;
            }"#,
            Some(Object::I32(1)),
        ),
        (
            r#"if true {
                if false {
                    return 1;
                }
                return 2;
            } else {
                return 3;
            }"#,
            Some(Object::I32(2)),
        ),
    ];

    let mut evaluator = Evaluator::default();

    for (expr, expected) in tests {
        assert_eq!(expected, evaluator.eval(expr));
    }
}

#[test]
fn test_eval_if_scope() {
    let input = r#"
        let x = 1;
        if true {
            let y = 2;
            let z = 3;
        }
    "#;

    let mut evaluator = Evaluator::default();
    evaluator.eval(input);

    assert_eq!(evaluator.env.borrow_mut().get("x"), Some(Object::I32(1)));
    assert_eq!(evaluator.env.borrow_mut().get("y"), None);
    assert_eq!(evaluator.env.borrow_mut().get("z"), None);
}

#[test]
fn test_eval_while_scope() {
    let input = r#"
        let i = 1;
        while i < 1000 {
            let x = 1;
            i = i + 1;
        }
    "#;

    let mut evaluator = Evaluator::default();
    evaluator.eval(input);

    assert_eq!(evaluator.env.borrow_mut().get("i"), Some(Object::I32(1000)));
    assert_eq!(evaluator.env.borrow_mut().get("x"), None);
}

#[test]
fn test_eval_let() {
    let tests = vec![
        ("let x = 7; x;", Some(Object::I32(7))),
        ("let x = 2 * 3; x;", Some(Object::I32(6))),
        ("let x = 7; let y = 10; x;", Some(Object::I32(7))),
        ("let x = 7; let y = 10; y;", Some(Object::I32(10))),
        (
            "let x = 7; let y = 10; let z: i32 = x + y + 3; z;",
            Some(Object::I32(20)),
        ),
    ];

    for (expr, expected) in tests {
        assert_eq!(expected, Evaluator::default().eval(expr));
    }
}

#[test]
fn test_eval_let_scope() {
    let input = "let x = 1; if true { x = x + 1; }; x;";
    assert_matches!(Evaluator::default().eval(input), Some(Object::I32(2)));

    let input = "let x = 1; fn foo() -> i32 { x = x + 1; }; foo(); x;";
    assert_matches!(Evaluator::default().eval(input), Some(Object::I32(2)));
}

#[test]
fn test_eval_assign() {
    let tests = vec![
        ("let x = 1; x = 2; x;", Some(Object::I32(2))),
        ("let x = 1; x = x + x + x; x;", Some(Object::I32(3))),
        ("let x = 1; x += 1; x;", Some(Object::I32(2))),
        ("let x = 4; x -= 1; x;", Some(Object::I32(3))),
        ("let x = 2; x *= 3; x;", Some(Object::I32(6))),
        ("let x = 8; x /= 2; x;", Some(Object::I32(4))),
        ("let x = 10; x %= 3; x;", Some(Object::I32(1))),
    ];

    for (expr, expected) in tests {
        assert_eq!(expected, Evaluator::default().eval(expr));
    }
}

#[test]
fn test_eval_assign_invalid() {
    let tests = vec![
        "x = 1;",
        "fn x() -> i32 {}; x = 1;",
        "fn x() -> i32 { x = 1 }; x();",
        "fn x() -> i32 { y = 1 }; x();",
    ];
    for input in tests {
        assert_matches!(Evaluator::default().eval(input), Some(Object::Error { .. }));
    }
}

#[test]
fn test_eval_function() {
    let tests = vec![(
        r#"fn add(x: i32, y: i32) -> i32 {
            if x > y {
                return x - y;
            } else {
                return x + y;
            }
        }"#,
        Some(Object::Fn {
            name: String::from("add"),
            args: vec![Var::new("x", I32), Var::new("y", I32)],
            body: vec![Stmt::Expr(Expr::If {
                cond: Box::new(Expr::Infix(
                    Infix::GT,
                    Box::new(Expr::Ident(Ident::from("x"), ((2, 4), (2, 4)))),
                    Box::new(Expr::Ident(Ident::from("y"), ((2, 8), (2, 8)))),
                    ((2, 4), (2, 8)),
                )),
                then: vec![Stmt::Return(Expr::Infix(
                    Infix::Sub,
                    Box::new(Expr::Ident(Ident::from("x"), ((3, 8), (3, 8)))),
                    Box::new(Expr::Ident(Ident::from("y"), ((3, 12), (3, 12)))),
                    ((3, 8), (3, 12)),
                ))],
                elifs: vec![],
                alt: Some(vec![Stmt::Return(Expr::Infix(
                    Infix::Add,
                    Box::new(Expr::Ident(Ident::from("x"), ((5, 8), (5, 8)))),
                    Box::new(Expr::Ident(Ident::from("y"), ((5, 12), (5, 12)))),
                    ((5, 8), (5, 12)),
                ))]),
                span: ((2, 1), (6, 1)),
            })],
            ret_t: I32,
        }),
    )];
    let mut evaluator = Evaluator::default();

    for (expr, expected) in tests {
        assert_eq!(expected, evaluator.eval(expr));
    }
}

#[bench]
fn bench_eval_math(b: &mut Bencher) {
    let mut evaluator = Evaluator::default();
    let test =
        "(((9876 * 5432) // 123 + (8765 % 34) * (4321 // 2)) * 1987) % 567 + (3456 * 7890) // 234";

    b.iter(|| evaluator.eval(test))
}
