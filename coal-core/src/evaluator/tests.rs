use std::assert_matches::assert_matches;

use test::Bencher;

use crate::{Expr, Ident, Infix, Stmt, Type, Var};

use super::{Evaluator, Object, FALSE, TRUE};

#[test]
fn test_eval_literal() {
    let tests = vec![
        ("5", Some(Object::Int(5))),
        ("3.14", Some(Object::Float(3.14))),
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
        ("-5", Some(Object::Int(-5))),
        ("-10", Some(Object::Int(-10))),
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
        ("(7 + 2 * 3 / 2) % 3", Some(Object::Float(1.0))),
        ("1 + 2 * 3 + 4 / 5", Some(Object::Float(7.8))),
        ("(1 + 2 * 3 + 4 / 5) * 2 + -10", Some(Object::Float(5.6))),
        ("8 // 3", Some(Object::Int(2))),
        ("10.4 // 2", Some(Object::Float(5.0))),
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
        (
            r#""a" - "b""#,
            Some(Object::Error {
                message: String::from("unsupported operation: str - str"),
                span: ((1, 1), (1, 9)),
            }),
        ),
        (
            r#""a" / 3.14"#,
            Some(Object::Error {
                message: String::from("unsupported operation: str / float"),
                span: ((1, 1), (1, 10)),
            }),
        ),
    ];

    let mut evaluator = Evaluator::default();

    for (expr, expected) in tests {
        assert_eq!(expected, evaluator.eval(expr));
    }
}

#[test]
fn test_eval_if() {
    let tests = vec![
        ("if true { 10 }", Some(Object::Int(10))),
        ("if false { 10 }", None),
        ("if 1 { 10 }", Some(Object::Int(10))),
        ("if 1 < 2 { 10 }", Some(Object::Int(10))),
        ("if 1 > 2 { 10 }", None),
        ("if 1 > 2 { 10 } else { 20 }", Some(Object::Int(20))),
        ("if 1 < 2 { 10 } else { 20 }", Some(Object::Int(10))),
        (
            r#"if 1 < 2 {
                let x = 999;
                return 10;
            } else {
                return 20;
            }"#,
            Some(Object::Int(10)),
        ),
        (
            r#"if 1 > 2 {
                return 1;
            } else {
                return 2;
                return 3;
            }"#,
            Some(Object::Int(2)),
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
            Some(Object::Int(1)),
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
            Some(Object::Int(2)),
        ),
    ];

    let mut evaluator = Evaluator::default();

    for (expr, expected) in tests {
        assert_eq!(expected, evaluator.eval(expr));
    }
}

#[test]
fn test_eval_let() {
    let tests = vec![
        ("let x = 7; x;", Some(Object::Int(7))),
        ("let x = 2 * 3; x;", Some(Object::Int(6))),
        ("let x = 7; let y = 10; x;", Some(Object::Int(7))),
        ("let x = 7; let y = 10; y;", Some(Object::Int(10))),
        (
            "let x = 7; let y = 10; let z: int = x + y + 3; z;",
            Some(Object::Int(20)),
        ),
    ];

    for (expr, expected) in tests {
        assert_eq!(expected, Evaluator::default().eval(expr));
    }
}

#[test]
fn test_eval_let_existing() {
    let input = "fn x() -> int {}; let x = 1;";
    assert_matches!(Evaluator::default().eval(input), Some(Object::Error { .. }));
}

#[test]
fn test_eval_let_scope() {
    let input = "fn x() -> int {}; let x = 1;";
    assert_matches!(Evaluator::default().eval(input), Some(Object::Error { .. }));
}

#[test]
fn test_eval_assign() {
    let tests = vec![
        ("let x = 1; x = 2; x;", Some(Object::Int(2))),
        ("let x = 1; x = x + x + x; x;", Some(Object::Int(3))),
    ];

    for (expr, expected) in tests {
        assert_eq!(expected, Evaluator::default().eval(expr));
    }
}

#[test]
fn test_eval_assign_invalid() {
    let tests = vec![
        "x = 1",
        r#"let x = 1; x = "foo";"#,
        "fn x() -> int {}; x = 1;",
        "fn x() -> int {}; let x = 1;",
    ];
    for input in tests {
        assert_matches!(Evaluator::default().eval(input), Some(Object::Error { .. }));
    }
}

#[test]
fn test_eval_function() {
    let tests = vec![(
        r#"fn add(x: int, y: int) -> int {
            if x > y {
                return x - y;
            } else {
                return x + y;
            }
        }"#,
        Some(Object::Fn {
            name: String::from("add"),
            args: vec![Var::new("x", Type::Int), Var::new("y", Type::Int)],
            body: vec![Stmt::Expr(Expr::If {
                cond: Box::new(Expr::Infix(
                    Infix::GT,
                    Box::new(Expr::Ident(Ident::from("x"), ((2, 4), (2, 4)))),
                    Box::new(Expr::Ident(Ident::from("y"), ((2, 8), (2, 8)))),
                    ((2, 4), (2, 8)),
                )),
                then: vec![Stmt::Return(Expr::Infix(
                    Infix::Minus,
                    Box::new(Expr::Ident(Ident::from("x"), ((3, 8), (3, 8)))),
                    Box::new(Expr::Ident(Ident::from("y"), ((3, 12), (3, 12)))),
                    ((3, 8), (3, 12)),
                ))],
                elifs: vec![],
                alt: Some(vec![Stmt::Return(Expr::Infix(
                    Infix::Plus,
                    Box::new(Expr::Ident(Ident::from("x"), ((5, 8), (5, 8)))),
                    Box::new(Expr::Ident(Ident::from("y"), ((5, 12), (5, 12)))),
                    ((5, 8), (5, 12)),
                ))]),
                span: ((2, 1), (6, 1)),
            })],
            ret_t: Type::Int,
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
