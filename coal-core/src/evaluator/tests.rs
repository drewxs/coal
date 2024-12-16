use test::Bencher;

use super::{Evaluator, Object, FALSE, TRUE};

#[test]
fn test_eval_int_expressions() {
    let tests = vec![
        ("5", Some(Object::Int(5))),
        ("10", Some(Object::Int(10))),
        ("-5", Some(Object::Int(-5))),
        ("-10", Some(Object::Int(-10))),
    ];

    let mut evaluator = Evaluator::default();

    for (expr, expected) in tests {
        assert_eq!(expected, evaluator.eval(expr));
    }
}

#[test]
fn test_eval_not_expressions() {
    let tests = vec![
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
fn test_eval_infix_expressions() {
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
fn test_eval_if_expressions() {
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
fn test_eval_nested_if_expressions() {
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
fn test_eval_let_statements() {
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
    let mut evaluator = Evaluator::default();

    for (expr, expected) in tests {
        assert_eq!(expected, evaluator.eval(expr));
    }
}

#[bench]
fn bench_eval_math_expression(b: &mut Bencher) {
    let mut evaluator = Evaluator::default();
    let test =
        "(((9876 * 5432) // 123 + (8765 % 34) * (4321 // 2)) * 1987) % 567 + (3456 * 7890) // 234";

    b.iter(|| evaluator.eval(test))
}
