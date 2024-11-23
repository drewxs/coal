use super::{Evaluator, Object, FALSE, TRUE};

#[test]
fn test_eval_int_expressions() {
    let tests = vec![
        ("5", Object::Int(5)),
        ("10", Object::Int(10)),
        ("-5", Object::Int(-5)),
        ("-10", Object::Int(-10)),
    ];

    let mut evaluator = Evaluator::default();

    for (expr, expected) in tests {
        let actual = evaluator.eval(expr).unwrap();
        assert_eq!(expected, actual);
    }
}

#[test]
fn test_eval_not_expressions() {
    let tests = vec![
        ("!true", FALSE),
        ("!!true", TRUE),
        ("!false", TRUE),
        ("!!false", FALSE),
        ("!5", FALSE),
        ("!!5", TRUE),
    ];

    let mut evaluator = Evaluator::default();

    for (expr, expected) in tests {
        let actual = evaluator.eval(expr).unwrap();
        assert_eq!(expected, actual);
    }
}

#[test]
fn test_eval_str_interpolation() {
    let tests = vec![
        (
            r#""not {!false} or {!!false}""#,
            Object::Str(String::from("not true or false")),
        ),
        (
            r#""(1 + \"{-false})""#,
            Object::Str(String::from(r#"(1 + "0)"#)),
        ),
    ];

    let mut evaluator = Evaluator::default();

    for (expr, expected) in tests {
        let actual = evaluator.eval(expr).unwrap();
        assert_eq!(expected, actual);
    }
}

#[test]
fn test_eval_infix_expressions() {
    let tests = vec![
        ("(7 + 2 * 3 / 2) % 3", Object::Float(1.0)),
        ("1 + 2 * 3 + 4 / 5", Object::Float(7.8)),
        ("(1 + 2 * 3 + 4 / 5) * 2 + -10", Object::Float(5.6)),
        ("8 // 3", Object::Int(2)),
        ("10.4 // 2", Object::Float(5.0)),
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
        (
            r#""a" - "b""#,
            Object::Error(String::from("unsupported operation: str - str")),
        ),
        (
            r#""a" / 3.14"#,
            Object::Error(String::from("unsupported operation: str / float")),
        ),
    ];

    let mut evaluator = Evaluator::default();

    for (expr, expected) in tests {
        let actual = evaluator.eval(expr).unwrap();
        assert_eq!(expected, actual);
    }
}
