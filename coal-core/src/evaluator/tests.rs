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
        let actual = evaluator.eval(expr);
        assert_eq!(expected, actual);
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
        let actual = evaluator.eval(expr);
        assert_eq!(expected, actual);
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
        let actual = evaluator.eval(expr);
        assert_eq!(expected, actual);
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
            Some(Object::Error(String::from(
                "unsupported operation: str - str",
            ))),
        ),
        (
            r#""a" / 3.14"#,
            Some(Object::Error(String::from(
                "unsupported operation: str / float",
            ))),
        ),
    ];

    let mut evaluator = Evaluator::default();

    for (expr, expected) in tests {
        let actual = evaluator.eval(expr);
        assert_eq!(expected, actual);
    }
}

#[test]
fn test_eval_if_else_expressions() {
    let tests = vec![
        ("if true { 10 }", Some(Object::Int(10))),
        ("if false { 10 }", None),
        ("if 1 { 10 }", Some(Object::Int(10))),
        ("if 1 < 2 { 10 }", Some(Object::Int(10))),
        ("if 1 > 2 { 10 }", None),
        ("if 1 > 2 { 10 } else { 20 }", Some(Object::Int(20))),
        ("if 1 < 2 { 10 } else { 20 }", Some(Object::Int(10))),
        (
            "if 1 < 2 {
    let x = 999;
    return 10;
} else {
    return 20;
}",
            Some(Object::Int(10)),
        ),
    ];

    let mut evaluator = Evaluator::default();

    for (expr, expected) in tests {
        let actual = evaluator.eval(expr);
        assert_eq!(expected, actual);
    }
}
