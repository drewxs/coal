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
