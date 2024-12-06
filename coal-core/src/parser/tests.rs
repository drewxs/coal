use super::*;

#[test]
fn test_parse_let_statements() {
    let tests = vec![
        (
            "let x: int = 5;",
            Stmt::Let(
                Ident(String::from("x")),
                Type::Int,
                Expr::Literal(Literal::Int(5), ((1, 14), (1, 15))),
            ),
        ),
        (
            "let y: int = 10;",
            Stmt::Let(
                Ident(String::from("y")),
                Type::Int,
                Expr::Literal(Literal::Int(10), ((1, 14), (1, 16))),
            ),
        ),
        (
            "let z: int = 99999;",
            Stmt::Let(
                Ident(String::from("z")),
                Type::Int,
                Expr::Literal(Literal::Int(99999), ((1, 14), (1, 19))),
            ),
        ),
        (
            "let foo: Foo = 0;",
            Stmt::Let(
                Ident(String::from("foo")),
                Type::UserDefined(String::from("Foo")),
                Expr::Literal(Literal::Int(0), ((1, 16), (1, 17))),
            ),
        ),
    ];

    for (input, expected) in tests {
        assert_eq!(expected, Program::parse(input).statements[0]);
    }
}

#[test]
fn test_parse_let_statements_inference() {
    let input = r#"
        let x = 5;
        let y = 5.0;
        let z = "hello";"#;
    let program = Program::parse(input);
    let expected = vec![
        Stmt::Let(
            Ident(String::from("x")),
            Type::Int,
            Expr::Literal(Literal::Int(5), ((1, 9), (1, 10))),
        ),
        Stmt::Let(
            Ident(String::from("y")),
            Type::Float,
            Expr::Literal(Literal::Float(5.0), ((2, 10), (2, 13))),
        ),
        Stmt::Let(
            Ident(String::from("z")),
            Type::Str,
            Expr::Literal(Literal::Str(String::from("hello")), ((3, 10), (3, 17))),
        ),
    ];

    assert_eq!(expected, program.statements);
}

#[test]
fn test_parse_return_statements() {
    let tests = vec!["return 7;", "return 100;", "return 999999;"];

    for input in tests {
        let stmt = &Program::parse(input).statements[0];
        if !matches!(stmt, Stmt::Return(_)) {
            panic!("[{input}] expected=Stmt::Return, got={stmt:?}");
        }
    }
}

#[test]
fn test_parse_identifier_expressions() {
    let input = "foo; bar; foobar;";
    let program = Program::parse(input);
    let expected = vec![
        Stmt::Expr(Expr::Ident(Ident::from("foo"), ((1, 1), (1, 4)))),
        Stmt::Expr(Expr::Ident(Ident::from("bar"), ((1, 6), (1, 9)))),
        Stmt::Expr(Expr::Ident(Ident::from("foobar"), ((1, 11), (1, 17)))),
    ];

    assert_eq!(expected, program.statements);
}

#[test]
fn test_parse_literal_expressions() {
    let input = r#"5; 10.0; false; "foo";"#;
    let program = Program::parse(input);
    let expected = vec![
        Stmt::Expr(Expr::Literal(Literal::Int(5), ((1, 1), (1, 2)))),
        Stmt::Expr(Expr::Literal(Literal::Float(10.0), ((1, 4), (1, 8)))),
        Stmt::Expr(Expr::Literal(Literal::Bool(false), ((1, 10), (1, 15)))),
        Stmt::Expr(Expr::Literal(
            Literal::Str(String::from("foo")),
            ((1, 17), (1, 22)),
        )),
    ];

    assert_eq!(expected, program.statements);
}

#[test]
fn test_parse_prefix_expressions() {
    let input = "!5; -5; !true; !false;";
    let program = Program::parse(input);
    let expected = vec![
        Stmt::Expr(Expr::Prefix(
            Prefix::Not,
            Box::new(Expr::Literal(Literal::Int(5), ((1, 2), (1, 3)))),
            ((1, 1), (1, 3)),
        )),
        Stmt::Expr(Expr::Prefix(
            Prefix::Minus,
            Box::new(Expr::Literal(Literal::Int(5), ((1, 6), (1, 7)))),
            ((1, 5), (1, 7)),
        )),
        Stmt::Expr(Expr::Prefix(
            Prefix::Not,
            Box::new(Expr::Literal(Literal::Bool(true), ((1, 10), (1, 14)))),
            ((1, 9), (1, 14)),
        )),
        Stmt::Expr(Expr::Prefix(
            Prefix::Not,
            Box::new(Expr::Literal(Literal::Bool(false), ((1, 17), (1, 22)))),
            ((1, 16), (1, 22)),
        )),
    ];

    assert_eq!(expected, program.statements);
}

#[test]
fn test_parse_infix_expressions() {
    let tests = vec![
        (
            "3 + 2",
            Stmt::Expr(Expr::Infix(
                Infix::Plus,
                Box::new(Expr::Literal(Literal::Int(3), ((1, 1), (1, 2)))),
                Box::new(Expr::Literal(Literal::Int(2), ((1, 5), (1, 6)))),
                ((1, 1), (1, 6)),
            )),
        ),
        (
            "5 - 2",
            Stmt::Expr(Expr::Infix(
                Infix::Minus,
                Box::new(Expr::Literal(Literal::Int(5), ((1, 1), (1, 2)))),
                Box::new(Expr::Literal(Literal::Int(2), ((1, 5), (1, 6)))),
                ((1, 1), (1, 6)),
            )),
        ),
        (
            "3 * 2",
            Stmt::Expr(Expr::Infix(
                Infix::Mul,
                Box::new(Expr::Literal(Literal::Int(3), ((1, 1), (1, 2)))),
                Box::new(Expr::Literal(Literal::Int(2), ((1, 5), (1, 6)))),
                ((1, 1), (1, 6)),
            )),
        ),
        (
            "6 / 2",
            Stmt::Expr(Expr::Infix(
                Infix::Div,
                Box::new(Expr::Literal(Literal::Int(6), ((1, 1), (1, 2)))),
                Box::new(Expr::Literal(Literal::Int(2), ((1, 5), (1, 6)))),
                ((1, 1), (1, 6)),
            )),
        ),
        (
            "7 % 2",
            Stmt::Expr(Expr::Infix(
                Infix::Mod,
                Box::new(Expr::Literal(Literal::Int(7), ((1, 1), (1, 2)))),
                Box::new(Expr::Literal(Literal::Int(2), ((1, 5), (1, 6)))),
                ((1, 1), (1, 6)),
            )),
        ),
        (
            "3 > 2",
            Stmt::Expr(Expr::Infix(
                Infix::GT,
                Box::new(Expr::Literal(Literal::Int(3), ((1, 1), (1, 2)))),
                Box::new(Expr::Literal(Literal::Int(2), ((1, 5), (1, 6)))),
                ((1, 1), (1, 6)),
            )),
        ),
        (
            "3 < 2",
            Stmt::Expr(Expr::Infix(
                Infix::LT,
                Box::new(Expr::Literal(Literal::Int(3), ((1, 1), (1, 2)))),
                Box::new(Expr::Literal(Literal::Int(2), ((1, 5), (1, 6)))),
                ((1, 1), (1, 6)),
            )),
        ),
        (
            "4 >= 2",
            Stmt::Expr(Expr::Infix(
                Infix::GTE,
                Box::new(Expr::Literal(Literal::Int(4), ((1, 1), (1, 2)))),
                Box::new(Expr::Literal(Literal::Int(2), ((1, 6), (1, 7)))),
                ((1, 1), (1, 7)),
            )),
        ),
        (
            "4 <= 2",
            Stmt::Expr(Expr::Infix(
                Infix::LTE,
                Box::new(Expr::Literal(Literal::Int(4), ((1, 1), (1, 2)))),
                Box::new(Expr::Literal(Literal::Int(2), ((1, 6), (1, 7)))),
                ((1, 1), (1, 7)),
            )),
        ),
        (
            "4 == 4",
            Stmt::Expr(Expr::Infix(
                Infix::EQ,
                Box::new(Expr::Literal(Literal::Int(4), ((1, 1), (1, 2)))),
                Box::new(Expr::Literal(Literal::Int(4), ((1, 6), (1, 7)))),
                ((1, 1), (1, 7)),
            )),
        ),
        (
            "4 != 4",
            Stmt::Expr(Expr::Infix(
                Infix::NEQ,
                Box::new(Expr::Literal(Literal::Int(4), ((1, 1), (1, 2)))),
                Box::new(Expr::Literal(Literal::Int(4), ((1, 6), (1, 7)))),
                ((1, 1), (1, 7)),
            )),
        ),
        (
            "true == true",
            Stmt::Expr(Expr::Infix(
                Infix::EQ,
                Box::new(Expr::Literal(Literal::Bool(true), ((1, 1), (1, 5)))),
                Box::new(Expr::Literal(Literal::Bool(true), ((1, 9), (1, 13)))),
                ((1, 1), (1, 13)),
            )),
        ),
        (
            "false == false",
            Stmt::Expr(Expr::Infix(
                Infix::EQ,
                Box::new(Expr::Literal(Literal::Bool(false), ((1, 1), (1, 6)))),
                Box::new(Expr::Literal(Literal::Bool(false), ((1, 10), (1, 15)))),
                ((1, 1), (1, 15)),
            )),
        ),
        (
            "true != false",
            Stmt::Expr(Expr::Infix(
                Infix::NEQ,
                Box::new(Expr::Literal(Literal::Bool(true), ((1, 1), (1, 5)))),
                Box::new(Expr::Literal(Literal::Bool(false), ((1, 9), (1, 14)))),
                ((1, 1), (1, 14)),
            )),
        ),
    ];

    for (input, expected) in tests {
        assert_eq!(expected, Program::parse(input).statements[0]);
    }
}

#[test]
fn test_parse_operator_precedence() {
    let tests = vec![
        (
            "-a * b",
            Stmt::Expr(Expr::Infix(
                Infix::Mul,
                Box::new(Expr::Prefix(
                    Prefix::Minus,
                    Box::new(Expr::Ident(Ident::from("a"), ((1, 2), (1, 3)))),
                    ((1, 1), (1, 3)),
                )),
                Box::new(Expr::Ident(Ident::from("b"), ((1, 6), (1, 7)))),
                ((1, 1), (1, 7)),
            )),
        ),
        (
            "!-a",
            Stmt::Expr(Expr::Prefix(
                Prefix::Not,
                Box::new(Expr::Prefix(
                    Prefix::Minus,
                    Box::new(Expr::Ident(Ident::from("a"), ((1, 3), (1, 4)))),
                    ((1, 2), (1, 4)),
                )),
                ((1, 1), (1, 4)),
            )),
        ),
        (
            "a + b + c",
            Stmt::Expr(Expr::Infix(
                Infix::Plus,
                Box::new(Expr::Infix(
                    Infix::Plus,
                    Box::new(Expr::Ident(Ident::from("a"), ((1, 1), (1, 2)))),
                    Box::new(Expr::Ident(Ident::from("b"), ((1, 5), (1, 6)))),
                    ((1, 1), (1, 6)),
                )),
                Box::new(Expr::Ident(Ident::from("c"), ((1, 9), (1, 10)))),
                ((1, 1), (1, 10)),
            )),
        ),
        (
            "a + b - c",
            Stmt::Expr(Expr::Infix(
                Infix::Minus,
                Box::new(Expr::Infix(
                    Infix::Plus,
                    Box::new(Expr::Ident(Ident::from("a"), ((1, 1), (1, 2)))),
                    Box::new(Expr::Ident(Ident::from("b"), ((1, 5), (1, 6)))),
                    ((1, 1), (1, 6)),
                )),
                Box::new(Expr::Ident(Ident::from("c"), ((1, 9), (1, 10)))),
                ((1, 1), (1, 10)),
            )),
        ),
        (
            "a * b * c",
            Stmt::Expr(Expr::Infix(
                Infix::Mul,
                Box::new(Expr::Infix(
                    Infix::Mul,
                    Box::new(Expr::Ident(Ident::from("a"), ((1, 1), (1, 2)))),
                    Box::new(Expr::Ident(Ident::from("b"), ((1, 5), (1, 6)))),
                    ((1, 1), (1, 6)),
                )),
                Box::new(Expr::Ident(Ident::from("c"), ((1, 9), (1, 10)))),
                ((1, 1), (1, 10)),
            )),
        ),
        (
            "a * b / c",
            Stmt::Expr(Expr::Infix(
                Infix::Div,
                Box::new(Expr::Infix(
                    Infix::Mul,
                    Box::new(Expr::Ident(Ident::from("a"), ((1, 1), (1, 2)))),
                    Box::new(Expr::Ident(Ident::from("b"), ((1, 5), (1, 6)))),
                    ((1, 1), (1, 6)),
                )),
                Box::new(Expr::Ident(Ident::from("c"), ((1, 9), (1, 10)))),
                ((1, 1), (1, 10)),
            )),
        ),
        (
            "a + b / c",
            Stmt::Expr(Expr::Infix(
                Infix::Plus,
                Box::new(Expr::Ident(Ident::from("a"), ((1, 1), (1, 2)))),
                Box::new(Expr::Infix(
                    Infix::Div,
                    Box::new(Expr::Ident(Ident::from("b"), ((1, 5), (1, 6)))),
                    Box::new(Expr::Ident(Ident::from("c"), ((1, 9), (1, 10)))),
                    ((1, 5), (1, 10)),
                )),
                ((1, 1), (1, 10)),
            )),
        ),
        (
            "a + b * c + d / e - f",
            Stmt::Expr(Expr::Infix(
                Infix::Minus,
                Box::new(Expr::Infix(
                    Infix::Plus,
                    Box::new(Expr::Infix(
                        Infix::Plus,
                        Box::new(Expr::Ident(Ident::from("a"), ((1, 1), (1, 2)))),
                        Box::new(Expr::Infix(
                            Infix::Mul,
                            Box::new(Expr::Ident(Ident::from("b"), ((1, 5), (1, 6)))),
                            Box::new(Expr::Ident(Ident::from("c"), ((1, 9), (1, 10)))),
                            ((1, 5), (1, 10)),
                        )),
                        ((1, 1), (1, 10)),
                    )),
                    Box::new(Expr::Infix(
                        Infix::Div,
                        Box::new(Expr::Ident(Ident::from("d"), ((1, 13), (1, 14)))),
                        Box::new(Expr::Ident(Ident::from("e"), ((1, 17), (1, 18)))),
                        ((1, 13), (1, 18)),
                    )),
                    ((1, 1), (1, 18)),
                )),
                Box::new(Expr::Ident(Ident::from("f"), ((1, 21), (1, 22)))),
                ((1, 1), (1, 22)),
            )),
        ),
        (
            "5 > 4 == 3 < 4",
            Stmt::Expr(Expr::Infix(
                Infix::EQ,
                Box::new(Expr::Infix(
                    Infix::GT,
                    Box::new(Expr::Literal(Literal::Int(5), ((1, 1), (1, 2)))),
                    Box::new(Expr::Literal(Literal::Int(4), ((1, 5), (1, 6)))),
                    ((1, 1), (1, 6)),
                )),
                Box::new(Expr::Infix(
                    Infix::LT,
                    Box::new(Expr::Literal(Literal::Int(3), ((1, 10), (1, 11)))),
                    Box::new(Expr::Literal(Literal::Int(4), ((1, 14), (1, 15)))),
                    ((1, 10), (1, 15)),
                )),
                ((1, 1), (1, 15)),
            )),
        ),
        (
            "5 < 4 != 3 > 4",
            Stmt::Expr(Expr::Infix(
                Infix::NEQ,
                Box::new(Expr::Infix(
                    Infix::LT,
                    Box::new(Expr::Literal(Literal::Int(5), ((1, 1), (1, 2)))),
                    Box::new(Expr::Literal(Literal::Int(4), ((1, 5), (1, 6)))),
                    ((1, 1), (1, 6)),
                )),
                Box::new(Expr::Infix(
                    Infix::GT,
                    Box::new(Expr::Literal(Literal::Int(3), ((1, 10), (1, 11)))),
                    Box::new(Expr::Literal(Literal::Int(4), ((1, 14), (1, 15)))),
                    ((1, 10), (1, 15)),
                )),
                ((1, 1), (1, 15)),
            )),
        ),
        (
            "3 + 4 * 5 == 3 * 1 + 4 * 5",
            Stmt::Expr(Expr::Infix(
                Infix::EQ,
                Box::new(Expr::Infix(
                    Infix::Plus,
                    Box::new(Expr::Literal(Literal::Int(3), ((1, 1), (1, 2)))),
                    Box::new(Expr::Infix(
                        Infix::Mul,
                        Box::new(Expr::Literal(Literal::Int(4), ((1, 5), (1, 6)))),
                        Box::new(Expr::Literal(Literal::Int(5), ((1, 9), (1, 10)))),
                        ((1, 5), (1, 10)),
                    )),
                    ((1, 1), (1, 10)),
                )),
                Box::new(Expr::Infix(
                    Infix::Plus,
                    Box::new(Expr::Infix(
                        Infix::Mul,
                        Box::new(Expr::Literal(Literal::Int(3), ((1, 14), (1, 15)))),
                        Box::new(Expr::Literal(Literal::Int(1), ((1, 18), (1, 19)))),
                        ((1, 14), (1, 19)),
                    )),
                    Box::new(Expr::Infix(
                        Infix::Mul,
                        Box::new(Expr::Literal(Literal::Int(4), ((1, 22), (1, 23)))),
                        Box::new(Expr::Literal(Literal::Int(5), ((1, 26), (1, 27)))),
                        ((1, 22), (1, 27)),
                    )),
                    ((1, 14), (1, 27)),
                )),
                ((1, 1), (1, 27)),
            )),
        ),
        (
            "true",
            Stmt::Expr(Expr::Literal(Literal::Bool(true), ((1, 1), (1, 5)))),
        ),
        (
            "false",
            Stmt::Expr(Expr::Literal(Literal::Bool(false), ((1, 1), (1, 6)))),
        ),
        (
            "1 < 2 == true",
            Stmt::Expr(Expr::Infix(
                Infix::EQ,
                Box::new(Expr::Infix(
                    Infix::LT,
                    Box::new(Expr::Literal(Literal::Int(1), ((1, 1), (1, 2)))),
                    Box::new(Expr::Literal(Literal::Int(2), ((1, 5), (1, 6)))),
                    ((1, 1), (1, 6)),
                )),
                Box::new(Expr::Literal(Literal::Bool(true), ((1, 10), (1, 14)))),
                ((1, 1), (1, 14)),
            )),
        ),
        (
            "1 > 2 == false",
            Stmt::Expr(Expr::Infix(
                Infix::EQ,
                Box::new(Expr::Infix(
                    Infix::GT,
                    Box::new(Expr::Literal(Literal::Int(1), ((1, 1), (1, 2)))),
                    Box::new(Expr::Literal(Literal::Int(2), ((1, 5), (1, 6)))),
                    ((1, 1), (1, 6)),
                )),
                Box::new(Expr::Literal(Literal::Bool(false), ((1, 10), (1, 15)))),
                ((1, 1), (1, 15)),
            )),
        ),
        (
            "1 + (2 + 3) + 4",
            Stmt::Expr(Expr::Infix(
                Infix::Plus,
                Box::new(Expr::Infix(
                    Infix::Plus,
                    Box::new(Expr::Literal(Literal::Int(1), ((1, 1), (1, 2)))),
                    Box::new(Expr::Infix(
                        Infix::Plus,
                        Box::new(Expr::Literal(Literal::Int(2), ((1, 6), (1, 7)))),
                        Box::new(Expr::Literal(Literal::Int(3), ((1, 10), (1, 11)))),
                        ((1, 6), (1, 11)),
                    )),
                    ((1, 1), (1, 11)),
                )),
                Box::new(Expr::Literal(Literal::Int(4), ((1, 15), (1, 16)))),
                ((1, 1), (1, 16)),
            )),
        ),
        (
            "(1 + 1) * 2",
            Stmt::Expr(Expr::Infix(
                Infix::Mul,
                Box::new(Expr::Infix(
                    Infix::Plus,
                    Box::new(Expr::Literal(Literal::Int(1), ((1, 2), (1, 3)))),
                    Box::new(Expr::Literal(Literal::Int(1), ((1, 6), (1, 7)))),
                    ((1, 2), (1, 7)),
                )),
                Box::new(Expr::Literal(Literal::Int(2), ((1, 11), (1, 12)))),
                ((1, 2), (1, 12)),
            )),
        ),
        (
            "1 / (2 + 2)",
            Stmt::Expr(Expr::Infix(
                Infix::Div,
                Box::new(Expr::Literal(Literal::Int(1), ((1, 1), (1, 2)))),
                Box::new(Expr::Infix(
                    Infix::Plus,
                    Box::new(Expr::Literal(Literal::Int(2), ((1, 6), (1, 7)))),
                    Box::new(Expr::Literal(Literal::Int(2), ((1, 10), (1, 11)))),
                    ((1, 6), (1, 11)),
                )),
                ((1, 1), (1, 11)),
            )),
        ),
        (
            "-(1 + 2)",
            Stmt::Expr(Expr::Prefix(
                Prefix::Minus,
                Box::new(Expr::Infix(
                    Infix::Plus,
                    Box::new(Expr::Literal(Literal::Int(1), ((1, 3), (1, 4)))),
                    Box::new(Expr::Literal(Literal::Int(2), ((1, 7), (1, 8)))),
                    ((1, 3), (1, 8)),
                )),
                ((1, 1), (1, 8)),
            )),
        ),
        (
            "!(true == true)",
            Stmt::Expr(Expr::Prefix(
                Prefix::Not,
                Box::new(Expr::Infix(
                    Infix::EQ,
                    Box::new(Expr::Literal(Literal::Bool(true), ((1, 3), (1, 7)))),
                    Box::new(Expr::Literal(Literal::Bool(true), ((1, 11), (1, 15)))),
                    ((1, 3), (1, 15)),
                )),
                ((1, 1), (1, 15)),
            )),
        ),
    ];

    for (input, expected) in tests {
        dbg!(input);
        assert_eq!(expected, Program::parse(input).statements[0]);
    }
}

#[test]
fn test_parse_if_expression() {
    let input = "if x < y { return x }";
    let program = Program::parse(input);
    let expected = Stmt::Expr(Expr::If {
        cond: Box::new(Expr::Infix(
            Infix::LT,
            Box::new(Expr::Ident(Ident::from("x"), ((1, 4), (1, 5)))),
            Box::new(Expr::Ident(Ident::from("y"), ((1, 8), (1, 9)))),
            ((1, 4), (1, 9)),
        )),
        then: vec![Stmt::Return(Expr::Ident(
            Ident::from("x"),
            ((1, 19), (1, 20)),
        ))],
        elifs: vec![],
        alt: None,
        span: ((1, 1), (1, 22)),
    });

    assert_eq!(expected, program.statements[0]);
}

#[test]
fn test_parse_nested_if_expression() {
    let input = r#"
        if x < y {
            if x > 1 {
                return x;
            }
            return y;
        } else {
            return z;
        }"#;
    let program = Program::parse(input);
    let expected = Stmt::Expr(Expr::If {
        cond: Box::new(Expr::Infix(
            Infix::LT,
            Box::new(Expr::Ident(Ident::from("x"), ((1, 4), (1, 5)))),
            Box::new(Expr::Ident(Ident::from("y"), ((1, 8), (1, 9)))),
            ((1, 4), (1, 9)),
        )),
        then: vec![
            Stmt::Expr(Expr::If {
                cond: Box::new(Expr::Infix(
                    Infix::GT,
                    Box::new(Expr::Ident(Ident::from("x"), ((2, 5), (2, 6)))),
                    Box::new(Expr::Literal(Literal::Int(1), ((2, 9), (2, 10)))),
                    ((2, 5), (2, 10)),
                )),
                then: vec![Stmt::Return(Expr::Ident(
                    Ident::from("x"),
                    ((3, 9), (3, 10)),
                ))],
                elifs: vec![],
                alt: None,
                span: ((2, 2), (5, 1)),
            }),
            Stmt::Return(Expr::Ident(Ident::from("y"), ((5, 9), (5, 10)))),
        ],
        elifs: vec![],
        alt: Some(vec![Stmt::Return(Expr::Ident(
            Ident::from("z"),
            ((7, 9), (7, 10)),
        ))]),
        span: ((1, 1), (8, 3)),
    });

    assert_eq!(expected, program.statements[0]);
}

#[test]
fn test_parse_elif_expression() {
    let input = r#"
        if x < y {
            return x;
        } elif x > y {
            return y;
        } elif x > 1 {
            return 1;
        } else {
            return z;
        }"#;
    let expected = Stmt::Expr(Expr::If {
        cond: Box::new(Expr::Infix(
            Infix::LT,
            Box::new(Expr::Ident(Ident::from("x"), ((1, 4), (1, 5)))),
            Box::new(Expr::Ident(Ident::from("y"), ((1, 8), (1, 9)))),
            ((1, 4), (1, 9)),
        )),
        then: vec![Stmt::Return(Expr::Ident(
            Ident::from("x"),
            ((2, 9), (2, 10)),
        ))],
        elifs: vec![
            IfExpr {
                cond: Box::new(Expr::Infix(
                    Infix::GT,
                    Box::new(Expr::Ident(Ident::from("x"), ((3, 9), (3, 10)))),
                    Box::new(Expr::Ident(Ident::from("y"), ((3, 13), (3, 14)))),
                    ((3, 9), (3, 14)),
                )),
                then: vec![Stmt::Return(Expr::Ident(
                    Ident::from("y"),
                    ((4, 9), (4, 10)),
                ))],
            },
            IfExpr {
                cond: Box::new(Expr::Infix(
                    Infix::GT,
                    Box::new(Expr::Ident(Ident::from("x"), ((5, 9), (5, 10)))),
                    Box::new(Expr::Literal(Literal::Int(1), ((5, 13), (5, 14)))),
                    ((5, 9), (5, 14)),
                )),
                then: vec![Stmt::Return(Expr::Literal(
                    Literal::Int(1),
                    ((6, 9), (6, 10)),
                ))],
            },
        ],
        alt: Some(vec![Stmt::Return(Expr::Ident(
            Ident::from("z"),
            ((8, 9), (8, 10)),
        ))]),
        span: ((1, 1), (9, 3)),
    });
    let program = Program::parse(input);

    assert_eq!(expected, program.statements[0]);
}

#[test]
fn test_parse_function_expressions() {
    let tests = [
        (
            r#"fn foo() -> int {
                   return 0;
               }"#,
            Stmt::Expr(Expr::Fn {
                name: String::from("foo"),
                args: vec![],
                ret_t: Type::Int,
                body: vec![Stmt::Return(Expr::Literal(
                    Literal::Int(0),
                    ((2, 9), (2, 10)),
                ))],
                span: ((1, 1), (3, 3)),
            }),
        ),
        (
            r#"fn add(x: int, y: int) -> int {
                   return x + y;
               }"#,
            Stmt::Expr(Expr::Fn {
                name: String::from("add"),
                args: vec![
                    Var::new(String::from("x"), Type::Int),
                    Var::new(String::from("y"), Type::Int),
                ],
                ret_t: Type::Int,
                body: vec![Stmt::Return(Expr::Infix(
                    Infix::Plus,
                    Box::new(Expr::Ident(Ident::from("x"), ((2, 9), (2, 10)))),
                    Box::new(Expr::Ident(Ident::from("y"), ((2, 13), (2, 14)))),
                    ((2, 9), (2, 14)),
                ))],
                span: ((1, 1), (3, 3)),
            }),
        ),
    ];

    for (input, expected) in tests {
        dbg!(input);
        assert_eq!(expected, Program::parse(input).statements[0]);
    }
}

#[test]
fn test_parse_call_expression() {
    let input = "add(1, 2 * 3, 4 + 5);";
    let program = Program::from(input);
    let expected = Stmt::Expr(Expr::Call {
        func: Box::new(Expr::Ident(Ident::from("add"), ((1, 1), (1, 4)))),
        args: vec![
            Expr::Literal(Literal::Int(1), ((1, 5), (1, 6))),
            Expr::Infix(
                Infix::Mul,
                Box::new(Expr::Literal(Literal::Int(2), ((1, 8), (1, 9)))),
                Box::new(Expr::Literal(Literal::Int(3), ((1, 12), (1, 13)))),
                ((1, 8), (1, 13)),
            ),
            Expr::Infix(
                Infix::Plus,
                Box::new(Expr::Literal(Literal::Int(4), ((1, 15), (1, 16)))),
                Box::new(Expr::Literal(Literal::Int(5), ((1, 19), (1, 20)))),
                ((1, 15), (1, 20)),
            ),
        ],
        span: ((1, 1), (1, 21)),
    });

    assert_eq!(expected, program.statements[0]);
}
