use super::*;

#[test]
fn test_parse_let_statements() {
    let tests = vec![
        (
            "let x: int = 5;",
            Stmt::Let(
                Ident(String::from("x")),
                Type::Int,
                Expr::Literal(Literal::Int(5), ((1, 14), (1, 14))),
            ),
        ),
        (
            "let y: int = 10;",
            Stmt::Let(
                Ident(String::from("y")),
                Type::Int,
                Expr::Literal(Literal::Int(10), ((1, 14), (1, 15))),
            ),
        ),
        (
            "let z: int = 99999;",
            Stmt::Let(
                Ident(String::from("z")),
                Type::Int,
                Expr::Literal(Literal::Int(99999), ((1, 14), (1, 18))),
            ),
        ),
        (
            "let foo: Foo = 0;",
            Stmt::Let(
                Ident(String::from("foo")),
                Type::UserDefined(String::from("Foo")),
                Expr::Literal(Literal::Int(0), ((1, 16), (1, 16))),
            ),
        ),
    ];

    for (input, expected) in tests {
        let actual = Parser::from(input).parse();
        assert_eq!(expected, actual[0]);
    }
}

#[test]
fn test_parse_let_statements_inference() {
    let input = r#"
        let x = 5;
        let y = 5.0;
        let z = "hello";"#;
    let expected = vec![
        Stmt::Let(
            Ident(String::from("x")),
            Type::Int,
            Expr::Literal(Literal::Int(5), ((1, 9), (1, 9))),
        ),
        Stmt::Let(
            Ident(String::from("y")),
            Type::Float,
            Expr::Literal(Literal::Float(5.0), ((2, 9), (2, 11))),
        ),
        Stmt::Let(
            Ident(String::from("z")),
            Type::Str,
            Expr::Literal(Literal::Str(String::from("hello")), ((3, 9), (3, 15))),
        ),
    ];
    let actual = Parser::from(input).parse();

    assert_eq!(expected, actual);
}

#[test]
fn test_parse_return_statements() {
    let tests = vec!["return 7;", "return 100;", "return 999999;"];

    for input in tests {
        let stmt = &Parser::from(input).parse()[0];
        if !matches!(stmt, Stmt::Return(_)) {
            panic!("[{input}] expected=Stmt::Return, got={stmt:?}");
        }
    }
}

#[test]
fn test_parse_identifier_expressions() {
    let input = "foo; bar; foobar;";
    let expected = vec![
        Stmt::Expr(Expr::Ident(Ident::from("foo"), ((1, 1), (1, 3)))),
        Stmt::Expr(Expr::Ident(Ident::from("bar"), ((1, 6), (1, 8)))),
        Stmt::Expr(Expr::Ident(Ident::from("foobar"), ((1, 11), (1, 16)))),
    ];
    let actual = Parser::from(input).parse();

    assert_eq!(expected, actual);
}

#[test]
fn test_parse_literal_expressions() {
    let input = r#"5; 10.0; false; "foo";"#;
    let expected = vec![
        Stmt::Expr(Expr::Literal(Literal::Int(5), ((1, 1), (1, 1)))),
        Stmt::Expr(Expr::Literal(Literal::Float(10.0), ((1, 4), (1, 7)))),
        Stmt::Expr(Expr::Literal(Literal::Bool(false), ((1, 10), (1, 14)))),
        Stmt::Expr(Expr::Literal(
            Literal::Str(String::from("foo")),
            ((1, 17), (1, 21)),
        )),
    ];
    let actual = Parser::from(input).parse();

    assert_eq!(expected, actual);
}

#[test]
fn test_parse_prefix_expressions() {
    let input = "!5; -5; !true; !false;";
    let expected = vec![
        Stmt::Expr(Expr::Prefix(
            Prefix::Not,
            Box::new(Expr::Literal(Literal::Int(5), ((1, 2), (1, 2)))),
            ((1, 1), (1, 2)),
        )),
        Stmt::Expr(Expr::Prefix(
            Prefix::Minus,
            Box::new(Expr::Literal(Literal::Int(5), ((1, 6), (1, 6)))),
            ((1, 5), (1, 6)),
        )),
        Stmt::Expr(Expr::Prefix(
            Prefix::Not,
            Box::new(Expr::Literal(Literal::Bool(true), ((1, 10), (1, 13)))),
            ((1, 9), (1, 13)),
        )),
        Stmt::Expr(Expr::Prefix(
            Prefix::Not,
            Box::new(Expr::Literal(Literal::Bool(false), ((1, 17), (1, 21)))),
            ((1, 16), (1, 21)),
        )),
    ];
    let actual = Parser::from(input).parse();

    assert_eq!(expected, actual);
}

#[test]
fn test_parse_infix_expressions() {
    let tests = vec![
        (
            "3 + 2",
            Stmt::Expr(Expr::Infix(
                Infix::Plus,
                Box::new(Expr::Literal(Literal::Int(3), ((1, 1), (1, 1)))),
                Box::new(Expr::Literal(Literal::Int(2), ((1, 5), (1, 5)))),
                ((1, 1), (1, 5)),
            )),
        ),
        (
            "5 - 2",
            Stmt::Expr(Expr::Infix(
                Infix::Minus,
                Box::new(Expr::Literal(Literal::Int(5), ((1, 1), (1, 1)))),
                Box::new(Expr::Literal(Literal::Int(2), ((1, 5), (1, 5)))),
                ((1, 1), (1, 5)),
            )),
        ),
        (
            "3 * 2",
            Stmt::Expr(Expr::Infix(
                Infix::Mul,
                Box::new(Expr::Literal(Literal::Int(3), ((1, 1), (1, 1)))),
                Box::new(Expr::Literal(Literal::Int(2), ((1, 5), (1, 5)))),
                ((1, 1), (1, 5)),
            )),
        ),
        (
            "6 / 2",
            Stmt::Expr(Expr::Infix(
                Infix::Div,
                Box::new(Expr::Literal(Literal::Int(6), ((1, 1), (1, 1)))),
                Box::new(Expr::Literal(Literal::Int(2), ((1, 5), (1, 5)))),
                ((1, 1), (1, 5)),
            )),
        ),
        (
            "7 % 2",
            Stmt::Expr(Expr::Infix(
                Infix::Mod,
                Box::new(Expr::Literal(Literal::Int(7), ((1, 1), (1, 1)))),
                Box::new(Expr::Literal(Literal::Int(2), ((1, 5), (1, 5)))),
                ((1, 1), (1, 5)),
            )),
        ),
        (
            "3 > 2",
            Stmt::Expr(Expr::Infix(
                Infix::GT,
                Box::new(Expr::Literal(Literal::Int(3), ((1, 1), (1, 1)))),
                Box::new(Expr::Literal(Literal::Int(2), ((1, 5), (1, 5)))),
                ((1, 1), (1, 5)),
            )),
        ),
        (
            "3 < 2",
            Stmt::Expr(Expr::Infix(
                Infix::LT,
                Box::new(Expr::Literal(Literal::Int(3), ((1, 1), (1, 1)))),
                Box::new(Expr::Literal(Literal::Int(2), ((1, 5), (1, 5)))),
                ((1, 1), (1, 5)),
            )),
        ),
        (
            "4 >= 2",
            Stmt::Expr(Expr::Infix(
                Infix::GTE,
                Box::new(Expr::Literal(Literal::Int(4), ((1, 1), (1, 1)))),
                Box::new(Expr::Literal(Literal::Int(2), ((1, 6), (1, 6)))),
                ((1, 1), (1, 6)),
            )),
        ),
        (
            "4 <= 2",
            Stmt::Expr(Expr::Infix(
                Infix::LTE,
                Box::new(Expr::Literal(Literal::Int(4), ((1, 1), (1, 1)))),
                Box::new(Expr::Literal(Literal::Int(2), ((1, 6), (1, 6)))),
                ((1, 1), (1, 6)),
            )),
        ),
        (
            "4 == 4",
            Stmt::Expr(Expr::Infix(
                Infix::EQ,
                Box::new(Expr::Literal(Literal::Int(4), ((1, 1), (1, 1)))),
                Box::new(Expr::Literal(Literal::Int(4), ((1, 6), (1, 6)))),
                ((1, 1), (1, 6)),
            )),
        ),
        (
            "4 != 4",
            Stmt::Expr(Expr::Infix(
                Infix::NEQ,
                Box::new(Expr::Literal(Literal::Int(4), ((1, 1), (1, 1)))),
                Box::new(Expr::Literal(Literal::Int(4), ((1, 6), (1, 6)))),
                ((1, 1), (1, 6)),
            )),
        ),
        (
            "true == true",
            Stmt::Expr(Expr::Infix(
                Infix::EQ,
                Box::new(Expr::Literal(Literal::Bool(true), ((1, 1), (1, 4)))),
                Box::new(Expr::Literal(Literal::Bool(true), ((1, 9), (1, 12)))),
                ((1, 1), (1, 12)),
            )),
        ),
        (
            "false == false",
            Stmt::Expr(Expr::Infix(
                Infix::EQ,
                Box::new(Expr::Literal(Literal::Bool(false), ((1, 1), (1, 5)))),
                Box::new(Expr::Literal(Literal::Bool(false), ((1, 10), (1, 14)))),
                ((1, 1), (1, 14)),
            )),
        ),
        (
            "true != false",
            Stmt::Expr(Expr::Infix(
                Infix::NEQ,
                Box::new(Expr::Literal(Literal::Bool(true), ((1, 1), (1, 4)))),
                Box::new(Expr::Literal(Literal::Bool(false), ((1, 9), (1, 13)))),
                ((1, 1), (1, 13)),
            )),
        ),
    ];

    for (input, expected) in tests {
        let actual = Parser::from(input).parse();
        assert_eq!(expected, actual[0]);
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
                    Box::new(Expr::Ident(Ident::from("a"), ((1, 2), (1, 2)))),
                    ((1, 1), (1, 2)),
                )),
                Box::new(Expr::Ident(Ident::from("b"), ((1, 6), (1, 6)))),
                ((1, 1), (1, 6)),
            )),
        ),
        (
            "!-a",
            Stmt::Expr(Expr::Prefix(
                Prefix::Not,
                Box::new(Expr::Prefix(
                    Prefix::Minus,
                    Box::new(Expr::Ident(Ident::from("a"), ((1, 3), (1, 3)))),
                    ((1, 2), (1, 3)),
                )),
                ((1, 1), (1, 3)),
            )),
        ),
        (
            "a + b + c",
            Stmt::Expr(Expr::Infix(
                Infix::Plus,
                Box::new(Expr::Infix(
                    Infix::Plus,
                    Box::new(Expr::Ident(Ident::from("a"), ((1, 1), (1, 1)))),
                    Box::new(Expr::Ident(Ident::from("b"), ((1, 5), (1, 5)))),
                    ((1, 1), (1, 5)),
                )),
                Box::new(Expr::Ident(Ident::from("c"), ((1, 9), (1, 9)))),
                ((1, 1), (1, 9)),
            )),
        ),
        (
            "a + b - c",
            Stmt::Expr(Expr::Infix(
                Infix::Minus,
                Box::new(Expr::Infix(
                    Infix::Plus,
                    Box::new(Expr::Ident(Ident::from("a"), ((1, 1), (1, 1)))),
                    Box::new(Expr::Ident(Ident::from("b"), ((1, 5), (1, 5)))),
                    ((1, 1), (1, 5)),
                )),
                Box::new(Expr::Ident(Ident::from("c"), ((1, 9), (1, 9)))),
                ((1, 1), (1, 9)),
            )),
        ),
        (
            "a * b * c",
            Stmt::Expr(Expr::Infix(
                Infix::Mul,
                Box::new(Expr::Infix(
                    Infix::Mul,
                    Box::new(Expr::Ident(Ident::from("a"), ((1, 1), (1, 1)))),
                    Box::new(Expr::Ident(Ident::from("b"), ((1, 5), (1, 5)))),
                    ((1, 1), (1, 5)),
                )),
                Box::new(Expr::Ident(Ident::from("c"), ((1, 9), (1, 9)))),
                ((1, 1), (1, 9)),
            )),
        ),
        (
            "a * b / c",
            Stmt::Expr(Expr::Infix(
                Infix::Div,
                Box::new(Expr::Infix(
                    Infix::Mul,
                    Box::new(Expr::Ident(Ident::from("a"), ((1, 1), (1, 1)))),
                    Box::new(Expr::Ident(Ident::from("b"), ((1, 5), (1, 5)))),
                    ((1, 1), (1, 5)),
                )),
                Box::new(Expr::Ident(Ident::from("c"), ((1, 9), (1, 9)))),
                ((1, 1), (1, 9)),
            )),
        ),
        (
            "a + b / c",
            Stmt::Expr(Expr::Infix(
                Infix::Plus,
                Box::new(Expr::Ident(Ident::from("a"), ((1, 1), (1, 1)))),
                Box::new(Expr::Infix(
                    Infix::Div,
                    Box::new(Expr::Ident(Ident::from("b"), ((1, 5), (1, 5)))),
                    Box::new(Expr::Ident(Ident::from("c"), ((1, 9), (1, 9)))),
                    ((1, 5), (1, 9)),
                )),
                ((1, 1), (1, 9)),
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
                        Box::new(Expr::Ident(Ident::from("a"), ((1, 1), (1, 1)))),
                        Box::new(Expr::Infix(
                            Infix::Mul,
                            Box::new(Expr::Ident(Ident::from("b"), ((1, 5), (1, 5)))),
                            Box::new(Expr::Ident(Ident::from("c"), ((1, 9), (1, 9)))),
                            ((1, 5), (1, 9)),
                        )),
                        ((1, 1), (1, 9)),
                    )),
                    Box::new(Expr::Infix(
                        Infix::Div,
                        Box::new(Expr::Ident(Ident::from("d"), ((1, 13), (1, 13)))),
                        Box::new(Expr::Ident(Ident::from("e"), ((1, 17), (1, 17)))),
                        ((1, 13), (1, 17)),
                    )),
                    ((1, 1), (1, 17)),
                )),
                Box::new(Expr::Ident(Ident::from("f"), ((1, 21), (1, 21)))),
                ((1, 1), (1, 21)),
            )),
        ),
        (
            "5 > 4 == 3 < 4",
            Stmt::Expr(Expr::Infix(
                Infix::EQ,
                Box::new(Expr::Infix(
                    Infix::GT,
                    Box::new(Expr::Literal(Literal::Int(5), ((1, 1), (1, 1)))),
                    Box::new(Expr::Literal(Literal::Int(4), ((1, 5), (1, 5)))),
                    ((1, 1), (1, 5)),
                )),
                Box::new(Expr::Infix(
                    Infix::LT,
                    Box::new(Expr::Literal(Literal::Int(3), ((1, 10), (1, 10)))),
                    Box::new(Expr::Literal(Literal::Int(4), ((1, 14), (1, 14)))),
                    ((1, 10), (1, 14)),
                )),
                ((1, 1), (1, 14)),
            )),
        ),
        (
            "5 < 4 != 3 > 4",
            Stmt::Expr(Expr::Infix(
                Infix::NEQ,
                Box::new(Expr::Infix(
                    Infix::LT,
                    Box::new(Expr::Literal(Literal::Int(5), ((1, 1), (1, 1)))),
                    Box::new(Expr::Literal(Literal::Int(4), ((1, 5), (1, 5)))),
                    ((1, 1), (1, 5)),
                )),
                Box::new(Expr::Infix(
                    Infix::GT,
                    Box::new(Expr::Literal(Literal::Int(3), ((1, 10), (1, 10)))),
                    Box::new(Expr::Literal(Literal::Int(4), ((1, 14), (1, 14)))),
                    ((1, 10), (1, 14)),
                )),
                ((1, 1), (1, 14)),
            )),
        ),
        (
            "3 + 4 * 5 == 3 * 1 + 4 * 5",
            Stmt::Expr(Expr::Infix(
                Infix::EQ,
                Box::new(Expr::Infix(
                    Infix::Plus,
                    Box::new(Expr::Literal(Literal::Int(3), ((1, 1), (1, 1)))),
                    Box::new(Expr::Infix(
                        Infix::Mul,
                        Box::new(Expr::Literal(Literal::Int(4), ((1, 5), (1, 5)))),
                        Box::new(Expr::Literal(Literal::Int(5), ((1, 9), (1, 9)))),
                        ((1, 5), (1, 9)),
                    )),
                    ((1, 1), (1, 9)),
                )),
                Box::new(Expr::Infix(
                    Infix::Plus,
                    Box::new(Expr::Infix(
                        Infix::Mul,
                        Box::new(Expr::Literal(Literal::Int(3), ((1, 14), (1, 14)))),
                        Box::new(Expr::Literal(Literal::Int(1), ((1, 18), (1, 18)))),
                        ((1, 14), (1, 18)),
                    )),
                    Box::new(Expr::Infix(
                        Infix::Mul,
                        Box::new(Expr::Literal(Literal::Int(4), ((1, 22), (1, 22)))),
                        Box::new(Expr::Literal(Literal::Int(5), ((1, 26), (1, 26)))),
                        ((1, 22), (1, 26)),
                    )),
                    ((1, 14), (1, 26)),
                )),
                ((1, 1), (1, 26)),
            )),
        ),
        (
            "true",
            Stmt::Expr(Expr::Literal(Literal::Bool(true), ((1, 1), (1, 4)))),
        ),
        (
            "false",
            Stmt::Expr(Expr::Literal(Literal::Bool(false), ((1, 1), (1, 5)))),
        ),
        (
            "1 < 2 == true",
            Stmt::Expr(Expr::Infix(
                Infix::EQ,
                Box::new(Expr::Infix(
                    Infix::LT,
                    Box::new(Expr::Literal(Literal::Int(1), ((1, 1), (1, 1)))),
                    Box::new(Expr::Literal(Literal::Int(2), ((1, 5), (1, 5)))),
                    ((1, 1), (1, 5)),
                )),
                Box::new(Expr::Literal(Literal::Bool(true), ((1, 10), (1, 13)))),
                ((1, 1), (1, 13)),
            )),
        ),
        (
            "1 > 2 == false",
            Stmt::Expr(Expr::Infix(
                Infix::EQ,
                Box::new(Expr::Infix(
                    Infix::GT,
                    Box::new(Expr::Literal(Literal::Int(1), ((1, 1), (1, 1)))),
                    Box::new(Expr::Literal(Literal::Int(2), ((1, 5), (1, 5)))),
                    ((1, 1), (1, 5)),
                )),
                Box::new(Expr::Literal(Literal::Bool(false), ((1, 10), (1, 14)))),
                ((1, 1), (1, 14)),
            )),
        ),
        (
            "1 + (2 + 3) + 4",
            Stmt::Expr(Expr::Infix(
                Infix::Plus,
                Box::new(Expr::Infix(
                    Infix::Plus,
                    Box::new(Expr::Literal(Literal::Int(1), ((1, 1), (1, 1)))),
                    Box::new(Expr::Infix(
                        Infix::Plus,
                        Box::new(Expr::Literal(Literal::Int(2), ((1, 6), (1, 6)))),
                        Box::new(Expr::Literal(Literal::Int(3), ((1, 10), (1, 10)))),
                        ((1, 6), (1, 10)),
                    )),
                    ((1, 1), (1, 10)),
                )),
                Box::new(Expr::Literal(Literal::Int(4), ((1, 15), (1, 15)))),
                ((1, 1), (1, 15)),
            )),
        ),
        (
            "(1 + 1) * 2",
            Stmt::Expr(Expr::Infix(
                Infix::Mul,
                Box::new(Expr::Infix(
                    Infix::Plus,
                    Box::new(Expr::Literal(Literal::Int(1), ((1, 2), (1, 2)))),
                    Box::new(Expr::Literal(Literal::Int(1), ((1, 6), (1, 6)))),
                    ((1, 2), (1, 6)),
                )),
                Box::new(Expr::Literal(Literal::Int(2), ((1, 11), (1, 11)))),
                ((1, 2), (1, 11)),
            )),
        ),
        (
            "1 / (2 + 2)",
            Stmt::Expr(Expr::Infix(
                Infix::Div,
                Box::new(Expr::Literal(Literal::Int(1), ((1, 1), (1, 1)))),
                Box::new(Expr::Infix(
                    Infix::Plus,
                    Box::new(Expr::Literal(Literal::Int(2), ((1, 6), (1, 6)))),
                    Box::new(Expr::Literal(Literal::Int(2), ((1, 10), (1, 10)))),
                    ((1, 6), (1, 10)),
                )),
                ((1, 1), (1, 10)),
            )),
        ),
        (
            "-(1 + 2)",
            Stmt::Expr(Expr::Prefix(
                Prefix::Minus,
                Box::new(Expr::Infix(
                    Infix::Plus,
                    Box::new(Expr::Literal(Literal::Int(1), ((1, 3), (1, 3)))),
                    Box::new(Expr::Literal(Literal::Int(2), ((1, 7), (1, 7)))),
                    ((1, 3), (1, 7)),
                )),
                ((1, 1), (1, 7)),
            )),
        ),
        (
            "!(true == true)",
            Stmt::Expr(Expr::Prefix(
                Prefix::Not,
                Box::new(Expr::Infix(
                    Infix::EQ,
                    Box::new(Expr::Literal(Literal::Bool(true), ((1, 3), (1, 6)))),
                    Box::new(Expr::Literal(Literal::Bool(true), ((1, 11), (1, 14)))),
                    ((1, 3), (1, 14)),
                )),
                ((1, 1), (1, 14)),
            )),
        ),
    ];

    for (input, expected) in tests {
        dbg!(input);
        let actual = Parser::from(input).parse();
        assert_eq!(expected, actual[0]);
    }
}

#[test]
fn test_parse_if_expression() {
    let input = "if x < y { return x }";
    let expected = Stmt::Expr(Expr::If {
        cond: Box::new(Expr::Infix(
            Infix::LT,
            Box::new(Expr::Ident(Ident::from("x"), ((1, 4), (1, 4)))),
            Box::new(Expr::Ident(Ident::from("y"), ((1, 8), (1, 8)))),
            ((1, 4), (1, 8)),
        )),
        then: vec![Stmt::Return(Expr::Ident(
            Ident::from("x"),
            ((1, 19), (1, 19)),
        ))],
        elifs: vec![],
        alt: None,
        span: ((1, 1), (1, 21)),
    });
    let actual = Parser::from(input).parse();

    assert_eq!(expected, actual[0]);
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
    let expected = Stmt::Expr(Expr::If {
        cond: Box::new(Expr::Infix(
            Infix::LT,
            Box::new(Expr::Ident(Ident::from("x"), ((1, 4), (1, 4)))),
            Box::new(Expr::Ident(Ident::from("y"), ((1, 8), (1, 8)))),
            ((1, 4), (1, 8)),
        )),
        then: vec![
            Stmt::Expr(Expr::If {
                cond: Box::new(Expr::Infix(
                    Infix::GT,
                    Box::new(Expr::Ident(Ident::from("x"), ((2, 4), (2, 4)))),
                    Box::new(Expr::Literal(Literal::Int(1), ((2, 8), (2, 8)))),
                    ((2, 4), (2, 8)),
                )),
                then: vec![Stmt::Return(Expr::Ident(
                    Ident::from("x"),
                    ((3, 8), (3, 8)),
                ))],
                elifs: vec![],
                alt: None,
                span: ((2, 1), (4, 1)),
            }),
            Stmt::Return(Expr::Ident(Ident::from("y"), ((5, 8), (5, 8)))),
        ],
        elifs: vec![],
        alt: Some(vec![Stmt::Return(Expr::Ident(
            Ident::from("z"),
            ((7, 8), (7, 8)),
        ))]),
        span: ((1, 1), (8, 1)),
    });
    let actual = Parser::from(input).parse();

    assert_eq!(expected, actual[0]);
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
            Box::new(Expr::Ident(Ident::from("x"), ((1, 4), (1, 4)))),
            Box::new(Expr::Ident(Ident::from("y"), ((1, 8), (1, 8)))),
            ((1, 4), (1, 8)),
        )),
        then: vec![Stmt::Return(Expr::Ident(
            Ident::from("x"),
            ((2, 8), (2, 8)),
        ))],
        elifs: vec![
            IfExpr {
                cond: Box::new(Expr::Infix(
                    Infix::GT,
                    Box::new(Expr::Ident(Ident::from("x"), ((3, 8), (3, 8)))),
                    Box::new(Expr::Ident(Ident::from("y"), ((3, 12), (3, 12)))),
                    ((3, 8), (3, 12)),
                )),
                then: vec![Stmt::Return(Expr::Ident(
                    Ident::from("y"),
                    ((4, 8), (4, 8)),
                ))],
            },
            IfExpr {
                cond: Box::new(Expr::Infix(
                    Infix::GT,
                    Box::new(Expr::Ident(Ident::from("x"), ((5, 8), (5, 8)))),
                    Box::new(Expr::Literal(Literal::Int(1), ((5, 12), (5, 12)))),
                    ((5, 8), (5, 12)),
                )),
                then: vec![Stmt::Return(Expr::Literal(
                    Literal::Int(1),
                    ((6, 8), (6, 8)),
                ))],
            },
        ],
        alt: Some(vec![Stmt::Return(Expr::Ident(
            Ident::from("z"),
            ((8, 8), (8, 8)),
        ))]),
        span: ((1, 1), (9, 1)),
    });
    let actual = Parser::from(input).parse();

    assert_eq!(expected, actual[0]);
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
                    ((2, 8), (2, 8)),
                ))],
                span: ((1, 1), (3, 1)),
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
                    Box::new(Expr::Ident(Ident::from("x"), ((2, 8), (2, 8)))),
                    Box::new(Expr::Ident(Ident::from("y"), ((2, 12), (2, 12)))),
                    ((2, 8), (2, 12)),
                ))],
                span: ((1, 1), (3, 1)),
            }),
        ),
    ];

    for (input, expected) in tests {
        dbg!(input);
        let actual = Parser::from(input).parse();
        assert_eq!(expected, actual[0]);
    }
}

#[test]
fn test_parse_call_expression() {
    let input = "add(1, 2 * 3, 4 + 5);";
    let expected = Stmt::Expr(Expr::Call {
        func: Box::new(Expr::Ident(Ident::from("add"), ((1, 1), (1, 3)))),
        args: vec![
            Expr::Literal(Literal::Int(1), ((1, 5), (1, 5))),
            Expr::Infix(
                Infix::Mul,
                Box::new(Expr::Literal(Literal::Int(2), ((1, 8), (1, 8)))),
                Box::new(Expr::Literal(Literal::Int(3), ((1, 12), (1, 12)))),
                ((1, 8), (1, 12)),
            ),
            Expr::Infix(
                Infix::Plus,
                Box::new(Expr::Literal(Literal::Int(4), ((1, 15), (1, 15)))),
                Box::new(Expr::Literal(Literal::Int(5), ((1, 19), (1, 19)))),
                ((1, 15), (1, 19)),
            ),
        ],
        span: ((1, 1), (1, 20)),
    });
    let actual = Parser::from(input).parse();

    assert_eq!(expected, actual[0]);
}
