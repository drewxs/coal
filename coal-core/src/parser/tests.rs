use crate::{F64, I32, U32};

use super::*;

#[test]
fn test_parse_let_statements() {
    let tests = vec![
        (
            "let x: i32 = 5;",
            Stmt::Let(
                Ident(String::from("x")),
                I32,
                Expr::Literal(Literal::I32(5), ((1, 14), (1, 14))),
            ),
        ),
        (
            "let y: i32 = 10;",
            Stmt::Let(
                Ident(String::from("y")),
                I32,
                Expr::Literal(Literal::I32(10), ((1, 14), (1, 15))),
            ),
        ),
        (
            "let z: i32 = 99999;",
            Stmt::Let(
                Ident(String::from("z")),
                I32,
                Expr::Literal(Literal::I32(99999), ((1, 14), (1, 18))),
            ),
        ),
        (
            "let foo: Foo = 0;",
            Stmt::Let(
                Ident(String::from("foo")),
                I32,
                Expr::Literal(Literal::I32(0), ((1, 16), (1, 16))),
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
            I32,
            Expr::Literal(Literal::I32(5), ((1, 9), (1, 9))),
        ),
        Stmt::Let(
            Ident(String::from("y")),
            F64,
            Expr::Literal(Literal::F64(5.0), ((2, 9), (2, 11))),
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
fn test_parse_assign_statements() {
    let input = r#"
        x = 1;
        y = "foo";;
        z = true;
        x += 1;
        "#;
    let expected = vec![
        Stmt::Assign(
            Ident(String::from("x")),
            Expr::Literal(Literal::I32(1), ((1, 5), (1, 5))),
        ),
        Stmt::Assign(
            Ident(String::from("y")),
            Expr::Literal(Literal::from("foo"), ((2, 5), (2, 9))),
        ),
        Stmt::Assign(
            Ident(String::from("z")),
            Expr::Literal(Literal::Bool(true), ((3, 5), (3, 8))),
        ),
        Stmt::AddAssign(
            Ident(String::from("x")),
            Expr::Literal(Literal::I32(1), ((4, 6), (4, 6))),
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
        Stmt::Expr(Expr::Ident(
            Ident::from("foo"),
            Type::Unknown,
            ((1, 1), (1, 3)),
        )),
        Stmt::Expr(Expr::Ident(
            Ident::from("bar"),
            Type::Unknown,
            ((1, 6), (1, 8)),
        )),
        Stmt::Expr(Expr::Ident(
            Ident::from("foobar"),
            Type::Unknown,
            ((1, 11), (1, 16)),
        )),
    ];
    let actual = Parser::from(input).parse();

    assert_eq!(expected, actual);
}

#[test]
fn test_parse_literal_expressions() {
    let input = r#"5; 10.0; false; "foo";"#;
    let expected = vec![
        Stmt::Expr(Expr::Literal(Literal::I32(5), ((1, 1), (1, 1)))),
        Stmt::Expr(Expr::Literal(Literal::F64(10.0), ((1, 4), (1, 7)))),
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
            Box::new(Expr::Literal(Literal::I32(5), ((1, 2), (1, 2)))),
            ((1, 1), (1, 2)),
        )),
        Stmt::Expr(Expr::Prefix(
            Prefix::Minus,
            Box::new(Expr::Literal(Literal::I32(5), ((1, 6), (1, 6)))),
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
                Infix::Add,
                Box::new(Expr::Literal(Literal::I32(3), ((1, 1), (1, 1)))),
                Box::new(Expr::Literal(Literal::I32(2), ((1, 5), (1, 5)))),
                ((1, 1), (1, 5)),
            )),
        ),
        (
            "5 - 2",
            Stmt::Expr(Expr::Infix(
                Infix::Sub,
                Box::new(Expr::Literal(Literal::I32(5), ((1, 1), (1, 1)))),
                Box::new(Expr::Literal(Literal::I32(2), ((1, 5), (1, 5)))),
                ((1, 1), (1, 5)),
            )),
        ),
        (
            "3 * 2",
            Stmt::Expr(Expr::Infix(
                Infix::Mul,
                Box::new(Expr::Literal(Literal::I32(3), ((1, 1), (1, 1)))),
                Box::new(Expr::Literal(Literal::I32(2), ((1, 5), (1, 5)))),
                ((1, 1), (1, 5)),
            )),
        ),
        (
            "6 / 2",
            Stmt::Expr(Expr::Infix(
                Infix::Div,
                Box::new(Expr::Literal(Literal::I32(6), ((1, 1), (1, 1)))),
                Box::new(Expr::Literal(Literal::I32(2), ((1, 5), (1, 5)))),
                ((1, 1), (1, 5)),
            )),
        ),
        (
            "7 % 2",
            Stmt::Expr(Expr::Infix(
                Infix::Rem,
                Box::new(Expr::Literal(Literal::I32(7), ((1, 1), (1, 1)))),
                Box::new(Expr::Literal(Literal::I32(2), ((1, 5), (1, 5)))),
                ((1, 1), (1, 5)),
            )),
        ),
        (
            "3 > 2",
            Stmt::Expr(Expr::Infix(
                Infix::GT,
                Box::new(Expr::Literal(Literal::I32(3), ((1, 1), (1, 1)))),
                Box::new(Expr::Literal(Literal::I32(2), ((1, 5), (1, 5)))),
                ((1, 1), (1, 5)),
            )),
        ),
        (
            "3 < 2",
            Stmt::Expr(Expr::Infix(
                Infix::LT,
                Box::new(Expr::Literal(Literal::I32(3), ((1, 1), (1, 1)))),
                Box::new(Expr::Literal(Literal::I32(2), ((1, 5), (1, 5)))),
                ((1, 1), (1, 5)),
            )),
        ),
        (
            "4 >= 2",
            Stmt::Expr(Expr::Infix(
                Infix::GTE,
                Box::new(Expr::Literal(Literal::I32(4), ((1, 1), (1, 1)))),
                Box::new(Expr::Literal(Literal::I32(2), ((1, 6), (1, 6)))),
                ((1, 1), (1, 6)),
            )),
        ),
        (
            "4 <= 2",
            Stmt::Expr(Expr::Infix(
                Infix::LTE,
                Box::new(Expr::Literal(Literal::I32(4), ((1, 1), (1, 1)))),
                Box::new(Expr::Literal(Literal::I32(2), ((1, 6), (1, 6)))),
                ((1, 1), (1, 6)),
            )),
        ),
        (
            "4 == 4",
            Stmt::Expr(Expr::Infix(
                Infix::EQ,
                Box::new(Expr::Literal(Literal::I32(4), ((1, 1), (1, 1)))),
                Box::new(Expr::Literal(Literal::I32(4), ((1, 6), (1, 6)))),
                ((1, 1), (1, 6)),
            )),
        ),
        (
            "4 != 4",
            Stmt::Expr(Expr::Infix(
                Infix::NEQ,
                Box::new(Expr::Literal(Literal::I32(4), ((1, 1), (1, 1)))),
                Box::new(Expr::Literal(Literal::I32(4), ((1, 6), (1, 6)))),
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
                    Box::new(Expr::Ident(
                        Ident::from("a"),
                        Type::Unknown,
                        ((1, 2), (1, 2)),
                    )),
                    ((1, 1), (1, 2)),
                )),
                Box::new(Expr::Ident(
                    Ident::from("b"),
                    Type::Unknown,
                    ((1, 6), (1, 6)),
                )),
                ((1, 1), (1, 6)),
            )),
        ),
        (
            "!-a",
            Stmt::Expr(Expr::Prefix(
                Prefix::Not,
                Box::new(Expr::Prefix(
                    Prefix::Minus,
                    Box::new(Expr::Ident(
                        Ident::from("a"),
                        Type::Unknown,
                        ((1, 3), (1, 3)),
                    )),
                    ((1, 2), (1, 3)),
                )),
                ((1, 1), (1, 3)),
            )),
        ),
        (
            "a + b + c",
            Stmt::Expr(Expr::Infix(
                Infix::Add,
                Box::new(Expr::Infix(
                    Infix::Add,
                    Box::new(Expr::Ident(
                        Ident::from("a"),
                        Type::Unknown,
                        ((1, 1), (1, 1)),
                    )),
                    Box::new(Expr::Ident(
                        Ident::from("b"),
                        Type::Unknown,
                        ((1, 5), (1, 5)),
                    )),
                    ((1, 1), (1, 5)),
                )),
                Box::new(Expr::Ident(
                    Ident::from("c"),
                    Type::Unknown,
                    ((1, 9), (1, 9)),
                )),
                ((1, 1), (1, 9)),
            )),
        ),
        (
            "a + b - c",
            Stmt::Expr(Expr::Infix(
                Infix::Sub,
                Box::new(Expr::Infix(
                    Infix::Add,
                    Box::new(Expr::Ident(
                        Ident::from("a"),
                        Type::Unknown,
                        ((1, 1), (1, 1)),
                    )),
                    Box::new(Expr::Ident(
                        Ident::from("b"),
                        Type::Unknown,
                        ((1, 5), (1, 5)),
                    )),
                    ((1, 1), (1, 5)),
                )),
                Box::new(Expr::Ident(
                    Ident::from("c"),
                    Type::Unknown,
                    ((1, 9), (1, 9)),
                )),
                ((1, 1), (1, 9)),
            )),
        ),
        (
            "a * b * c",
            Stmt::Expr(Expr::Infix(
                Infix::Mul,
                Box::new(Expr::Infix(
                    Infix::Mul,
                    Box::new(Expr::Ident(
                        Ident::from("a"),
                        Type::Unknown,
                        ((1, 1), (1, 1)),
                    )),
                    Box::new(Expr::Ident(
                        Ident::from("b"),
                        Type::Unknown,
                        ((1, 5), (1, 5)),
                    )),
                    ((1, 1), (1, 5)),
                )),
                Box::new(Expr::Ident(
                    Ident::from("c"),
                    Type::Unknown,
                    ((1, 9), (1, 9)),
                )),
                ((1, 1), (1, 9)),
            )),
        ),
        (
            "a * b / c",
            Stmt::Expr(Expr::Infix(
                Infix::Div,
                Box::new(Expr::Infix(
                    Infix::Mul,
                    Box::new(Expr::Ident(
                        Ident::from("a"),
                        Type::Unknown,
                        ((1, 1), (1, 1)),
                    )),
                    Box::new(Expr::Ident(
                        Ident::from("b"),
                        Type::Unknown,
                        ((1, 5), (1, 5)),
                    )),
                    ((1, 1), (1, 5)),
                )),
                Box::new(Expr::Ident(
                    Ident::from("c"),
                    Type::Unknown,
                    ((1, 9), (1, 9)),
                )),
                ((1, 1), (1, 9)),
            )),
        ),
        (
            "a + b / c",
            Stmt::Expr(Expr::Infix(
                Infix::Add,
                Box::new(Expr::Ident(
                    Ident::from("a"),
                    Type::Unknown,
                    ((1, 1), (1, 1)),
                )),
                Box::new(Expr::Infix(
                    Infix::Div,
                    Box::new(Expr::Ident(
                        Ident::from("b"),
                        Type::Unknown,
                        ((1, 5), (1, 5)),
                    )),
                    Box::new(Expr::Ident(
                        Ident::from("c"),
                        Type::Unknown,
                        ((1, 9), (1, 9)),
                    )),
                    ((1, 5), (1, 9)),
                )),
                ((1, 1), (1, 9)),
            )),
        ),
        (
            "a + b * c + d / e - f",
            Stmt::Expr(Expr::Infix(
                Infix::Sub,
                Box::new(Expr::Infix(
                    Infix::Add,
                    Box::new(Expr::Infix(
                        Infix::Add,
                        Box::new(Expr::Ident(
                            Ident::from("a"),
                            Type::Unknown,
                            ((1, 1), (1, 1)),
                        )),
                        Box::new(Expr::Infix(
                            Infix::Mul,
                            Box::new(Expr::Ident(
                                Ident::from("b"),
                                Type::Unknown,
                                ((1, 5), (1, 5)),
                            )),
                            Box::new(Expr::Ident(
                                Ident::from("c"),
                                Type::Unknown,
                                ((1, 9), (1, 9)),
                            )),
                            ((1, 5), (1, 9)),
                        )),
                        ((1, 1), (1, 9)),
                    )),
                    Box::new(Expr::Infix(
                        Infix::Div,
                        Box::new(Expr::Ident(
                            Ident::from("d"),
                            Type::Unknown,
                            ((1, 13), (1, 13)),
                        )),
                        Box::new(Expr::Ident(
                            Ident::from("e"),
                            Type::Unknown,
                            ((1, 17), (1, 17)),
                        )),
                        ((1, 13), (1, 17)),
                    )),
                    ((1, 1), (1, 17)),
                )),
                Box::new(Expr::Ident(
                    Ident::from("f"),
                    Type::Unknown,
                    ((1, 21), (1, 21)),
                )),
                ((1, 1), (1, 21)),
            )),
        ),
        (
            "5 > 4 == 3 < 4",
            Stmt::Expr(Expr::Infix(
                Infix::EQ,
                Box::new(Expr::Infix(
                    Infix::GT,
                    Box::new(Expr::Literal(Literal::I32(5), ((1, 1), (1, 1)))),
                    Box::new(Expr::Literal(Literal::I32(4), ((1, 5), (1, 5)))),
                    ((1, 1), (1, 5)),
                )),
                Box::new(Expr::Infix(
                    Infix::LT,
                    Box::new(Expr::Literal(Literal::I32(3), ((1, 10), (1, 10)))),
                    Box::new(Expr::Literal(Literal::I32(4), ((1, 14), (1, 14)))),
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
                    Box::new(Expr::Literal(Literal::I32(5), ((1, 1), (1, 1)))),
                    Box::new(Expr::Literal(Literal::I32(4), ((1, 5), (1, 5)))),
                    ((1, 1), (1, 5)),
                )),
                Box::new(Expr::Infix(
                    Infix::GT,
                    Box::new(Expr::Literal(Literal::I32(3), ((1, 10), (1, 10)))),
                    Box::new(Expr::Literal(Literal::I32(4), ((1, 14), (1, 14)))),
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
                    Infix::Add,
                    Box::new(Expr::Literal(Literal::I32(3), ((1, 1), (1, 1)))),
                    Box::new(Expr::Infix(
                        Infix::Mul,
                        Box::new(Expr::Literal(Literal::I32(4), ((1, 5), (1, 5)))),
                        Box::new(Expr::Literal(Literal::I32(5), ((1, 9), (1, 9)))),
                        ((1, 5), (1, 9)),
                    )),
                    ((1, 1), (1, 9)),
                )),
                Box::new(Expr::Infix(
                    Infix::Add,
                    Box::new(Expr::Infix(
                        Infix::Mul,
                        Box::new(Expr::Literal(Literal::I32(3), ((1, 14), (1, 14)))),
                        Box::new(Expr::Literal(Literal::I32(1), ((1, 18), (1, 18)))),
                        ((1, 14), (1, 18)),
                    )),
                    Box::new(Expr::Infix(
                        Infix::Mul,
                        Box::new(Expr::Literal(Literal::I32(4), ((1, 22), (1, 22)))),
                        Box::new(Expr::Literal(Literal::I32(5), ((1, 26), (1, 26)))),
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
                    Box::new(Expr::Literal(Literal::I32(1), ((1, 1), (1, 1)))),
                    Box::new(Expr::Literal(Literal::I32(2), ((1, 5), (1, 5)))),
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
                    Box::new(Expr::Literal(Literal::I32(1), ((1, 1), (1, 1)))),
                    Box::new(Expr::Literal(Literal::I32(2), ((1, 5), (1, 5)))),
                    ((1, 1), (1, 5)),
                )),
                Box::new(Expr::Literal(Literal::Bool(false), ((1, 10), (1, 14)))),
                ((1, 1), (1, 14)),
            )),
        ),
        (
            "1 + (2 + 3) + 4",
            Stmt::Expr(Expr::Infix(
                Infix::Add,
                Box::new(Expr::Infix(
                    Infix::Add,
                    Box::new(Expr::Literal(Literal::I32(1), ((1, 1), (1, 1)))),
                    Box::new(Expr::Infix(
                        Infix::Add,
                        Box::new(Expr::Literal(Literal::I32(2), ((1, 6), (1, 6)))),
                        Box::new(Expr::Literal(Literal::I32(3), ((1, 10), (1, 10)))),
                        ((1, 6), (1, 10)),
                    )),
                    ((1, 1), (1, 10)),
                )),
                Box::new(Expr::Literal(Literal::I32(4), ((1, 15), (1, 15)))),
                ((1, 1), (1, 15)),
            )),
        ),
        (
            "(1 + 1) * 2",
            Stmt::Expr(Expr::Infix(
                Infix::Mul,
                Box::new(Expr::Infix(
                    Infix::Add,
                    Box::new(Expr::Literal(Literal::I32(1), ((1, 2), (1, 2)))),
                    Box::new(Expr::Literal(Literal::I32(1), ((1, 6), (1, 6)))),
                    ((1, 2), (1, 6)),
                )),
                Box::new(Expr::Literal(Literal::I32(2), ((1, 11), (1, 11)))),
                ((1, 2), (1, 11)),
            )),
        ),
        (
            "1 / (2 + 2)",
            Stmt::Expr(Expr::Infix(
                Infix::Div,
                Box::new(Expr::Literal(Literal::I32(1), ((1, 1), (1, 1)))),
                Box::new(Expr::Infix(
                    Infix::Add,
                    Box::new(Expr::Literal(Literal::I32(2), ((1, 6), (1, 6)))),
                    Box::new(Expr::Literal(Literal::I32(2), ((1, 10), (1, 10)))),
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
                    Infix::Add,
                    Box::new(Expr::Literal(Literal::I32(1), ((1, 3), (1, 3)))),
                    Box::new(Expr::Literal(Literal::I32(2), ((1, 7), (1, 7)))),
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
            Box::new(Expr::Ident(
                Ident::from("x"),
                Type::Unknown,
                ((1, 4), (1, 4)),
            )),
            Box::new(Expr::Ident(
                Ident::from("y"),
                Type::Unknown,
                ((1, 8), (1, 8)),
            )),
            ((1, 4), (1, 8)),
        )),
        then: vec![Stmt::Return(Expr::Ident(
            Ident::from("x"),
            Type::Unknown,
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
            Box::new(Expr::Ident(
                Ident::from("x"),
                Type::Unknown,
                ((1, 4), (1, 4)),
            )),
            Box::new(Expr::Ident(
                Ident::from("y"),
                Type::Unknown,
                ((1, 8), (1, 8)),
            )),
            ((1, 4), (1, 8)),
        )),
        then: vec![
            Stmt::Expr(Expr::If {
                cond: Box::new(Expr::Infix(
                    Infix::GT,
                    Box::new(Expr::Ident(
                        Ident::from("x"),
                        Type::Unknown,
                        ((2, 4), (2, 4)),
                    )),
                    Box::new(Expr::Literal(Literal::I32(1), ((2, 8), (2, 8)))),
                    ((2, 4), (2, 8)),
                )),
                then: vec![Stmt::Return(Expr::Ident(
                    Ident::from("x"),
                    Type::Unknown,
                    ((3, 8), (3, 8)),
                ))],
                elifs: vec![],
                alt: None,
                span: ((2, 1), (4, 1)),
            }),
            Stmt::Return(Expr::Ident(
                Ident::from("y"),
                Type::Unknown,
                ((5, 8), (5, 8)),
            )),
        ],
        elifs: vec![],
        alt: Some(vec![Stmt::Return(Expr::Ident(
            Ident::from("z"),
            Type::Unknown,
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
            Box::new(Expr::Ident(
                Ident::from("x"),
                Type::Unknown,
                ((1, 4), (1, 4)),
            )),
            Box::new(Expr::Ident(
                Ident::from("y"),
                Type::Unknown,
                ((1, 8), (1, 8)),
            )),
            ((1, 4), (1, 8)),
        )),
        then: vec![Stmt::Return(Expr::Ident(
            Ident::from("x"),
            Type::Unknown,
            ((2, 8), (2, 8)),
        ))],
        elifs: vec![
            IfExpr {
                cond: Box::new(Expr::Infix(
                    Infix::GT,
                    Box::new(Expr::Ident(
                        Ident::from("x"),
                        Type::Unknown,
                        ((3, 8), (3, 8)),
                    )),
                    Box::new(Expr::Ident(
                        Ident::from("y"),
                        Type::Unknown,
                        ((3, 12), (3, 12)),
                    )),
                    ((3, 8), (3, 12)),
                )),
                then: vec![Stmt::Return(Expr::Ident(
                    Ident::from("y"),
                    Type::Unknown,
                    ((4, 8), (4, 8)),
                ))],
            },
            IfExpr {
                cond: Box::new(Expr::Infix(
                    Infix::GT,
                    Box::new(Expr::Ident(
                        Ident::from("x"),
                        Type::Unknown,
                        ((5, 8), (5, 8)),
                    )),
                    Box::new(Expr::Literal(Literal::I32(1), ((5, 12), (5, 12)))),
                    ((5, 8), (5, 12)),
                )),
                then: vec![Stmt::Return(Expr::Literal(
                    Literal::I32(1),
                    ((6, 8), (6, 8)),
                ))],
            },
        ],
        alt: Some(vec![Stmt::Return(Expr::Ident(
            Ident::from("z"),
            Type::Unknown,
            ((8, 8), (8, 8)),
        ))]),
        span: ((1, 1), (9, 1)),
    });
    let actual = Parser::from(input).parse();

    assert_eq!(expected, actual[0]);
}

#[test]
fn test_parse_while_expression() {
    let input = "while x < y { 0; }";
    let expected = Stmt::Expr(Expr::While {
        cond: Box::new(Expr::Infix(
            Infix::LT,
            Box::new(Expr::Ident(
                Ident::from("x"),
                Type::Unknown,
                ((1, 7), (1, 7)),
            )),
            Box::new(Expr::Ident(
                Ident::from("y"),
                Type::Unknown,
                ((1, 11), (1, 11)),
            )),
            ((1, 7), (1, 11)),
        )),
        body: vec![Stmt::Expr(Expr::Literal(
            Literal::I32(0),
            ((1, 15), (1, 15)),
        ))],
        span: ((1, 1), (1, 18)),
    });
    let actual = Parser::from(input).parse();

    assert_eq!(expected, actual[0]);
}

#[test]
fn test_parse_function_expressions() {
    let tests = [
        (
            "fn foo() {}",
            Stmt::Expr(Expr::Fn {
                name: String::from("foo"),
                args: vec![],
                ret_t: Type::Void,
                body: vec![],
                span: ((1, 1), (1, 11)),
            }),
        ),
        (
            r#"fn foo() {
                   return 0;
               }"#,
            Stmt::Expr(Expr::Fn {
                name: String::from("foo"),
                args: vec![],
                ret_t: Type::Void,
                body: vec![Stmt::Return(Expr::Literal(
                    Literal::I32(0),
                    ((2, 8), (2, 8)),
                ))],
                span: ((1, 1), (3, 1)),
            }),
        ),
        (
            r#"fn foo() -> i32 {
                   return 0;
               }"#,
            Stmt::Expr(Expr::Fn {
                name: String::from("foo"),
                args: vec![],
                ret_t: I32,
                body: vec![Stmt::Return(Expr::Literal(
                    Literal::I32(0),
                    ((2, 8), (2, 8)),
                ))],
                span: ((1, 1), (3, 1)),
            }),
        ),
        (
            r#"fn add(x: i32, y: i32) -> i32 {
                   return x + y;
               }"#,
            Stmt::Expr(Expr::Fn {
                name: String::from("add"),
                args: vec![Var::new("x", I32), Var::new("y", I32)],
                ret_t: I32,
                body: vec![Stmt::Return(Expr::Infix(
                    Infix::Add,
                    Box::new(Expr::Ident(Ident::from("x"), I32, ((2, 8), (2, 8)))),
                    Box::new(Expr::Ident(Ident::from("y"), I32, ((2, 12), (2, 12)))),
                    ((2, 8), (2, 12)),
                ))],
                span: ((1, 1), (3, 1)),
            }),
        ),
        (
            r#"fn add(x: u32, f: Fn(u32, u32) -> u32) -> u32 {
                   return f(x, 0);
               }"#,
            Stmt::Expr(Expr::Fn {
                name: String::from("add"),
                args: vec![
                    Var::new("x", U32),
                    Var::new("f", Type::Fn(vec![U32, U32], Box::new(U32))),
                ],
                ret_t: U32,
                body: vec![Stmt::Return(Expr::Call {
                    name: String::from("f"),
                    args: vec![
                        Expr::Ident(Ident::from("x"), U32, ((2, 10), (2, 10))),
                        Expr::Literal(Literal::I32(0), ((2, 13), (2, 13))),
                    ],
                    ret_t: U32,
                    span: ((2, 8), (2, 14)),
                })],
                span: ((1, 1), (3, 1)),
            }),
        ),
    ];

    for (input, expected) in tests {
        let actual = Parser::from(input).parse();
        if expected != actual[0] {
            panic!(
                "input:\n{}\nexpected:\n{:?}\nactual:\n{:?}",
                input, expected, actual[0]
            );
        }
    }
}

#[test]
fn test_parse_call_expression() {
    let input = "add(1, 2 * 3, 4 + 5);";
    let expected = Stmt::Expr(Expr::Call {
        name: String::from("add"),
        args: vec![
            Expr::Literal(Literal::I32(1), ((1, 5), (1, 5))),
            Expr::Infix(
                Infix::Mul,
                Box::new(Expr::Literal(Literal::I32(2), ((1, 8), (1, 8)))),
                Box::new(Expr::Literal(Literal::I32(3), ((1, 12), (1, 12)))),
                ((1, 8), (1, 12)),
            ),
            Expr::Infix(
                Infix::Add,
                Box::new(Expr::Literal(Literal::I32(4), ((1, 15), (1, 15)))),
                Box::new(Expr::Literal(Literal::I32(5), ((1, 19), (1, 19)))),
                ((1, 15), (1, 19)),
            ),
        ],
        ret_t: Type::Unknown,
        span: ((1, 1), (1, 20)),
    });
    let actual = Parser::from(input).parse();

    assert_eq!(expected, actual[0]);
}

#[test]
fn test_parse_infer_type_from_var() {
    let input = "let x = 5; let y = x;";
    let expected = Stmt::Let(
        Ident(String::from("y")),
        I32,
        Expr::Ident(Ident::from("x"), I32, ((1, 20), (1, 20))),
    );
    let actual = Parser::from(input).parse();
    assert_eq!(expected, actual[1]);
}

#[test]
fn test_parse_infer_type_from_fn() {
    let input = "fn foo() -> u32 { return 0 }; let x = foo;";
    let expected = Stmt::Let(
        Ident(String::from("x")),
        Type::Fn(vec![], Box::new(U32)),
        Expr::Ident(
            Ident::from("foo"),
            Type::Fn(vec![], Box::new(U32)),
            ((1, 39), (1, 41)),
        ),
    );
    let actual = Parser::from(input).parse();
    assert_eq!(expected, actual[1]);
}

#[test]
fn test_parse_infer_type_from_call() {
    let input = "fn foo() -> u32 { return 0 }; let x = foo();";
    let expected = Stmt::Let(
        Ident(String::from("x")),
        U32,
        Expr::Call {
            name: String::from("foo"),
            args: vec![],
            ret_t: U32,
            span: ((1, 39), (1, 43)),
        },
    );
    let actual = Parser::from(input).parse();
    assert_eq!(expected, actual[1]);
}
