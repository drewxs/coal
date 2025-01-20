use crate::{F64, I32, I64, U32, U64};

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
            "let foo: f64 = 0;",
            Stmt::Let(
                Ident(String::from("foo")),
                F64,
                Expr::Literal(Literal::F64(0.0), ((1, 16), (1, 16))),
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
        let x = 0;
        x = 1;
        x += 1;
        "#;
    let expected = vec![
        Stmt::Let(
            Ident::from("x"),
            I32,
            Expr::Literal(Literal::I32(0), ((1, 9), (1, 9))),
        ),
        Stmt::Assign(
            Expr::Ident(Ident::from("x"), I32, ((2, 1), (2, 1))),
            Expr::Literal(Literal::I32(1), ((2, 5), (2, 5))),
        ),
        Stmt::OpAssign(
            Infix::Add,
            Expr::Ident(Ident::from("x"), I32, ((3, 1), (3, 1))),
            Expr::Literal(Literal::I32(1), ((3, 6), (3, 6))),
        ),
    ];
    assert_eq!(expected, Parser::from(input).parse());

    let input = r#"
        let y = "";
        y = "foo";
        "#;
    let expected = vec![
        Stmt::Let(
            Ident::from("y"),
            Type::Str,
            Expr::Literal(Literal::Str(String::from("")), ((1, 9), (1, 10))),
        ),
        Stmt::Assign(
            Expr::Ident(Ident::from("y"), Type::Str, ((2, 1), (2, 1))),
            Expr::Literal(Literal::from("foo"), ((2, 5), (2, 9))),
        ),
    ];
    assert_eq!(expected, Parser::from(input).parse());

    let input = r#"
        let z = false;
        z = true;
        "#;
    let expected = vec![
        Stmt::Let(
            Ident::from("z"),
            Type::Bool,
            Expr::Literal(Literal::Bool(false), ((1, 9), (1, 13))),
        ),
        Stmt::Assign(
            Expr::Ident(Ident::from("z"), Type::Bool, ((2, 1), (2, 1))),
            Expr::Literal(Literal::Bool(true), ((2, 5), (2, 8))),
        ),
    ];
    assert_eq!(expected, Parser::from(input).parse());

    let input = r#"
        let x = [1, 2, 3];
        x[1] = 2;
        "#;
    let expected = Stmt::Assign(
        Expr::Index(
            Box::new(Expr::Ident(
                Ident::from("x"),
                Type::List(Box::new(I32)),
                ((2, 1), (2, 1)),
            )),
            Box::new(Expr::Literal(Literal::I32(1), ((2, 3), (2, 3)))),
            ((2, 1), (2, 4)),
        ),
        Expr::Literal(Literal::I32(2), ((2, 8), (2, 8))),
    );
    let actual = Parser::from(input).parse();
    assert_eq!(expected, actual[1]);

    let input = r#"
        let x = [1, 2, 3];
        x[0] = true;
        "#;
    let mut parser = Parser::from(input);
    parser.parse();
    if parser.errors.len() != 1 {
        panic!("expected 1 error, got:\n{:?}", parser.errors);
    }
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

    assert_eq!(expected, Parser::from(input).parse());
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

    assert_eq!(expected, Parser::from(input).parse());
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

    assert_eq!(expected, Parser::from(input).parse());
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
            "-1 * 2",
            Stmt::Expr(Expr::Infix(
                Infix::Mul,
                Box::new(Expr::Prefix(
                    Prefix::Minus,
                    Box::new(Expr::Literal(Literal::I32(1), ((1, 2), (1, 2)))),
                    ((1, 1), (1, 2)),
                )),
                Box::new(Expr::Literal(Literal::I32(2), ((1, 6), (1, 6)))),
                ((1, 1), (1, 6)),
            )),
        ),
        (
            "!-1",
            Stmt::Expr(Expr::Prefix(
                Prefix::Not,
                Box::new(Expr::Prefix(
                    Prefix::Minus,
                    Box::new(Expr::Literal(Literal::I32(1), ((1, 3), (1, 3)))),
                    ((1, 2), (1, 3)),
                )),
                ((1, 1), (1, 3)),
            )),
        ),
        (
            "1 * 2 * 3",
            Stmt::Expr(Expr::Infix(
                Infix::Mul,
                Box::new(Expr::Infix(
                    Infix::Mul,
                    Box::new(Expr::Literal(Literal::I32(1), ((1, 1), (1, 1)))),
                    Box::new(Expr::Literal(Literal::I32(2), ((1, 5), (1, 5)))),
                    ((1, 1), (1, 5)),
                )),
                Box::new(Expr::Literal(Literal::I32(3), ((1, 9), (1, 9)))),
                ((1, 1), (1, 9)),
            )),
        ),
        (
            "1 * 2 / 3",
            Stmt::Expr(Expr::Infix(
                Infix::Div,
                Box::new(Expr::Infix(
                    Infix::Mul,
                    Box::new(Expr::Literal(Literal::I32(1), ((1, 1), (1, 1)))),
                    Box::new(Expr::Literal(Literal::I32(2), ((1, 5), (1, 5)))),
                    ((1, 1), (1, 5)),
                )),
                Box::new(Expr::Literal(Literal::I32(3), ((1, 9), (1, 9)))),
                ((1, 1), (1, 9)),
            )),
        ),
        (
            "1 + 2 * 3 + 4 / 5 - 6",
            Stmt::Expr(Expr::Infix(
                Infix::Sub,
                Box::new(Expr::Infix(
                    Infix::Add,
                    Box::new(Expr::Infix(
                        Infix::Add,
                        Box::new(Expr::Literal(Literal::I32(1), ((1, 1), (1, 1)))),
                        Box::new(Expr::Infix(
                            Infix::Mul,
                            Box::new(Expr::Literal(Literal::I32(2), ((1, 5), (1, 5)))),
                            Box::new(Expr::Literal(Literal::I32(3), ((1, 9), (1, 9)))),
                            ((1, 5), (1, 9)),
                        )),
                        ((1, 1), (1, 9)),
                    )),
                    Box::new(Expr::Infix(
                        Infix::Div,
                        Box::new(Expr::Literal(Literal::I32(4), ((1, 13), (1, 13)))),
                        Box::new(Expr::Literal(Literal::I32(5), ((1, 17), (1, 17)))),
                        ((1, 13), (1, 17)),
                    )),
                    ((1, 1), (1, 17)),
                )),
                Box::new(Expr::Literal(Literal::I32(6), ((1, 21), (1, 21)))),
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
        if actual.is_empty() {
            panic!("input:\n{}", input);
        }
        if expected != actual[0] {
            panic!(
                "input:\n{}\n\nexpected:\n{:?}\n\nactual:\n{:?}\n\n",
                input, expected, actual,
            );
        }
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
                ret_t: I32,
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

    let tests = vec![
        "fn f() -> i32 {}",
        "fn f() -> i32 { if true { return 0; } }",
        "fn f() -> i32 { if true { return 0; } else { print(0); } }",
        "fn f() -> i32 { if true { let x = 0; } else { print(0); } }",
        "fn f() -> i32 { if true { let x = 0; } else { return 0; } }",
        "fn f() -> i32 { return 1; if true { return 2; } else { return 3; } }",
    ];

    for input in tests {
        let mut parser = Parser::from(input);
        parser.parse();

        if parser.errors.is_empty() && parser.warnings.is_empty() {
            panic!("expected invalid:\n{}", input);
        }
    }
}

#[test]
fn test_parse_closure_expressions() {
    let tests = vec![
        (
            "|| {}",
            Stmt::Expr(Expr::Closure {
                args: vec![],
                body: vec![],
                ret_t: Type::Void,
                span: ((1, 1), (1, 5)),
            }),
        ),
        (
            "|x: i32| {}",
            Stmt::Expr(Expr::Closure {
                args: vec![Var::new("x", I32)],
                body: vec![],
                ret_t: Type::Void,
                span: ((1, 1), (1, 11)),
            }),
        ),
        (
            "|| { return 0; }",
            Stmt::Expr(Expr::Closure {
                args: vec![],
                body: vec![Stmt::Return(Expr::Literal(
                    Literal::I32(0),
                    ((1, 13), (1, 13)),
                ))],
                ret_t: I32,
                span: ((1, 1), (1, 16)),
            }),
        ),
        (
            "|x: i32| { return x; }",
            Stmt::Expr(Expr::Closure {
                args: vec![Var::new("x", I32)],
                body: vec![Stmt::Return(Expr::Ident(
                    Ident::from("x"),
                    I32,
                    ((1, 19), (1, 19)),
                ))],
                ret_t: I32,
                span: ((1, 1), (1, 22)),
            }),
        ),
        (
            r#"
            [0, 0].map(|| {
                return 1;
            });
            "#,
            Stmt::Expr(Expr::MethodCall {
                lhs: Box::new(Expr::Literal(
                    Literal::List(List::new(
                        &[
                            Expr::Literal(Literal::I32(0), ((1, 2), (1, 2))),
                            Expr::Literal(Literal::I32(0), ((1, 5), (1, 5))),
                        ],
                        I32,
                    )),
                    ((1, 1), (1, 6)),
                )),
                name: String::from("map"),
                args: vec![Expr::Closure {
                    args: vec![],
                    body: vec![Stmt::Return(Expr::Literal(
                        Literal::I32(1),
                        ((2, 8), (2, 8)),
                    ))],
                    ret_t: I32,
                    span: ((1, 12), (3, 1)),
                }],
                ret_t: Type::List(Box::new(Type::Unknown)),
                span: ((1, 1), (3, 2)),
            }),
        ),
        (
            r#"
            let x = [0, 0];
            x.map(|| {
                return 1;
            });
            "#,
            Stmt::Expr(Expr::MethodCall {
                lhs: Box::new(Expr::Ident(
                    Ident::from("x"),
                    Type::List(Box::new(I32)),
                    ((2, 1), (2, 1)),
                )),
                name: String::from("map"),
                args: vec![Expr::Closure {
                    args: vec![],
                    body: vec![Stmt::Return(Expr::Literal(
                        Literal::I32(1),
                        ((3, 8), (3, 8)),
                    ))],
                    ret_t: I32,
                    span: ((2, 7), (4, 1)),
                }],
                ret_t: Type::List(Box::new(Type::Unknown)),
                span: ((2, 1), (4, 2)),
            }),
        ),
    ];

    for (input, expected) in tests {
        let parsed = Parser::from(input).parse();
        let actual = parsed.last().unwrap().clone();

        if expected != actual {
            panic!(
                "input:\n{}\nexpected:\n{:?}\nactual:\n{:?}",
                input, expected, actual
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

#[test]
fn test_parse_infer_type_from_method() {
    let input = r#"let x = "foo".len()"#;
    let expected = Stmt::Let(
        Ident(String::from("x")),
        U64,
        Expr::MethodCall {
            lhs: Box::new(Expr::Literal(
                Literal::Str(String::from("foo")),
                ((1, 9), (1, 13)),
            )),
            name: String::from("len"),
            args: vec![],
            ret_t: U64,
            span: ((1, 9), (1, 19)),
        },
    );
    let actual = Parser::from(input).parse();

    assert_eq!(expected, actual[0]);
}

#[test]
fn test_parse_promote_infix_i32_u64() {
    let input = r#"
    fn foo() -> u64 {
        return 1;
    };
    let x = 2;
    let y = foo() + x;
    "#;
    let expected = Stmt::Let(
        Ident(String::from("y")),
        I64,
        Expr::Infix(
            Infix::Add,
            Box::new(Expr::Call {
                name: String::from("foo"),
                args: vec![],
                ret_t: U64,
                span: ((5, 9), (5, 13)),
            }),
            Box::new(Expr::Ident(Ident::from("x"), I32, ((5, 17), (5, 17)))),
            ((5, 9), (5, 17)),
        ),
    );
    let actual = Parser::from(input).parse();

    assert_eq!(expected, *actual.last().unwrap());
}

#[test]
fn test_parse_lists() {
    let tests = vec![
        (
            "[]",
            Stmt::Expr(Expr::Literal(
                Literal::List(List::default()),
                ((1, 1), (1, 2)),
            )),
        ),
        (
            "[1, 2]",
            Stmt::Expr(Expr::Literal(
                Literal::List(List::new(
                    &[
                        Expr::Literal(Literal::I32(1), ((1, 2), (1, 2))),
                        Expr::Literal(Literal::I32(2), ((1, 5), (1, 5))),
                    ],
                    I32,
                )),
                ((1, 1), (1, 6)),
            )),
        ),
        (
            "[foo()]",
            Stmt::Expr(Expr::Literal(
                Literal::List(List::new(
                    &[Expr::Call {
                        name: String::from("foo"),
                        args: vec![],
                        ret_t: Type::Unknown,
                        span: ((1, 2), (1, 6)),
                    }],
                    Type::Unknown,
                )),
                ((1, 1), (1, 7)),
            )),
        ),
        (
            r#"
            fn foo() -> str {
                return "asdf";
            }
            [foo()]
            "#,
            Stmt::Expr(Expr::Literal(
                Literal::List(List::new(
                    &[Expr::Call {
                        name: String::from("foo"),
                        args: vec![],
                        ret_t: Type::Str,
                        span: ((4, 2), (4, 6)),
                    }],
                    Type::Str,
                )),
                ((4, 1), (4, 7)),
            )),
        ),
        (
            "[1, 2, 3][1]",
            Stmt::Expr(Expr::Index(
                Box::new(Expr::Literal(
                    Literal::List(List::new(
                        &[
                            Expr::Literal(Literal::I32(1), ((1, 2), (1, 2))),
                            Expr::Literal(Literal::I32(2), ((1, 5), (1, 5))),
                            Expr::Literal(Literal::I32(3), ((1, 8), (1, 8))),
                        ],
                        I32,
                    )),
                    ((1, 1), (1, 9)),
                )),
                Box::new(Expr::Literal(Literal::I32(1), ((1, 11), (1, 11)))),
                ((1, 1), (1, 12)),
            )),
        ),
        (
            "let x = [1, 2, 3]; [x[1], 1]",
            Stmt::Expr(Expr::Literal(
                Literal::List(List::new(
                    &[
                        Expr::Index(
                            Box::new(Expr::Ident(
                                Ident::from("x"),
                                Type::List(Box::new(I32)),
                                ((1, 21), (1, 21)),
                            )),
                            Box::new(Expr::Literal(Literal::I32(1), ((1, 23), (1, 23)))),
                            ((1, 21), (1, 24)),
                        ),
                        Expr::Literal(Literal::I32(1), ((1, 27), (1, 27))),
                    ],
                    I32,
                )),
                ((1, 20), (1, 28)),
            )),
        ),
        (
            "let x: list[i32] = []",
            Stmt::Let(
                Ident(String::from("x")),
                Type::List(Box::new(I32)),
                Expr::Literal(Literal::List(List::new(&[], I32)), ((1, 20), (1, 21))),
            ),
        ),
    ];

    for (input, expected) in tests {
        let mut parser = Parser::from(input);
        let parsed = parser.parse();
        if let Err(errors) = parser.check() {
            println!("input:\n{}", input);
            for e in errors {
                println!("{e}");
            }
        }

        let actual = parsed.last().unwrap();
        if expected != *actual {
            panic!(
                "input:\n{}\n\nexpected:\n{}{:?}\n\nactual:\n{}{:?}\n",
                input, expected, expected, actual, actual
            );
        }
    }

    let tests = vec![r#"[1, "2"]"#, r#"["1", 2]"#, r#"[1, 2.0]"#];

    for input in tests {
        let mut parser = Parser::from(input);
        let actual = parser.parse();

        if parser.errors.is_empty() || !actual.is_empty() {
            panic!("expected invalid:\n{}", input);
        }
    }
}

#[test]
fn test_parse_maps() {
    let tests = vec![
        (
            "{}",
            Stmt::Expr(Expr::Literal(
                Literal::Map(Map::default()),
                ((1, 1), (1, 2)),
            )),
        ),
        (
            "{1: 2}",
            Stmt::Expr(Expr::Literal(
                Literal::Map(Map::new(
                    &[(
                        Expr::Literal(Literal::I32(1), ((1, 2), (1, 2))),
                        Expr::Literal(Literal::I32(2), ((1, 5), (1, 5))),
                    )],
                    (I32, I32),
                )),
                ((1, 1), (1, 6)),
            )),
        ),
        (
            r#"{"one": 1, "two": 2}"#,
            Stmt::Expr(Expr::Literal(
                Literal::Map(Map::new(
                    &[
                        (
                            Expr::Literal(Literal::Str(String::from("one")), ((1, 2), (1, 6))),
                            Expr::Literal(Literal::I32(1), ((1, 9), (1, 9))),
                        ),
                        (
                            Expr::Literal(Literal::Str(String::from("two")), ((1, 12), (1, 16))),
                            Expr::Literal(Literal::I32(2), ((1, 19), (1, 19))),
                        ),
                    ],
                    (Type::Str, I32),
                )),
                ((1, 1), (1, 20)),
            )),
        ),
    ];

    for (input, expected) in tests {
        let mut parser = Parser::from(input);
        let parsed = parser.parse();
        if let Err(errors) = parser.check() {
            println!("input:\n{}", input);
            for e in errors {
                println!("{e}");
            }
        }

        let actual = parsed.last().unwrap();
        if expected != *actual {
            panic!(
                "input:\n{}\n\nexpected:\n{}{:?}\n\nactual:\n{}{:?}\n",
                input, expected, expected, actual, actual
            );
        }
    }
}

#[test]
fn test_parse_iter() {
    let tests = vec![
        (
            "for i in 0..10 {}",
            Stmt::Expr(Expr::Iter {
                ident: Ident::from("i"),
                expr: Box::new(Expr::Range(
                    Box::new(Expr::Literal(Literal::I32(0), ((1, 10), (1, 10)))),
                    Box::new(Expr::Literal(Literal::I32(10), ((1, 13), (1, 14)))),
                    ((1, 10), (1, 14)),
                )),
                body: vec![],
                span: ((1, 1), (1, 17)),
            }),
        ),
        (
            "for i in 0..100 { let x = i; }",
            Stmt::Expr(Expr::Iter {
                ident: Ident::from("i"),
                expr: Box::new(Expr::Range(
                    Box::new(Expr::Literal(Literal::I32(0), ((1, 10), (1, 10)))),
                    Box::new(Expr::Literal(Literal::I32(100), ((1, 13), (1, 15)))),
                    ((1, 10), (1, 15)),
                )),
                body: vec![Stmt::Let(
                    Ident(String::from("x")),
                    U64,
                    Expr::Ident(Ident::from("i"), U64, ((1, 27), (1, 27))),
                )],
                span: ((1, 1), (1, 30)),
            }),
        ),
        (
            r#"
            let list = [1, 2, 3];
            for item in list {
                let x = item;
            }
            "#,
            Stmt::Expr(Expr::Iter {
                ident: Ident::from("item"),
                expr: Box::new(Expr::Ident(
                    Ident::from("list"),
                    Type::List(Box::new(I32)),
                    ((2, 13), (2, 16)),
                )),
                body: vec![Stmt::Let(
                    Ident(String::from("x")),
                    I32,
                    Expr::Ident(Ident::from("item"), I32, ((3, 9), (3, 12))),
                )],
                span: ((2, 1), (4, 1)),
            }),
        ),
        (
            r#"
            let list: list[i64] = [1, 2, 3];
            for item in list {
                let x = item;
            }
            "#,
            Stmt::Expr(Expr::Iter {
                ident: Ident::from("item"),
                expr: Box::new(Expr::Ident(
                    Ident::from("list"),
                    Type::List(Box::new(I64)),
                    ((2, 13), (2, 16)),
                )),
                body: vec![Stmt::Let(
                    Ident(String::from("x")),
                    I64,
                    Expr::Ident(Ident::from("item"), I64, ((3, 9), (3, 12))),
                )],
                span: ((2, 1), (4, 1)),
            }),
        ),
    ];

    for (input, expected) in tests {
        let mut parser = Parser::from(input);
        let parsed = parser.parse();
        if let Err(errors) = parser.check() {
            println!("input:\n{}", input);
            for e in errors {
                println!("{e}");
            }
        }

        let actual = parsed.last().unwrap();
        if expected != *actual {
            panic!(
                "input:\n{}\n\nexpected:\n{}{:?}\n\nactual:\n{}{:?}\n",
                input, expected, expected, actual, actual
            );
        }
    }
}
