use super::*;

#[test]
fn test_let_statements() {
    let tests = vec![
        (
            "let x: int = 5;",
            Stmt::Let(
                Ident(String::from("x")),
                Type::Int,
                Expr::Literal(Literal::Int(5)),
            ),
        ),
        (
            "let y: int = 10;",
            Stmt::Let(
                Ident(String::from("y")),
                Type::Int,
                Expr::Literal(Literal::Int(10)),
            ),
        ),
        (
            "let z: int = 99999;",
            Stmt::Let(
                Ident(String::from("z")),
                Type::Int,
                Expr::Literal(Literal::Int(99999)),
            ),
        ),
        (
            "let foo: Foo = 0;",
            Stmt::Let(
                Ident(String::from("foo")),
                Type::UserDefined(String::from("Foo")),
                Expr::Literal(Literal::Int(0)),
            ),
        ),
    ];

    for (input, expected) in tests {
        assert_eq!(expected, Program::parse(input).statements[0]);
    }
}

#[test]
fn test_let_statements_inference() {
    let input = r#"
        let x = 5;
        let y = 5.0;
        let z = "hello";"#;
    let program = Program::parse(input);
    let expected = vec![
        Stmt::Let(
            Ident(String::from("x")),
            Type::Int,
            Expr::Literal(Literal::Int(5)),
        ),
        Stmt::Let(
            Ident(String::from("y")),
            Type::Float,
            Expr::Literal(Literal::Float(5.0)),
        ),
        Stmt::Let(
            Ident(String::from("z")),
            Type::String,
            Expr::Literal(Literal::Str(String::from("hello"))),
        ),
    ];

    assert_eq!(expected, program.statements);
}

#[test]
fn test_return_statements() {
    let tests = vec!["return 7;", "return 100;", "return 999999;"];

    for input in tests {
        let stmt = &Program::parse(input).statements[0];
        if !matches!(stmt, Stmt::Return(_)) {
            panic!("[{input}] expected=Stmt::Return, got={stmt:?}");
        }
    }
}

#[test]
fn test_identifier_expressions() {
    let input = "foo; bar; foobar;";
    let program = Program::parse(input);
    let expected = vec![
        Stmt::Expr(Expr::Ident(Ident::from("foo"))),
        Stmt::Expr(Expr::Ident(Ident::from("bar"))),
        Stmt::Expr(Expr::Ident(Ident::from("foobar"))),
    ];

    assert_eq!(expected, program.statements);
}

#[test]
fn test_literal_expressions() {
    let input = r#"5; 10.0; false; "foo";"#;
    let program = Program::parse(input);
    let expected = vec![
        Stmt::Expr(Expr::Literal(Literal::Int(5))),
        Stmt::Expr(Expr::Literal(Literal::Float(10.0))),
        Stmt::Expr(Expr::Literal(Literal::Bool(false))),
        Stmt::Expr(Expr::Literal(Literal::Str(String::from("foo")))),
    ];

    assert_eq!(expected, program.statements);
}

#[test]
fn test_prefix_expressions() {
    let input = "!5; -5; !true; !false;";
    let program = Program::parse(input);
    let expected = vec![
        Stmt::Expr(Expr::Prefix(
            Prefix::Not,
            Box::new(Expr::Literal(Literal::Int(5))),
        )),
        Stmt::Expr(Expr::Prefix(
            Prefix::Minus,
            Box::new(Expr::Literal(Literal::Int(5))),
        )),
        Stmt::Expr(Expr::Prefix(
            Prefix::Not,
            Box::new(Expr::Literal(Literal::Bool(true))),
        )),
        Stmt::Expr(Expr::Prefix(
            Prefix::Not,
            Box::new(Expr::Literal(Literal::Bool(false))),
        )),
    ];

    assert_eq!(expected, program.statements);
}

#[test]
fn test_infix_expressions() {
    let tests = vec![
        (
            "3 + 2",
            Stmt::Expr(Expr::Infix(
                Infix::Plus,
                Box::new(Expr::Literal(Literal::Int(3))),
                Box::new(Expr::Literal(Literal::Int(2))),
            )),
        ),
        (
            "5 - 2",
            Stmt::Expr(Expr::Infix(
                Infix::Minus,
                Box::new(Expr::Literal(Literal::Int(5))),
                Box::new(Expr::Literal(Literal::Int(2))),
            )),
        ),
        (
            "3 * 2",
            Stmt::Expr(Expr::Infix(
                Infix::Mul,
                Box::new(Expr::Literal(Literal::Int(3))),
                Box::new(Expr::Literal(Literal::Int(2))),
            )),
        ),
        (
            "6 / 2",
            Stmt::Expr(Expr::Infix(
                Infix::Div,
                Box::new(Expr::Literal(Literal::Int(6))),
                Box::new(Expr::Literal(Literal::Int(2))),
            )),
        ),
        (
            "7 % 2",
            Stmt::Expr(Expr::Infix(
                Infix::Mod,
                Box::new(Expr::Literal(Literal::Int(7))),
                Box::new(Expr::Literal(Literal::Int(2))),
            )),
        ),
        (
            "3 > 2",
            Stmt::Expr(Expr::Infix(
                Infix::GT,
                Box::new(Expr::Literal(Literal::Int(3))),
                Box::new(Expr::Literal(Literal::Int(2))),
            )),
        ),
        (
            "3 < 2",
            Stmt::Expr(Expr::Infix(
                Infix::LT,
                Box::new(Expr::Literal(Literal::Int(3))),
                Box::new(Expr::Literal(Literal::Int(2))),
            )),
        ),
        (
            "4 >= 2",
            Stmt::Expr(Expr::Infix(
                Infix::GTE,
                Box::new(Expr::Literal(Literal::Int(4))),
                Box::new(Expr::Literal(Literal::Int(2))),
            )),
        ),
        (
            "4 <= 2",
            Stmt::Expr(Expr::Infix(
                Infix::LTE,
                Box::new(Expr::Literal(Literal::Int(4))),
                Box::new(Expr::Literal(Literal::Int(2))),
            )),
        ),
        (
            "4 == 4",
            Stmt::Expr(Expr::Infix(
                Infix::EQ,
                Box::new(Expr::Literal(Literal::Int(4))),
                Box::new(Expr::Literal(Literal::Int(4))),
            )),
        ),
        (
            "4 != 4",
            Stmt::Expr(Expr::Infix(
                Infix::NEQ,
                Box::new(Expr::Literal(Literal::Int(4))),
                Box::new(Expr::Literal(Literal::Int(4))),
            )),
        ),
        (
            "true == true",
            Stmt::Expr(Expr::Infix(
                Infix::EQ,
                Box::new(Expr::Literal(Literal::Bool(true))),
                Box::new(Expr::Literal(Literal::Bool(true))),
            )),
        ),
        (
            "false == false",
            Stmt::Expr(Expr::Infix(
                Infix::EQ,
                Box::new(Expr::Literal(Literal::Bool(false))),
                Box::new(Expr::Literal(Literal::Bool(false))),
            )),
        ),
        (
            "true != false",
            Stmt::Expr(Expr::Infix(
                Infix::NEQ,
                Box::new(Expr::Literal(Literal::Bool(true))),
                Box::new(Expr::Literal(Literal::Bool(false))),
            )),
        ),
    ];

    for (input, expected) in tests {
        assert_eq!(expected, Program::parse(input).statements[0]);
    }
}

#[test]
fn test_operator_precedence() {
    let tests = vec![
        ("-a * b", "((-a) * b)"),
        ("!-a", "(!(-a))"),
        ("a + b + c", "((a + b) + c)"),
        ("a + b - c", "((a + b) - c)"),
        ("a * b * c", "((a * b) * c)"),
        ("a * b / c", "((a * b) / c)"),
        ("a + b / c", "(a + (b / c))"),
        ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
        ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
        ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
        (
            "3 + 4 * 5 == 3 * 1 + 4 * 5",
            "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
        ),
        ("true", "true"),
        ("false", "false"),
        ("1 < 2 == true", "((1 < 2) == true)"),
        ("1 > 2 == false", "((1 > 2) == false)"),
        ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
        ("(1 + 1) * 2", "((1 + 1) * 2)"),
        ("1 / (2 + 2)", "(1 / (2 + 2))"),
        ("-(1 + 2)", "(-(1 + 2))"),
        ("!(true == true)", "(!(true == true))"),
    ];

    for (input, expected) in tests {
        assert_eq!(expected, Program::read_line(input));
    }
}

#[test]
fn test_if_expression() {
    let input = "if x < y { return x }";
    let program = Program::parse(input);
    let expected = Stmt::Expr(Expr::If {
        cond: Box::new(Expr::Infix(
            Infix::LT,
            Box::new(Expr::Ident(Ident::from("x"))),
            Box::new(Expr::Ident(Ident::from("y"))),
        )),
        then: vec![Stmt::Return(Expr::Ident(Ident::from("x")))],
        elifs: vec![],
        alt: None,
    });

    assert_eq!(expected, program.statements[0]);
}

#[test]
fn test_nested_if_expression() {
    let input = r#"
        if x < y {
            if x > 1 {
                return x;
            }
        } else {
            return y;
        }"#;
    let program = Program::parse(input);
    let expected = Stmt::Expr(Expr::If {
        cond: Box::new(Expr::Infix(
            Infix::LT,
            Box::new(Expr::Ident(Ident::from("x"))),
            Box::new(Expr::Ident(Ident::from("y"))),
        )),
        then: vec![Stmt::Expr(Expr::If {
            cond: Box::new(Expr::Infix(
                Infix::GT,
                Box::new(Expr::Ident(Ident::from("x"))),
                Box::new(Expr::Literal(Literal::Int(1))),
            )),
            then: vec![Stmt::Return(Expr::Ident(Ident::from("x")))],
            elifs: vec![],
            alt: None,
        })],
        elifs: vec![],
        alt: Some(vec![Stmt::Return(Expr::Ident(Ident::from("y")))]),
    });

    assert_eq!(expected, program.statements[0]);
}

#[test]
fn test_elif_expression() {
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
    let expected = r#"if (x < y) {
    return x;
} elif (x > y) {
    return y;
} elif (x > 1) {
    return 1;
} else {
    return z;
}
"#;
    let program = Program::from(input);
    assert_eq!(expected, program.to_string());
}

#[test]
fn test_function_expressions() {
    let tests = [
        (
            r#"fn foo() -> int {
                   return 0;
               }"#,
            Stmt::Expr(Expr::Fn {
                name: String::from("foo"),
                args: vec![],
                ret_t: Type::Int,
                body: vec![Stmt::Return(Expr::Literal(Literal::Int(0)))],
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
                    Box::new(Expr::Ident(Ident::from("x"))),
                    Box::new(Expr::Ident(Ident::from("y"))),
                ))],
            }),
        ),
    ];

    for (input, expected) in tests {
        assert_eq!(expected, Program::parse(input).statements[0]);
    }
}

#[test]
fn test_call_expression() {
    let input = "add(1, 2 * 3, 4 + 5);";
    let program = Program::from(input);
    let expected = Stmt::Expr(Expr::Call {
        func: Box::new(Expr::Ident(Ident::from("add"))),
        args: vec![
            Expr::Literal(Literal::Int(1)),
            Expr::Infix(
                Infix::Mul,
                Box::new(Expr::Literal(Literal::Int(2))),
                Box::new(Expr::Literal(Literal::Int(3))),
            ),
            Expr::Infix(
                Infix::Plus,
                Box::new(Expr::Literal(Literal::Int(4))),
                Box::new(Expr::Literal(Literal::Int(5))),
            ),
        ],
    });

    assert_eq!(expected, program.statements[0]);
}
