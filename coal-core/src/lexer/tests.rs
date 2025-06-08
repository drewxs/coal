use super::*;

#[test]
fn test_lex_whitespace() {
    let input = r#"

        1;


        2;
        3

        "hello"
        "#;

    let lexer = Lexer::new(input);

    let expected = vec![
        Token::new(TokenKind::I32(1), ((1, 1), (1, 1))),
        Token::new(TokenKind::Semicolon, ((1, 2), (1, 2))),
        Token::new(TokenKind::NewLine, ((2, 1), (2, 1))),
        Token::new(TokenKind::I32(2), ((3, 1), (3, 1))),
        Token::new(TokenKind::Semicolon, ((3, 2), (3, 2))),
        Token::new(TokenKind::I32(3), ((4, 1), (4, 1))),
        Token::new(TokenKind::NewLine, ((5, 1), (5, 1))),
        Token::new(TokenKind::Str(String::from("hello")), ((6, 1), (6, 7))),
    ];
    let actual = lexer.collect::<Vec<Token>>();

    assert_eq!(expected, actual);
}

#[test]
fn test_lex_comments() {
    let input = r#"
        // foo

        let one = 1;
        // bar
        2;
        "#;

    let lexer = Lexer::new(input);

    let expected = vec![
        TokenKind::Comment(String::from("foo")),
        TokenKind::NewLine,
        TokenKind::Let,
        TokenKind::Ident(String::from("one")),
        TokenKind::Assign,
        TokenKind::I32(1),
        TokenKind::Semicolon,
        TokenKind::Comment(String::from("bar")),
        TokenKind::I32(2),
        TokenKind::Semicolon,
    ];
    let actual: Vec<TokenKind> = lexer.map(|node| node.kind).collect();

    assert_eq!(expected, actual);
}

#[test]
fn test_lex_string() {
    let input = r#""hello""#;
    let expected = Token::new(TokenKind::Str(String::from("hello")), ((1, 1), (1, 7)));
    let mut lexer = Lexer::new(input);

    assert_eq!(expected, lexer.next().unwrap());
}

#[test]
fn test_lex_let_statements() {
    let input = r#"
        let five: i32 = 5;
        let ten: i64 = 10;
        "#;

    let mut lexer = Lexer::new(input);

    let expected = vec![
        TokenKind::Let,
        TokenKind::Ident(String::from("five")),
        TokenKind::Colon,
        TokenKind::Ident(String::from("i32")),
        TokenKind::Assign,
        TokenKind::I32(5),
        TokenKind::Semicolon,
        TokenKind::Let,
        TokenKind::Ident(String::from("ten")),
        TokenKind::Colon,
        TokenKind::Ident(String::from("i64")),
        TokenKind::Assign,
        TokenKind::I32(10),
        TokenKind::Semicolon,
        TokenKind::EOF,
    ];

    for (i, expected) in expected.iter().enumerate() {
        if let Some(Token { kind: token, .. }) = lexer.next() {
            println!("[{i}] expected: {expected:?}, actual: {token:?}");
            assert_eq!(*expected, token);
        }
    }
}

#[test]
fn test_lex_functions() {
    let input = r#"
        fn add(x: i64, y: i64) -> i64 {
            x + y
        }
        let result = add(five, ten);
        "#;

    let mut lexer = Lexer::new(input);

    let expected = vec![
        TokenKind::Fn,
        TokenKind::Ident(String::from("add")),
        TokenKind::Lparen,
        TokenKind::Ident(String::from("x")),
        TokenKind::Colon,
        TokenKind::Ident(String::from("i64")),
        TokenKind::Comma,
        TokenKind::Ident(String::from("y")),
        TokenKind::Colon,
        TokenKind::Ident(String::from("i64")),
        TokenKind::Rparen,
        TokenKind::Arrow,
        TokenKind::Ident(String::from("i64")),
        TokenKind::Lbrace,
        TokenKind::Ident(String::from("x")),
        TokenKind::Add,
        TokenKind::Ident(String::from("y")),
        TokenKind::Rbrace,
        TokenKind::Let,
        TokenKind::Ident(String::from("result")),
        TokenKind::Assign,
        TokenKind::Ident(String::from("add")),
        TokenKind::Lparen,
        TokenKind::Ident(String::from("five")),
        TokenKind::Comma,
        TokenKind::Ident(String::from("ten")),
        TokenKind::Rparen,
        TokenKind::Semicolon,
        TokenKind::EOF,
    ];

    for (i, expected) in expected.iter().enumerate() {
        if let Some(Token { kind: token, .. }) = lexer.next() {
            println!("[{i}] expected: {expected:?}, actual: {token:?}");
            assert_eq!(*expected, token);
        }
    }
}

#[test]
fn test_lex_operators() {
    let input = r#"
        !-/*5;
        5 < 10 > 5;

        if 5 < 10 {
            return 1;
        } elif 4 < 5 {
            return true;
        } else {
            return false;
        }

        10 == 10;
        10 != 9;
        "#;

    let mut lexer = Lexer::new(input);

    let expected = vec![
        TokenKind::Bang,
        TokenKind::Sub,
        TokenKind::Div,
        TokenKind::Mul,
        TokenKind::I32(5),
        TokenKind::Semicolon,
        TokenKind::I32(5),
        TokenKind::LT,
        TokenKind::I32(10),
        TokenKind::GT,
        TokenKind::I32(5),
        TokenKind::Semicolon,
        TokenKind::NewLine,
        TokenKind::If,
        TokenKind::I32(5),
        TokenKind::LT,
        TokenKind::I32(10),
        TokenKind::Lbrace,
        TokenKind::Return,
        TokenKind::I32(1),
        TokenKind::Semicolon,
        TokenKind::Rbrace,
        TokenKind::Elif,
        TokenKind::I32(4),
        TokenKind::LT,
        TokenKind::I32(5),
        TokenKind::Lbrace,
        TokenKind::Return,
        TokenKind::Bool(true),
        TokenKind::Semicolon,
        TokenKind::Rbrace,
        TokenKind::Else,
        TokenKind::Lbrace,
        TokenKind::Return,
        TokenKind::Bool(false),
        TokenKind::Semicolon,
        TokenKind::Rbrace,
        TokenKind::NewLine,
        TokenKind::I32(10),
        TokenKind::EQ,
        TokenKind::I32(10),
        TokenKind::Semicolon,
        TokenKind::I32(10),
        TokenKind::NEQ,
        TokenKind::I32(9),
        TokenKind::Semicolon,
        TokenKind::EOF,
    ];

    for (i, expected) in expected.iter().enumerate() {
        if let Some(Token { kind: token, .. }) = lexer.next() {
            println!("[{i}] expected: {expected:?}, actual: {token:?}");
            assert_eq!(*expected, token);
        }
    }
}

#[test]
fn test_lex_lists() {
    let input = r#"
        [1, 2]
        "#;

    let mut lexer = Lexer::new(input);

    let expected = vec![
        TokenKind::Lbracket,
        TokenKind::I32(1),
        TokenKind::Comma,
        TokenKind::I32(2),
        TokenKind::Rbracket,
        TokenKind::EOF,
    ];

    for (i, expected) in expected.iter().enumerate() {
        if let Some(Token { kind: token, .. }) = lexer.next() {
            println!("[{i}] expected: {expected:?}, actual: {token:?}");
            assert_eq!(*expected, token);
        }
    }
}

#[test]
fn test_lex_iter() {
    let input = r#"for i in 0..100"#;

    let mut lexer = Lexer::new(input);

    let expected = vec![
        TokenKind::For,
        TokenKind::Ident(String::from("i")),
        TokenKind::In,
        TokenKind::I32(0),
        TokenKind::Range,
        TokenKind::I32(100),
        TokenKind::EOF,
    ];

    for (i, expected) in expected.iter().enumerate() {
        if let Some(Token { kind: token, .. }) = lexer.next() {
            println!("[{i}] expected: {expected:?}, actual: {token:?}");
            assert_eq!(*expected, token);
        }
    }
}

#[test]
fn test_lex_hashmap() {
    let input = r#"{key: "value"}"#;

    let mut lexer = Lexer::new(input);

    let expected = vec![
        TokenKind::Lbrace,
        TokenKind::Ident(String::from("key")),
        TokenKind::Colon,
        TokenKind::Str(String::from("value")),
        TokenKind::Rbrace,
        TokenKind::EOF,
    ];

    for (i, expected) in expected.iter().enumerate() {
        if let Some(Token { kind: token, .. }) = lexer.next() {
            println!("[{i}] expected: {expected:?}, actual: {token:?}");
            assert_eq!(*expected, token);
        }
    }
}
