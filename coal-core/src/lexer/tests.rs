use super::*;

#[test]
fn test_lex_whitespace() {
    let input = r#"

        1;


        2;
        3

        "hello"
        "#;

    let mut lexer = Lexer::new(input);

    let expected = vec![
        LexicalToken::new(Token::Int(1), ((1, 1), (1, 1))),
        LexicalToken::new(Token::Semicolon, ((1, 2), (1, 2))),
        LexicalToken::new(Token::NewLine, ((2, 1), (2, 1))),
        LexicalToken::new(Token::Int(2), ((3, 1), (3, 1))),
        LexicalToken::new(Token::Semicolon, ((3, 2), (3, 2))),
        LexicalToken::new(Token::Int(3), ((4, 1), (4, 1))),
        LexicalToken::new(Token::NewLine, ((5, 1), (5, 1))),
        LexicalToken::new(Token::Str(String::from("hello")), ((6, 1), (6, 7))),
    ];

    assert_eq!(expected, lexer.lexical_tokens());
}

#[test]
fn test_lex_string() {
    let input = r#""hello""#;
    let expected = LexicalToken::new(Token::Str(String::from("hello")), ((1, 1), (1, 7)));
    let actual = Lexer::new(input).lexical_tokens();

    assert_eq!(expected, actual[0]);
}

#[test]
fn test_lex_comments() {
    let input = r#"
        // foo

        let one = 1;
        // bar
        2;
        "#;

    let mut lexer = Lexer::new(input);

    let expected = vec![
        Token::Comment(String::from("foo")),
        Token::NewLine,
        Token::Let,
        Token::Ident(String::from("one")),
        Token::Assign,
        Token::Int(1),
        Token::Semicolon,
        Token::Comment(String::from("bar")),
        Token::Int(2),
        Token::Semicolon,
    ];

    assert_eq!(expected, lexer.tokens());
}

#[test]
fn test_next_tok() {
    let input = r#"
        // foo

        let five: int = 5;
        let ten: int = 10;

        fn add(x: int, y: int) -> int {
            x + y
        }

        let result = add(five, ten);
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
        Token::Comment(String::from("foo")),
        Token::NewLine,
        Token::Let,
        Token::Ident(String::from("five")),
        Token::Colon,
        Token::Ident(String::from("int")),
        Token::Assign,
        Token::Int(5),
        Token::Semicolon,
        Token::Let,
        Token::Ident(String::from("ten")),
        Token::Colon,
        Token::Ident(String::from("int")),
        Token::Assign,
        Token::Int(10),
        Token::Semicolon,
        Token::NewLine,
        Token::Fn,
        Token::Ident(String::from("add")),
        Token::Lparen,
        Token::Ident(String::from("x")),
        Token::Colon,
        Token::Ident(String::from("int")),
        Token::Comma,
        Token::Ident(String::from("y")),
        Token::Colon,
        Token::Ident(String::from("int")),
        Token::Rparen,
        Token::Arrow,
        Token::Ident(String::from("int")),
        Token::Lbrace,
        Token::Ident(String::from("x")),
        Token::Plus,
        Token::Ident(String::from("y")),
        Token::Rbrace,
        Token::NewLine,
        Token::Let,
        Token::Ident(String::from("result")),
        Token::Assign,
        Token::Ident(String::from("add")),
        Token::Lparen,
        Token::Ident(String::from("five")),
        Token::Comma,
        Token::Ident(String::from("ten")),
        Token::Rparen,
        Token::Semicolon,
        Token::Bang,
        Token::Minus,
        Token::Slash,
        Token::Asterisk,
        Token::Int(5),
        Token::Semicolon,
        Token::Int(5),
        Token::LT,
        Token::Int(10),
        Token::GT,
        Token::Int(5),
        Token::Semicolon,
        Token::NewLine,
        Token::If,
        Token::Int(5),
        Token::LT,
        Token::Int(10),
        Token::Lbrace,
        Token::Return,
        Token::Int(1),
        Token::Semicolon,
        Token::Rbrace,
        Token::Elif,
        Token::Int(4),
        Token::LT,
        Token::Int(5),
        Token::Lbrace,
        Token::Return,
        Token::Bool(true),
        Token::Semicolon,
        Token::Rbrace,
        Token::Else,
        Token::Lbrace,
        Token::Return,
        Token::Bool(false),
        Token::Semicolon,
        Token::Rbrace,
        Token::NewLine,
        Token::Int(10),
        Token::EQ,
        Token::Int(10),
        Token::Semicolon,
        Token::Int(10),
        Token::NEQ,
        Token::Int(9),
        Token::Semicolon,
        Token::EOF,
    ];

    for (i, expected) in expected.iter().enumerate() {
        if let Some(LexicalToken { token, .. }) = lexer.next() {
            println!("[{i}] expected: {expected:?}, actual: {token:?}");
            assert_eq!(*expected, token);
        }
    }
}
