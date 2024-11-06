#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    Illegal,
    EOF,

    // Identifiers + literals
    Ident(String),
    Str(String),
    Int(i64),
    Float(f64),
    Bool(bool),

    // Operators
    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,
    Modulo,

    // Comparisons
    EQ,
    NEQ,
    LT,
    LTE,
    GT,
    GTE,

    // Delimiters
    Colon,
    Semicolon,
    Comma,
    Arrow,

    // Brackets
    Lparen,
    Rparen,
    Lbrace,
    Rbrace,
    Lbracket,
    Rbracket,

    // Keywords
    Function,
    Let,
    If,
    Elif,
    Else,
    Return,
}

impl From<&str> for Token {
    fn from(s: &str) -> Self {
        match s {
            "true" => Token::Bool(true),
            "false" => Token::Bool(false),
            "=" => Token::Assign,
            "+" => Token::Plus,
            "-" => Token::Minus,
            "!" => Token::Bang,
            "*" => Token::Asterisk,
            "/" => Token::Slash,
            "%" => Token::Modulo,
            "==" => Token::EQ,
            "!=" => Token::NEQ,
            "<" => Token::LT,
            "<=" => Token::LTE,
            ">" => Token::GT,
            ">=" => Token::GTE,
            ":" => Token::Colon,
            ";" => Token::Semicolon,
            "," => Token::Comma,
            "->" => Token::Arrow,
            "(" => Token::Lparen,
            ")" => Token::Rparen,
            "{" => Token::Lbrace,
            "}" => Token::Rbrace,
            "[" => Token::Lbracket,
            "]" => Token::Rbracket,
            "fn" => Token::Function,
            "let" => Token::Let,
            "if" => Token::If,
            "elif" => Token::Elif,
            "else" => Token::Else,
            "return" => Token::Return,
            _ => Token::Ident(s.to_string()),
        }
    }
}
