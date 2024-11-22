use std::fmt;

#[derive(Clone, Debug, PartialEq, Default)]
pub enum Token {
    #[default]
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
    DoubleSlash,
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
    Fn,
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
            "//" => Token::DoubleSlash,
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
            "fn" => Token::Fn,
            "let" => Token::Let,
            "if" => Token::If,
            "elif" => Token::Elif,
            "else" => Token::Else,
            "return" => Token::Return,
            _ => Token::Ident(s.to_string()),
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::Illegal => write!(f, "ILLEGAL"),
            Token::EOF => write!(f, "EOF"),
            Token::Ident(name) => write!(f, "{name}"),
            Token::Str(s) => write!(f, "{s}"),
            Token::Int(i) => write!(f, "{i}"),
            Token::Float(n) => write!(f, "{n}"),
            Token::Bool(b) => write!(f, "{b}"),
            Token::Assign => write!(f, "="),
            Token::Plus => write!(f, "+"),
            Token::Minus => write!(f, "-"),
            Token::Bang => write!(f, "!"),
            Token::Asterisk => write!(f, "*"),
            Token::Slash => write!(f, "/"),
            Token::DoubleSlash => write!(f, "//"),
            Token::Modulo => write!(f, "%"),
            Token::EQ => write!(f, "=="),
            Token::NEQ => write!(f, "!="),
            Token::LT => write!(f, "<"),
            Token::LTE => write!(f, "<="),
            Token::GT => write!(f, ">"),
            Token::GTE => write!(f, ">="),
            Token::Colon => write!(f, ":"),
            Token::Semicolon => write!(f, ";"),
            Token::Comma => write!(f, ","),
            Token::Arrow => write!(f, "->"),
            Token::Lparen => write!(f, "("),
            Token::Rparen => write!(f, ")"),
            Token::Lbrace => write!(f, "{{"),
            Token::Rbrace => write!(f, "}}"),
            Token::Lbracket => write!(f, "["),
            Token::Rbracket => write!(f, "]"),
            Token::Fn => write!(f, "fn"),
            Token::Let => write!(f, "let"),
            Token::If => write!(f, "if"),
            Token::Elif => write!(f, "elif"),
            Token::Else => write!(f, "else"),
            Token::Return => write!(f, "return"),
        }
    }
}
