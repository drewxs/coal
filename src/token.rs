use std::fmt::Display;

#[derive(Debug, PartialEq)]
pub enum Token {
    Illegal,
    EOF,

    // Identifiers + literals
    Ident(String),
    Int(i64),
    Str(String),
    Float(f64),
    Bool(bool),

    // Types
    IntType,
    FloatType,
    StrType,
    BoolType,

    // Operators
    Assign,
    Plus,

    // Delimiters
    Colon,
    Comma,
    Semicolon,

    Lparen,
    Rparen,
    Lbrace,
    Rbrace,

    // Keywords
    Function,
    Let,
}

pub fn lookup_ident(ident: &str) -> Token {
    match ident {
        "fn" => Token::Function,
        "let" => Token::Let,
        "true" => Token::Bool(true),
        "false" => Token::Bool(false),
        "int" => Token::IntType,
        "float" => Token::FloatType,
        "str" => Token::StrType,
        "bool" => Token::BoolType,
        _ => Token::Ident(ident.to_string()),
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Illegal => write!(f, "illegal"),
            Token::EOF => write!(f, "eof"),
            Token::Ident(ident) => write!(f, "ident({})", ident),
            Token::Int(int) => write!(f, "int({})", int),
            Token::Float(float) => write!(f, "float({})", float),
            Token::Str(str) => write!(f, "str({})", str),
            Token::Bool(bool) => write!(f, "bool({})", bool),
            Token::IntType => write!(f, "int"),
            Token::FloatType => write!(f, "float"),
            Token::StrType => write!(f, "str"),
            Token::BoolType => write!(f, "bool"),
            Token::Assign => write!(f, "assign"),
            Token::Plus => write!(f, "plus"),
            Token::Comma => write!(f, "comma"),
            Token::Colon => write!(f, "colon"),
            Token::Semicolon => write!(f, "semicolon"),
            Token::Lparen => write!(f, "lparen"),
            Token::Rparen => write!(f, "rparen"),
            Token::Lbrace => write!(f, "lbrace"),
            Token::Rbrace => write!(f, "rbrace"),
            Token::Function => write!(f, "fn"),
            Token::Let => write!(f, "let"),
        }
    }
}
