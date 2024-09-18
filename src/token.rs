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
    Minus,
    Bang,
    Asterisk,
    Slash,

    // Comparisons
    LT,
    GT,
    LTE,
    GTE,
    EQ,
    NEQ,

    // Delimiters
    Colon,
    Comma,
    Semicolon,
    Arrow,

    Lparen,
    Rparen,
    Lbrace,
    Rbrace,

    // Keywords
    Function,
    Let,
    If,
    Elif,
    Else,
    Return,
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
        "if" => Token::If,
        "elif" => Token::Elif,
        "else" => Token::Else,
        "return" => Token::Return,
        _ => Token::Ident(ident.to_string()),
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}
