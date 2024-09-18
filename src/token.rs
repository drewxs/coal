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
            Token::Minus => write!(f, "minus"),
            Token::Bang => write!(f, "bang"),
            Token::Asterisk => write!(f, "asterisk"),
            Token::Slash => write!(f, "slash"),
            Token::LT => write!(f, "lt"),
            Token::GT => write!(f, "gt"),
            Token::LTE => write!(f, "lte"),
            Token::GTE => write!(f, "gte"),
            Token::EQ => write!(f, "eq"),
            Token::NEQ => write!(f, "neq"),
            Token::Comma => write!(f, "comma"),
            Token::Colon => write!(f, "colon"),
            Token::Semicolon => write!(f, "semicolon"),
            Token::Arrow => write!(f, "arrow"),
            Token::Lparen => write!(f, "lparen"),
            Token::Rparen => write!(f, "rparen"),
            Token::Lbrace => write!(f, "lbrace"),
            Token::Rbrace => write!(f, "rbrace"),
            Token::Function => write!(f, "fn"),
            Token::Let => write!(f, "let"),
            Token::If => write!(f, "if"),
            Token::Elif => write!(f, "elif"),
            Token::Else => write!(f, "else"),
            Token::Return => write!(f, "return"),
        }
    }
}
