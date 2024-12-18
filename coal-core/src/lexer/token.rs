use std::fmt;

#[derive(Clone, Debug, PartialEq, Default)]
pub enum Token {
    #[default]
    Illegal,
    EOF,
    Comment(String),
    NewLine,

    // Identifiers + literals
    Ident(String),
    U32(u32),
    U64(u64),
    I32(i32),
    I64(i64),
    I128(i128),
    F32(f32),
    F64(f64),
    Str(String),
    Bool(bool),

    // Operators
    Add,
    Sub,
    Mul,
    Div,
    IntDiv,
    Rem,
    Bang,
    Assign,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    RemAssign,

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
    While,
    Return,
}

impl From<&str> for Token {
    fn from(s: &str) -> Self {
        match s {
            "true" => Token::Bool(true),
            "false" => Token::Bool(false),
            "+" => Token::Add,
            "-" => Token::Sub,
            "*" => Token::Mul,
            "/" => Token::Div,
            "//" => Token::IntDiv,
            "%" => Token::Rem,
            "!" => Token::Bang,
            "=" => Token::Assign,
            "+=" => Token::AddAssign,
            "-=" => Token::SubAssign,
            "*=" => Token::MulAssign,
            "/=" => Token::DivAssign,
            "%=" => Token::RemAssign,
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
            "while" => Token::While,
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
            Token::Comment(s) => writeln!(f, "// {s}"),
            Token::NewLine => writeln!(f),
            Token::Ident(name) => write!(f, "{name}"),
            Token::U32(i) => write!(f, "{i}"),
            Token::U64(i) => write!(f, "{i}"),
            Token::I32(i) => write!(f, "{i}"),
            Token::I64(i) => write!(f, "{i}"),
            Token::I128(i) => write!(f, "{i}"),
            Token::F32(n) => write!(f, "{n}"),
            Token::F64(n) => write!(f, "{n}"),
            Token::Str(s) => write!(f, "{s}"),
            Token::Bool(b) => write!(f, "{b}"),
            Token::Add => write!(f, "+"),
            Token::Sub => write!(f, "-"),
            Token::Mul => write!(f, "*"),
            Token::Div => write!(f, "/"),
            Token::IntDiv => write!(f, "//"),
            Token::Rem => write!(f, "%"),
            Token::Assign => write!(f, "="),
            Token::AddAssign => write!(f, "+="),
            Token::SubAssign => write!(f, "-="),
            Token::MulAssign => write!(f, "*="),
            Token::DivAssign => write!(f, "/="),
            Token::RemAssign => write!(f, "%="),
            Token::Bang => write!(f, "!"),
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
            Token::While => write!(f, "while"),
            Token::Return => write!(f, "return"),
        }
    }
}

pub type Position = (usize, usize);
pub type Span = (Position, Position);

#[derive(Clone, Debug, Default, PartialEq)]
pub struct LexicalToken {
    pub token: Token,
    pub span: Span,
}

impl LexicalToken {
    pub fn new(token: Token, span: Span) -> Self {
        LexicalToken { token, span }
    }
}
