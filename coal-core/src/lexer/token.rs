use std::fmt;

#[derive(Clone, Debug, Default, PartialEq)]
pub enum TokenKind {
    #[default]
    Illegal,
    EOF,
    Comment(String),
    NewLine,
    Dot,

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
    For,
    In,
    Range,
    Return,
    Nil,
}

impl From<&str> for TokenKind {
    fn from(s: &str) -> Self {
        match s {
            "\0" => TokenKind::EOF,
            "\n" => TokenKind::NewLine,
            "." => TokenKind::Dot,
            "true" => TokenKind::Bool(true),
            "false" => TokenKind::Bool(false),
            "+" => TokenKind::Add,
            "-" => TokenKind::Sub,
            "*" => TokenKind::Mul,
            "/" => TokenKind::Div,
            "//" => TokenKind::IntDiv,
            "%" => TokenKind::Rem,
            "!" => TokenKind::Bang,
            "=" => TokenKind::Assign,
            "+=" => TokenKind::AddAssign,
            "-=" => TokenKind::SubAssign,
            "*=" => TokenKind::MulAssign,
            "/=" => TokenKind::DivAssign,
            "%=" => TokenKind::RemAssign,
            "==" => TokenKind::EQ,
            "!=" => TokenKind::NEQ,
            "<" => TokenKind::LT,
            "<=" => TokenKind::LTE,
            ">" => TokenKind::GT,
            ">=" => TokenKind::GTE,
            ":" => TokenKind::Colon,
            ";" => TokenKind::Semicolon,
            "," => TokenKind::Comma,
            "->" => TokenKind::Arrow,
            "(" => TokenKind::Lparen,
            ")" => TokenKind::Rparen,
            "{" => TokenKind::Lbrace,
            "}" => TokenKind::Rbrace,
            "[" => TokenKind::Lbracket,
            "]" => TokenKind::Rbracket,
            "fn" => TokenKind::Fn,
            "let" => TokenKind::Let,
            "if" => TokenKind::If,
            "elif" => TokenKind::Elif,
            "else" => TokenKind::Else,
            "while" => TokenKind::While,
            "for" => TokenKind::For,
            "in" => TokenKind::In,
            ".." => TokenKind::Range,
            "return" => TokenKind::Return,
            "nil" => TokenKind::Nil,
            _ => TokenKind::Ident(s.to_string()),
        }
    }
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TokenKind::Illegal => write!(f, "ILLEGAL"),
            TokenKind::EOF => write!(f, "EOF"),
            TokenKind::Comment(s) => writeln!(f, "// {s}"),
            TokenKind::NewLine => writeln!(f),
            TokenKind::Dot => write!(f, "."),
            TokenKind::Ident(name) => write!(f, "{name}"),
            TokenKind::U32(i) => write!(f, "{i}"),
            TokenKind::U64(i) => write!(f, "{i}"),
            TokenKind::I32(i) => write!(f, "{i}"),
            TokenKind::I64(i) => write!(f, "{i}"),
            TokenKind::I128(i) => write!(f, "{i}"),
            TokenKind::F32(n) => write!(f, "{n}"),
            TokenKind::F64(n) => write!(f, "{n}"),
            TokenKind::Str(s) => write!(f, "{s}"),
            TokenKind::Bool(b) => write!(f, "{b}"),
            TokenKind::Add => write!(f, "+"),
            TokenKind::Sub => write!(f, "-"),
            TokenKind::Mul => write!(f, "*"),
            TokenKind::Div => write!(f, "/"),
            TokenKind::IntDiv => write!(f, "//"),
            TokenKind::Rem => write!(f, "%"),
            TokenKind::Assign => write!(f, "="),
            TokenKind::AddAssign => write!(f, "+="),
            TokenKind::SubAssign => write!(f, "-="),
            TokenKind::MulAssign => write!(f, "*="),
            TokenKind::DivAssign => write!(f, "/="),
            TokenKind::RemAssign => write!(f, "%="),
            TokenKind::Bang => write!(f, "!"),
            TokenKind::EQ => write!(f, "=="),
            TokenKind::NEQ => write!(f, "!="),
            TokenKind::LT => write!(f, "<"),
            TokenKind::LTE => write!(f, "<="),
            TokenKind::GT => write!(f, ">"),
            TokenKind::GTE => write!(f, ">="),
            TokenKind::Colon => write!(f, ":"),
            TokenKind::Semicolon => write!(f, ";"),
            TokenKind::Comma => write!(f, ","),
            TokenKind::Arrow => write!(f, "->"),
            TokenKind::Lparen => write!(f, "("),
            TokenKind::Rparen => write!(f, ")"),
            TokenKind::Lbrace => write!(f, "{{"),
            TokenKind::Rbrace => write!(f, "}}"),
            TokenKind::Lbracket => write!(f, "["),
            TokenKind::Rbracket => write!(f, "]"),
            TokenKind::Fn => write!(f, "fn"),
            TokenKind::Let => write!(f, "let"),
            TokenKind::If => write!(f, "if"),
            TokenKind::Elif => write!(f, "elif"),
            TokenKind::Else => write!(f, "else"),
            TokenKind::While => write!(f, "while"),
            TokenKind::For => write!(f, "for"),
            TokenKind::In => write!(f, "in"),
            TokenKind::Range => write!(f, "range"),
            TokenKind::Return => write!(f, "return"),
            TokenKind::Nil => write!(f, "nil"),
        }
    }
}

pub type Position = (usize, usize);
pub type Span = (Position, Position);

#[derive(Clone, Debug, Default, PartialEq)]
pub struct Token {
    pub token: TokenKind,
    pub span: Span,
}

impl Token {
    pub fn new(token: TokenKind, span: Span) -> Self {
        Token { token, span }
    }
}
