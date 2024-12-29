#[cfg(test)]
mod tests;

pub mod token;

use crate::clean_input;

pub use token::{Span, Token, TokenKind};

#[derive(Clone, Debug, Default)]
pub struct Lexer {
    pub input: String,
    pub pos: usize,
    pub next_pos: usize,
    pub ch: char,
    pub line: usize,
    pub col: usize,
}

impl Lexer {
    pub fn new(input: &str) -> Lexer {
        let mut lexer = Lexer {
            input: clean_input(input),
            pos: 0,
            next_pos: 0,
            ch: '\0',
            line: 1,
            col: 0,
        };
        lexer.read_char();
        lexer
    }

    pub fn lexical_tokens(&mut self) -> Vec<Token> {
        self.collect()
    }

    pub fn tokens(&mut self) -> Vec<TokenKind> {
        let mut tokens = vec![];
        for node in self {
            tokens.push(node.token)
        }
        tokens
    }

    pub fn print_tokens(&mut self) {
        for token in self {
            println!("{token:?}");
        }
    }

    fn next_char(&mut self) -> char {
        if self.next_pos < self.input.len() {
            self.input.chars().nth(self.next_pos).unwrap_or('\0')
        } else {
            '\0'
        }
    }

    fn read_char(&mut self) {
        if self.next_pos < self.input.len() {
            self.ch = self.input.chars().nth(self.next_pos).unwrap_or('\0');
        } else {
            self.ch = '\0';
        }
        self.pos = self.next_pos;
        self.next_pos += 1;

        if self.ch == '\n' {
            self.line += 1;
            self.col = 0;
        } else {
            self.col += 1;
        }
    }

    fn skip_whitespace(&mut self) {
        while self.ch.is_whitespace() && self.ch != '\n' && self.ch != '\0' {
            self.read_char();
        }
        self.consume('\n');
    }

    fn consume(&mut self, ch: char) {
        if self.ch == ch {
            self.read_char();
        }
    }
}

impl Iterator for Lexer {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.skip_whitespace();
        let cursor = (self.line, self.col);

        let node = match self.ch {
            '=' => match self.next_char() {
                '=' => {
                    self.read_char();
                    self.read_char();
                    Token::new(TokenKind::EQ, (cursor, (self.line, self.col)))
                }
                _ => {
                    self.read_char();
                    Token::new(TokenKind::Assign, (cursor, cursor))
                }
            },
            ';' => {
                self.read_char();
                Token::new(TokenKind::Semicolon, (cursor, cursor))
            }
            '(' => {
                self.read_char();
                Token::new(TokenKind::Lparen, (cursor, cursor))
            }
            ')' => {
                self.read_char();
                Token::new(TokenKind::Rparen, (cursor, cursor))
            }
            ',' => {
                self.read_char();
                Token::new(TokenKind::Comma, (cursor, cursor))
            }
            '!' => match self.next_char() {
                '=' => {
                    self.read_char();
                    self.read_char();
                    Token::new(TokenKind::NEQ, (cursor, (self.line, self.col)))
                }
                _ => {
                    self.read_char();
                    Token::new(TokenKind::Bang, (cursor, cursor))
                }
            },
            '+' => match self.next_char() {
                '=' => {
                    self.read_char();
                    self.read_char();
                    Token::new(TokenKind::AddAssign, (cursor, (self.line, self.col)))
                }
                _ => {
                    self.read_char();
                    Token::new(TokenKind::Add, (cursor, cursor))
                }
            },
            '-' => match self.next_char() {
                '=' => {
                    self.read_char();
                    self.read_char();
                    Token::new(TokenKind::SubAssign, (cursor, (self.line, self.col)))
                }
                '>' => {
                    self.read_char();
                    self.read_char();
                    Token::new(TokenKind::Arrow, (cursor, (self.line, self.col)))
                }
                _ => {
                    self.read_char();
                    Token::new(TokenKind::Sub, (cursor, cursor))
                }
            },
            '*' => match self.next_char() {
                '=' => {
                    self.read_char();
                    self.read_char();
                    Token::new(TokenKind::MulAssign, (cursor, (self.line, self.col)))
                }
                _ => {
                    self.read_char();
                    Token::new(TokenKind::Mul, (cursor, cursor))
                }
            },
            '/' => match self.next_char() {
                '=' => {
                    self.read_char();
                    self.read_char();
                    Token::new(TokenKind::DivAssign, (cursor, (self.line, self.col)))
                }
                '/' => {
                    if self.col <= 2 {
                        let pos = self.pos + 3;
                        while self.ch != '\n' && self.ch != '\0' {
                            self.read_char();
                        }
                        let val = self.input[pos..(self.pos.max(pos))].to_string();
                        Token::new(TokenKind::Comment(val), (cursor, (self.line, self.col)))
                    } else {
                        self.read_char();
                        self.read_char();
                        Token::new(TokenKind::IntDiv, (cursor, (self.line, self.col)))
                    }
                }
                _ => {
                    self.read_char();
                    Token::new(TokenKind::Div, (cursor, cursor))
                }
            },
            '%' => match self.next_char() {
                '=' => {
                    self.read_char();
                    self.read_char();
                    Token::new(TokenKind::RemAssign, (cursor, (self.line, self.col)))
                }
                _ => {
                    self.read_char();
                    Token::new(TokenKind::Rem, (cursor, cursor))
                }
            },
            '<' => match self.next_char() {
                '=' => {
                    self.read_char();
                    self.read_char();
                    Token::new(TokenKind::LTE, (cursor, (self.line, self.col)))
                }
                _ => {
                    self.read_char();
                    Token::new(TokenKind::LT, (cursor, cursor))
                }
            },
            '>' => match self.next_char() {
                '=' => {
                    self.read_char();
                    self.read_char();
                    Token::new(TokenKind::GTE, (cursor, (self.line, self.col)))
                }
                _ => {
                    self.read_char();
                    Token::new(TokenKind::GT, (cursor, cursor))
                }
            },
            '{' => {
                self.read_char();
                Token::new(TokenKind::Lbrace, (cursor, cursor))
            }
            '}' => {
                self.read_char();
                Token::new(TokenKind::Rbrace, (cursor, cursor))
            }
            '[' => {
                self.read_char();
                Token::new(TokenKind::Lbracket, (cursor, cursor))
            }
            ']' => {
                self.read_char();
                Token::new(TokenKind::Rbracket, (cursor, cursor))
            }
            ':' => {
                self.read_char();
                Token::new(TokenKind::Colon, (cursor, cursor))
            }
            '"' => {
                self.read_char();
                let pos = self.pos;
                while self.ch != '"' && self.ch != '\0' {
                    if self.ch == '\\' && self.next_char() != '\0' {
                        self.read_char();
                        self.read_char();
                    } else {
                        self.read_char();
                    }
                }
                let val = self.input[pos..self.pos].to_string();
                let cursor_end = (self.line, self.col);
                self.consume('"');
                Token::new(TokenKind::Str(val), (cursor, cursor_end))
            }
            'a'..='z' | 'A'..='Z' | '_' => {
                let pos = self.pos;
                while self.ch == '_' || self.ch.is_alphanumeric() {
                    self.read_char();
                }
                Token::new(
                    TokenKind::from(&self.input[pos..self.pos]),
                    (cursor, (self.line, self.col.saturating_sub(1))),
                )
            }
            '0'..='9' => {
                let pos = self.pos;
                let (line, col) = cursor;

                let mut is_float = false;
                while self.ch.is_ascii_digit() || self.ch == '.' {
                    self.read_char();
                    if self.ch == '.' {
                        is_float = true;
                    }
                }

                let s = &self.input[pos..self.pos];
                if is_float {
                    Token::new(
                        TokenKind::F64(s.parse::<f64>().expect("invalid float literal")),
                        (cursor, (line, col + s.len() - 1)),
                    )
                } else {
                    Token::new(
                        TokenKind::I32(s.parse::<i32>().expect("invalid int literal")),
                        (cursor, (line, col + s.len() - 1)),
                    )
                }
            }
            '.' => match self.next_char() {
                '0'..='9' => {
                    let pos = self.pos;
                    let (line, col) = cursor;

                    while self.ch.is_ascii_digit() || self.ch == '.' {
                        self.read_char();
                    }

                    Token::new(
                        TokenKind::F64(
                            self.input[pos..self.pos]
                                .parse::<f64>()
                                .expect("invalid float literal"),
                        ),
                        (cursor, (line, col + self.pos - pos)),
                    )
                }
                _ => {
                    self.read_char();
                    Token::new(TokenKind::Dot, (cursor, cursor))
                }
            },
            '\n' => {
                self.read_char();
                Token::new(
                    TokenKind::NewLine,
                    ((self.line - 1, self.col), (self.line - 1, self.col)),
                )
            }
            '\0' => return None,
            _ => {
                self.read_char();
                Token::new(TokenKind::Illegal, (cursor, (self.line, self.col)))
            }
        };

        Some(node)
    }
}
