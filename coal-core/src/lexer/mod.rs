#[cfg(test)]
mod tests;

pub mod token;

use crate::clean_input;

pub use token::{Span, Token, TokenKind};

#[derive(Clone, Debug, Default)]
pub struct Lexer {
    pub input: String,
    pub chars: Vec<char>,
    pub pos: usize,
    pub next_pos: usize,
    pub ch: char,
    pub line: usize,
    pub col: usize,
}

impl Lexer {
    pub fn new(input: &str) -> Lexer {
        let input = clean_input(input);
        let chars = input.chars().collect();
        let mut lexer = Lexer {
            input,
            chars,
            pos: 0,
            next_pos: 0,
            ch: '\0',
            line: 1,
            col: 0,
        };
        lexer.read_ch();
        lexer
    }

    fn next_ch(&mut self) -> char {
        if self.next_pos < self.input.len() {
            self.chars[self.next_pos]
        } else {
            '\0'
        }
    }

    fn read_ch(&mut self) {
        self.ch = self.next_ch();
        self.pos = self.next_pos;
        self.next_pos += 1;

        if self.ch == '\n' {
            self.line += 1;
            self.col = 0;
        } else {
            self.col += 1;
        }
    }

    fn consume(&mut self, ch: char) {
        if self.ch == ch {
            self.read_ch();
        }
    }

    fn skip_whitespace(&mut self) {
        while self.ch.is_whitespace() && self.ch != '\n' && self.ch != '\0' {
            self.read_ch();
        }
        self.consume('\n');
    }
}

impl Iterator for Lexer {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.skip_whitespace();
        let cursor = (self.line, self.col);

        let node = match self.ch {
            '=' => match self.next_ch() {
                '=' => {
                    self.read_ch();
                    self.read_ch();
                    Token::new(TokenKind::EQ, (cursor, (self.line, self.col)))
                }
                _ => {
                    self.read_ch();
                    Token::new(TokenKind::Assign, (cursor, cursor))
                }
            },
            ';' => {
                self.read_ch();
                Token::new(TokenKind::Semicolon, (cursor, cursor))
            }
            '(' => {
                self.read_ch();
                Token::new(TokenKind::Lparen, (cursor, cursor))
            }
            ')' => {
                self.read_ch();
                Token::new(TokenKind::Rparen, (cursor, cursor))
            }
            ',' => {
                self.read_ch();
                Token::new(TokenKind::Comma, (cursor, cursor))
            }
            '!' => match self.next_ch() {
                '=' => {
                    self.read_ch();
                    self.read_ch();
                    Token::new(TokenKind::NEQ, (cursor, (self.line, self.col)))
                }
                _ => {
                    self.read_ch();
                    Token::new(TokenKind::Bang, (cursor, cursor))
                }
            },
            '+' => match self.next_ch() {
                '=' => {
                    self.read_ch();
                    self.read_ch();
                    Token::new(TokenKind::AddAssign, (cursor, (self.line, self.col)))
                }
                _ => {
                    self.read_ch();
                    Token::new(TokenKind::Add, (cursor, cursor))
                }
            },
            '-' => match self.next_ch() {
                '=' => {
                    self.read_ch();
                    self.read_ch();
                    Token::new(TokenKind::SubAssign, (cursor, (self.line, self.col)))
                }
                '>' => {
                    self.read_ch();
                    self.read_ch();
                    Token::new(TokenKind::Arrow, (cursor, (self.line, self.col)))
                }
                _ => {
                    self.read_ch();
                    Token::new(TokenKind::Sub, (cursor, cursor))
                }
            },
            '*' => match self.next_ch() {
                '=' => {
                    self.read_ch();
                    self.read_ch();
                    Token::new(TokenKind::MulAssign, (cursor, (self.line, self.col)))
                }
                _ => {
                    self.read_ch();
                    Token::new(TokenKind::Mul, (cursor, cursor))
                }
            },
            '/' => match self.next_ch() {
                '=' => {
                    self.read_ch();
                    self.read_ch();
                    Token::new(TokenKind::DivAssign, (cursor, (self.line, self.col)))
                }
                '/' if self.col <= 2 => {
                    let pos = self.pos + 3;
                    while self.ch != '\n' && self.ch != '\0' {
                        self.read_ch();
                    }
                    let val = self.input[pos..(self.pos.max(pos))].to_string();
                    Token::new(TokenKind::Comment(val), (cursor, (self.line, self.col)))
                }
                _ => {
                    self.read_ch();
                    Token::new(TokenKind::Div, (cursor, cursor))
                }
            },
            '%' => match self.next_ch() {
                '=' => {
                    self.read_ch();
                    self.read_ch();
                    Token::new(TokenKind::RemAssign, (cursor, (self.line, self.col)))
                }
                _ => {
                    self.read_ch();
                    Token::new(TokenKind::Rem, (cursor, cursor))
                }
            },
            '<' => match self.next_ch() {
                '=' => {
                    self.read_ch();
                    self.read_ch();
                    Token::new(TokenKind::LTE, (cursor, (self.line, self.col)))
                }
                _ => {
                    self.read_ch();
                    Token::new(TokenKind::LT, (cursor, cursor))
                }
            },
            '>' => match self.next_ch() {
                '=' => {
                    self.read_ch();
                    self.read_ch();
                    Token::new(TokenKind::GTE, (cursor, (self.line, self.col)))
                }
                _ => {
                    self.read_ch();
                    Token::new(TokenKind::GT, (cursor, cursor))
                }
            },
            '{' => {
                self.read_ch();
                Token::new(TokenKind::Lbrace, (cursor, cursor))
            }
            '}' => {
                self.read_ch();
                Token::new(TokenKind::Rbrace, (cursor, cursor))
            }
            '[' => {
                self.read_ch();
                Token::new(TokenKind::Lbracket, (cursor, cursor))
            }
            ']' => {
                self.read_ch();
                Token::new(TokenKind::Rbracket, (cursor, cursor))
            }
            ':' => {
                self.read_ch();
                Token::new(TokenKind::Colon, (cursor, cursor))
            }
            '"' => {
                self.read_ch();

                let pos = self.pos;

                while self.ch != '"' && self.ch != '\0' {
                    if self.ch == '\\' && self.next_ch() != '\0' {
                        self.read_ch();
                        self.read_ch();
                    } else {
                        self.read_ch();
                    }
                }

                let val = self.input[pos..self.pos].to_string();
                let cursor_end = (self.line, self.col);

                self.consume('"');

                Token::new(TokenKind::Str(val), (cursor, cursor_end))
            }
            'a'..='z' | 'A'..='Z' | '_' => {
                let pos = self.pos;

                while self.ch.is_alphanumeric() || self.ch == '_' {
                    self.read_ch();
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
                    self.read_ch();

                    if self.ch == '.' && self.next_ch() == '.' {
                        let s = &self.input[pos..self.pos];
                        return Some(Token::new(
                            s.parse::<i32>()
                                .map(TokenKind::I32)
                                .or_else(|_| s.parse::<i64>().map(TokenKind::I64))
                                .or_else(|_| s.parse::<i128>().map(TokenKind::I128))
                                .expect("invalid int literal"),
                            (cursor, (line, col + s.len() - 1)),
                        ));
                    }

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
                        s.parse::<i32>()
                            .map(TokenKind::I32)
                            .or_else(|_| s.parse::<i64>().map(TokenKind::I64))
                            .or_else(|_| s.parse::<i128>().map(TokenKind::I128))
                            .expect("invalid int literal"),
                        (cursor, (line, col + s.len() - 1)),
                    )
                }
            }
            '.' => match self.next_ch() {
                '.' => {
                    self.read_ch();
                    self.read_ch();
                    Token::new(TokenKind::Range, (cursor, (self.line, self.col)))
                }
                '0'..='9' => {
                    let pos = self.pos;
                    let (line, col) = cursor;

                    while self.ch.is_ascii_digit() || self.ch == '.' {
                        self.read_ch();
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
                    self.read_ch();
                    Token::new(TokenKind::Dot, (cursor, cursor))
                }
            },
            '|' => {
                self.read_ch();
                Token::new(TokenKind::Pipe, (cursor, cursor))
            }
            '\n' => {
                self.read_ch();
                Token::new(
                    TokenKind::NewLine,
                    ((self.line - 1, self.col), (self.line - 1, self.col)),
                )
            }
            '\0' => return None,
            _ => {
                self.read_ch();
                Token::new(TokenKind::Illegal, (cursor, (self.line, self.col)))
            }
        };

        Some(node)
    }
}
