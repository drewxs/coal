pub mod token;

#[cfg(test)]
mod tests;

pub use token::{LexicalToken, Span, Token};

use crate::clean_input;

#[derive(Clone, Debug)]
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

    pub fn lexical_tokens(&mut self) -> Vec<LexicalToken> {
        self.collect()
    }

    pub fn tokens(&mut self) -> Vec<Token> {
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
            self.col = 1;
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
    type Item = LexicalToken;

    fn next(&mut self) -> Option<Self::Item> {
        self.skip_whitespace();
        let cursor = (self.line, self.col);

        let node = match self.ch {
            '=' => match self.next_char() {
                '=' => {
                    self.read_char();
                    self.read_char();
                    LexicalToken::new(Token::EQ, (cursor, (self.line, self.col)))
                }
                _ => {
                    self.read_char();
                    LexicalToken::new(Token::Assign, (cursor, (self.line, self.col)))
                }
            },
            ';' => {
                self.read_char();
                LexicalToken::new(Token::Semicolon, (cursor, (cursor.0, cursor.1 + 1)))
            }
            '(' => {
                self.read_char();
                LexicalToken::new(Token::Lparen, (cursor, (self.line, self.col)))
            }
            ')' => {
                self.read_char();
                LexicalToken::new(Token::Rparen, (cursor, (self.line, self.col)))
            }
            ',' => {
                self.read_char();
                LexicalToken::new(Token::Comma, (cursor, (self.line, self.col)))
            }
            '!' => match self.next_char() {
                '=' => {
                    self.read_char();
                    self.read_char();
                    LexicalToken::new(Token::NEQ, (cursor, (self.line, self.col)))
                }
                _ => {
                    self.read_char();
                    LexicalToken::new(Token::Bang, (cursor, (self.line, self.col)))
                }
            },
            '+' => {
                self.read_char();
                LexicalToken::new(Token::Plus, (cursor, (self.line, self.col)))
            }
            '-' => match self.next_char() {
                '>' => {
                    self.read_char();
                    self.read_char();
                    LexicalToken::new(Token::Arrow, (cursor, (self.line, self.col)))
                }
                _ => {
                    self.read_char();
                    LexicalToken::new(Token::Minus, (cursor, (self.line, self.col)))
                }
            },
            '*' => {
                self.read_char();
                LexicalToken::new(Token::Asterisk, (cursor, (self.line, self.col)))
            }
            '/' => match self.next_char() {
                '/' => {
                    if self.col <= 2 {
                        let pos = self.pos + 3;
                        while self.ch != '\n' && self.ch != '\0' {
                            self.read_char();
                        }
                        let val = self.input[pos..self.pos].to_string();
                        LexicalToken::new(Token::Comment(val), (cursor, (self.line, self.col)))
                    } else {
                        self.read_char();
                        self.read_char();
                        LexicalToken::new(Token::DoubleSlash, (cursor, (self.line, self.col)))
                    }
                }
                _ => {
                    self.read_char();
                    LexicalToken::new(Token::Slash, (cursor, (self.line, self.col)))
                }
            },
            '%' => {
                self.read_char();
                LexicalToken::new(Token::Modulo, (cursor, (self.line, self.col)))
            }
            '<' => match self.next_char() {
                '=' => {
                    self.read_char();
                    self.read_char();
                    LexicalToken::new(Token::LTE, (cursor, (self.line, self.col)))
                }
                _ => {
                    self.read_char();
                    LexicalToken::new(Token::LT, (cursor, (self.line, self.col)))
                }
            },
            '>' => match self.next_char() {
                '=' => {
                    self.read_char();
                    self.read_char();
                    LexicalToken::new(Token::GTE, (cursor, (self.line, self.col)))
                }
                _ => {
                    self.read_char();
                    LexicalToken::new(Token::GT, (cursor, (self.line, self.col)))
                }
            },
            '{' => {
                self.read_char();
                LexicalToken::new(Token::Lbrace, (cursor, (self.line, self.col)))
            }
            '}' => {
                self.read_char();
                LexicalToken::new(Token::Rbrace, (cursor, (self.line, self.col)))
            }
            ':' => {
                self.read_char();
                LexicalToken::new(Token::Colon, (cursor, (self.line, self.col)))
            }
            '"' => {
                let pos = self.pos + 1;
                loop {
                    self.read_char();
                    if self.ch == '\\' && self.next_char() != '\0' {
                        self.read_char();
                        continue;
                    }
                    if self.ch == '"' || self.ch == '\0' {
                        break;
                    }
                }
                let val = self.input[pos..self.pos].to_string();
                self.read_char();
                LexicalToken::new(Token::Str(val), (cursor, (self.line, self.col)))
            }
            'a'..='z' | 'A'..='Z' | '_' => {
                let pos = self.pos;
                while self.ch == '_' || self.ch.is_alphabetic() {
                    self.read_char();
                }
                LexicalToken::new(
                    Token::from(&self.input[pos..self.pos]),
                    (cursor, (self.line, self.col)),
                )
            }
            '0'..='9' | '.' => {
                let pos = self.pos;

                while self.ch.is_ascii_digit() || self.ch == '.' {
                    self.read_char();
                }

                let num_str = &self.input[pos..self.pos];
                if num_str.contains('.') {
                    LexicalToken::new(
                        Token::Float(num_str.parse::<f64>().expect("invalid float literal")),
                        (cursor, (self.line, self.col)),
                    )
                } else {
                    LexicalToken::new(
                        Token::Int(num_str.parse::<i64>().expect("invalid int literal")),
                        (cursor, (self.line, self.col)),
                    )
                }
            }
            '\n' => {
                self.read_char();
                LexicalToken::new(Token::NewLine, (cursor, (self.line, self.col)))
            }
            '\0' => {
                return None;
            }
            _ => {
                self.read_char();
                LexicalToken::new(Token::Illegal, (cursor, (self.line, self.col)))
            }
        };

        Some(node)
    }
}
