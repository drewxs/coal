pub mod node;

#[cfg(test)]
mod tests;

pub use node::{Node, Position};

use crate::Token;

#[derive(Clone, Debug)]
pub struct Lexer<'i> {
    pub input: &'i str,
    pub pos: usize,
    pub next_pos: usize,
    pub ch: char,
    pub line: usize,
    pub col: usize,
}

impl Lexer<'_> {
    pub fn new(input: &str) -> Lexer {
        let mut lexer = Lexer {
            input,
            pos: 0,
            next_pos: 0,
            ch: '\0',
            line: 1,
            col: 1,
        };
        lexer.read_char();
        lexer
    }

    pub fn next_node(&mut self) -> Node {
        self.skip_whitespace();
        self.skip_comments();

        let tok = match self.ch {
            '=' => match self.next_char() {
                '=' => {
                    self.read_char();
                    self.read_char();
                    Token::EQ
                }
                _ => {
                    self.read_char();
                    Token::Assign
                }
            },
            ';' => {
                self.read_char();
                Token::Semicolon
            }
            '(' => {
                self.read_char();
                Token::Lparen
            }
            ')' => {
                self.read_char();
                Token::Rparen
            }
            ',' => {
                self.read_char();
                Token::Comma
            }
            '!' => match self.next_char() {
                '=' => {
                    self.read_char();
                    self.read_char();
                    Token::NEQ
                }
                _ => {
                    self.read_char();
                    Token::Bang
                }
            },
            '+' => {
                self.read_char();
                Token::Plus
            }
            '-' => match self.next_char() {
                '>' => {
                    self.read_char();
                    self.read_char();
                    Token::Arrow
                }
                _ => {
                    self.read_char();
                    Token::Minus
                }
            },
            '*' => {
                self.read_char();
                Token::Asterisk
            }
            '/' => match self.next_char() {
                '/' => {
                    self.read_char();
                    Token::DoubleSlash
                }

                _ => {
                    self.read_char();
                    Token::Slash
                }
            },
            '%' => {
                self.read_char();
                Token::Modulo
            }
            '<' => match self.next_char() {
                '=' => {
                    self.read_char();
                    self.read_char();
                    Token::LTE
                }
                _ => {
                    self.read_char();
                    Token::LT
                }
            },
            '>' => match self.next_char() {
                '=' => {
                    self.read_char();
                    self.read_char();
                    Token::GTE
                }
                _ => {
                    self.read_char();
                    Token::GT
                }
            },
            '{' => {
                self.read_char();
                Token::Lbrace
            }
            '}' => {
                self.read_char();
                Token::Rbrace
            }
            ':' => {
                self.read_char();
                Token::Colon
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
                Token::Str(val)
            }
            'a'..='z' | 'A'..='Z' | '_' => {
                let pos = self.pos;
                while self.ch == '_' || self.ch.is_alphabetic() {
                    self.read_char();
                }
                Token::from(&self.input[pos..self.pos])
            }
            '0'..='9' | '.' => {
                let pos = self.pos;

                while self.ch.is_ascii_digit() || self.ch == '.' {
                    self.read_char();
                }

                let num_str = &self.input[pos..self.pos];
                if num_str.contains('.') {
                    Token::Float(num_str.parse::<f64>().unwrap())
                } else {
                    Token::Int(num_str.parse::<i64>().unwrap())
                }
            }
            '\0' => {
                self.read_char();
                Token::EOF
            }
            _ => {
                self.read_char();
                Token::Illegal
            }
        };

        Node {
            token: tok,
            pos: (self.line, self.col),
        }
    }

    pub fn tokens(&mut self) -> Vec<Token> {
        self.collect()
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

    fn read_line(&mut self) {
        while self.ch != '\n' && self.ch != '\0' {
            self.read_char();
        }
        if self.ch == '\n' {
            self.read_char();
        }
    }

    fn skip_comments(&mut self) {
        while self.ch == '/' && self.next_char() == '/' {
            self.read_line();
        }
    }

    fn skip_whitespace(&mut self) {
        while self.ch.is_whitespace() {
            self.read_char();
        }
    }
}

impl Iterator for Lexer<'_> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        match self.next_node() {
            Node {
                token: Token::EOF, ..
            } => None,
            Node { token, .. } => Some(token),
        }
    }
}
