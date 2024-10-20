use crate::token::{lookup_ident, Token};

#[derive(Clone, Debug)]
pub struct Lexer<'i> {
    pub input: &'i str,
    pub pos: usize,
    pub next_pos: usize,
    pub ch: char,
}

impl Lexer<'_> {
    pub fn new(input: &str) -> Lexer {
        let mut lexer = Lexer {
            input,
            pos: 0,
            next_pos: 0,
            ch: '\0',
        };
        lexer.read_char();
        lexer
    }

    pub fn next_tok(&mut self) -> Token {
        self.skip_whitespace();
        self.skip_comments();

        match self.ch {
            '=' => match self.peek_char() {
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
            '+' => {
                self.read_char();
                Token::Plus
            }
            '-' => match self.peek_char() {
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
            '!' => match self.peek_char() {
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
            '*' => {
                self.read_char();
                Token::Asterisk
            }
            '/' => {
                self.read_char();
                Token::Slash
            }
            '<' => match self.peek_char() {
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
            '>' => match self.peek_char() {
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
                lookup_ident(&self.input[pos..self.pos])
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
        }
    }

    pub fn print_tokens(&mut self) {
        let mut token = self.next_tok();
        while token != Token::EOF {
            println!("{token:?}");
            token = self.next_tok();
        }
    }

    fn peek_char(&mut self) -> char {
        if self.next_pos < self.input.len() {
            self.input.chars().nth(self.next_pos).unwrap()
        } else {
            '\0'
        }
    }

    fn read_char(&mut self) {
        if self.next_pos < self.input.len() {
            self.ch = self.input.chars().nth(self.next_pos).unwrap();
        } else {
            self.ch = '\0';
        }
        self.pos = self.next_pos;
        self.next_pos += 1;
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
        while self.ch == '/' && self.peek_char() == '/' {
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
        match self.next_tok() {
            Token::EOF => None,
            token => Some(token),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_next_tok() {
        let input = "// comment
let five: int = 5;
let ten: int = 10;

fn add(x: int, y: int) -> int {
    x + y
}

let result = add(five, ten);
!-/*5;
5 < 10 > 5;

if 5 < 10 {
    return 1;
} elif 4 < 5 {
    return true;
} else {
    return false;
}

10 == 10;
10 != 9;
";

        let mut lexer = Lexer::new(input);

        let expected = vec![
            Token::Let,
            Token::Ident(String::from("five")),
            Token::Colon,
            Token::IntType,
            Token::Assign,
            Token::Int(5),
            Token::Semicolon,
            Token::Let,
            Token::Ident(String::from("ten")),
            Token::Colon,
            Token::IntType,
            Token::Assign,
            Token::Int(10),
            Token::Semicolon,
            Token::Function,
            Token::Ident(String::from("add")),
            Token::Lparen,
            Token::Ident(String::from("x")),
            Token::Colon,
            Token::IntType,
            Token::Comma,
            Token::Ident(String::from("y")),
            Token::Colon,
            Token::IntType,
            Token::Rparen,
            Token::Arrow,
            Token::IntType,
            Token::Lbrace,
            Token::Ident(String::from("x")),
            Token::Plus,
            Token::Ident(String::from("y")),
            Token::Rbrace,
            Token::Let,
            Token::Ident(String::from("result")),
            Token::Assign,
            Token::Ident(String::from("add")),
            Token::Lparen,
            Token::Ident(String::from("five")),
            Token::Comma,
            Token::Ident(String::from("ten")),
            Token::Rparen,
            Token::Semicolon,
            Token::Bang,
            Token::Minus,
            Token::Slash,
            Token::Asterisk,
            Token::Int(5),
            Token::Semicolon,
            Token::Int(5),
            Token::LT,
            Token::Int(10),
            Token::GT,
            Token::Int(5),
            Token::Semicolon,
            Token::If,
            Token::Int(5),
            Token::LT,
            Token::Int(10),
            Token::Lbrace,
            Token::Return,
            Token::Int(1),
            Token::Semicolon,
            Token::Rbrace,
            Token::Elif,
            Token::Int(4),
            Token::LT,
            Token::Int(5),
            Token::Lbrace,
            Token::Return,
            Token::Bool(true),
            Token::Semicolon,
            Token::Rbrace,
            Token::Else,
            Token::Lbrace,
            Token::Return,
            Token::Bool(false),
            Token::Semicolon,
            Token::Rbrace,
            Token::Int(10),
            Token::EQ,
            Token::Int(10),
            Token::Semicolon,
            Token::Int(10),
            Token::NEQ,
            Token::Int(9),
            Token::Semicolon,
            Token::EOF,
        ];

        for (i, expected) in expected.iter().enumerate() {
            let actual = lexer.next_tok();
            println!("[{i}] expected: {expected:?}, actual: {actual:?}");
            assert_eq!(*expected, actual);
        }
    }
}
