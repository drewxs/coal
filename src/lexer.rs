use crate::token::{lookup_ident, Token};

pub struct Lexer<'a> {
    pub input: &'a str,
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

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();
        self.skip_comments();

        let token = match self.ch {
            '=' => Token::Assign,
            ';' => Token::Semicolon,
            '(' => Token::Lparen,
            ')' => Token::Rparen,
            ',' => Token::Comma,
            '+' => Token::Plus,
            '{' => Token::Lbrace,
            '}' => Token::Rbrace,
            '"' => return self.read_str(),
            'a'..='z' | 'A'..='Z' | '_' => {
                return match self.read_ident().as_str() {
                    "true" => Token::Bool(true),
                    "false" => Token::Bool(false),
                    ident => lookup_ident(ident),
                }
            }
            '0'..='9' | '.' => return self.read_num(),
            ':' => Token::Colon,
            '\0' => Token::EOF,
            _ => Token::Illegal,
        };

        self.read_char();

        token
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

    fn read_ident(&mut self) -> String {
        let pos = self.pos;
        while is_letter(self.ch) {
            self.read_char();
        }
        self.input[pos..self.pos].to_string()
    }

    fn read_str(&mut self) -> Token {
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

    fn read_num(&mut self) -> Token {
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

fn is_letter(ch: char) -> bool {
    ch == '_' || ch.is_alphabetic()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_next_token() {
        let input = "// comment
let five: int = 5;
let ten: int = 10;

let add = fn(x, y) {
    x + y
};

let result: int = add(five, ten);
";

        let mut lexer = Lexer::new(input);

        let expected = vec![
            Token::Let,
            Token::Ident("five".to_string()),
            Token::Colon,
            Token::IntType,
            Token::Assign,
            Token::Int(5),
            Token::Semicolon,
            Token::Let,
            Token::Ident("ten".to_string()),
            Token::Colon,
            Token::IntType,
            Token::Assign,
            Token::Int(10),
            Token::Semicolon,
            Token::Let,
            Token::Ident("add".to_string()),
            Token::Assign,
            Token::Function,
            Token::Lparen,
            Token::Ident("x".to_string()),
            Token::Comma,
            Token::Ident("y".to_string()),
            Token::Rparen,
            Token::Lbrace,
            Token::Ident("x".to_string()),
            Token::Plus,
            Token::Ident("y".to_string()),
            Token::Rbrace,
            Token::Semicolon,
            Token::Let,
            Token::Ident("result".to_string()),
            Token::Colon,
            Token::IntType,
            Token::Assign,
            Token::Ident("add".to_string()),
            Token::Lparen,
            Token::Ident("five".to_string()),
            Token::Comma,
            Token::Ident("ten".to_string()),
            Token::Rparen,
            Token::Semicolon,
            Token::EOF,
        ];

        for (i, expected) in expected.iter().enumerate() {
            let actual = lexer.next_token();
            println!("[{i}] expected: {expected}, actual: {actual}");
            assert_eq!(*expected, actual);
        }
    }
}
