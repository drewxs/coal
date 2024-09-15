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
            '\0' => Token::EOF,
            'a'..='z' | 'A'..='Z' | '_' => {
                return lookup_ident(&self.read_ident());
            }
            '0'..='9' | '.' => {
                return self.read_num();
            }
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

    fn read_num(&mut self) -> Token {
        let pos = self.pos;
        let mut is_float = false;

        while self.ch == '.' || self.ch.is_ascii_digit() {
            if self.ch == '.' {
                is_float = true;
            }
            self.read_char();
        }

        let num_str = &self.input[pos..self.pos];
        if is_float {
            match num_str.parse::<f64>() {
                Ok(value) => Token::Float(value),
                Err(_) => panic!("Failed to parse float: {}", num_str),
            }
        } else {
            match num_str.parse::<i64>() {
                Ok(value) => Token::Int(value),
                Err(_) => panic!("Failed to parse int: {}", num_str),
            }
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
let five = 5;
let ten = 10;

let add = fn(x, y) {
    x + y
};

let result = add(five, ten);
";

        let mut lexer = Lexer::new(input);

        let expected = vec![
            Token::Let,
            Token::Ident("five".to_string()),
            Token::Assign,
            Token::Int(5),
            Token::Semicolon,
            Token::Let,
            Token::Ident("ten".to_string()),
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

        for (i, token) in expected.iter().enumerate() {
            println!("[{i}] {token}");
            assert_eq!(lexer.next_token(), *token);
        }
    }
}
