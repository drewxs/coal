pub mod error;
pub mod precedence;

pub use error::ParserError;
use error::ParserErrorKind;
pub use precedence::Precedence;

use crate::{
    ast::{Expr, Ident, Infix, Literal, Prefix, Stmt, Type},
    Lexer, Program, Token,
};

#[derive(Clone, Debug)]
pub struct Parser<'l> {
    pub lexer: Lexer<'l>,
    pub curr_tok: Token,
    pub next_tok: Token,
    pub errors: Vec<ParserError>,
}

impl Parser<'_> {
    pub fn parse(&mut self) -> Program {
        let mut program = Program::new();
        while self.curr_tok != Token::EOF {
            if let Some(stmt) = self.parse_stmt() {
                program.statements.push(stmt);
            }
            self.advance();
        }
        program
    }

    /// Checks for parser errors and returns `Ok(())` if none, or an `Err` with error details if present.
    pub fn check(&mut self) -> Result<(), String> {
        if self.errors.is_empty() {
            return Ok(());
        }

        let mut errors = format!("parser has {} errors", self.errors.len());
        for error in &self.errors {
            errors += &format!("\n{error}");
        }
        Err(errors)
    }

    /// Prints errors if there are any.
    pub fn print_errors(&mut self) {
        if let Err(e) = self.check() {
            println!("{e}");
        }
    }

    /// Runs `check`, then panics if there are errors.
    pub fn validate(&mut self) {
        if let Err(e) = self.check() {
            panic!("parser encountered errors\n{}", e);
        }
    }

    fn advance(&mut self) {
        self.curr_tok = self.next_tok.clone();
        self.next_tok = self.lexer.next_tok();
    }

    fn consume(&mut self, token: Token) {
        if self.next_tok == token {
            self.advance();
        }
    }

    fn parse_stmt(&mut self) -> Option<Stmt> {
        match self.curr_tok {
            Token::Let => self.parse_let_stmt(),
            Token::Return => self.parse_return_stmt(),
            _ => self.parse_expr_stmt(),
        }
    }

    fn parse_let_stmt(&mut self) -> Option<Stmt> {
        match self.next_tok {
            Token::Ident(_) => self.advance(),
            _ => return None,
        }

        let ident = self.parse_ident()?;

        if !self.expect_next_tok(Token::Colon) {
            return None;
        }
        self.advance();

        let t = self.parse_type()?;

        if !self.expect_next_tok(Token::Assign) {
            return None;
        }
        self.advance();

        let expr = self.parse_expr(Precedence::Lowest)?;
        self.consume(Token::Semicolon);

        Some(Stmt::Let(ident, t, expr))
    }

    fn parse_return_stmt(&mut self) -> Option<Stmt> {
        self.advance();
        let expr = self.parse_expr(Precedence::Lowest).map(Stmt::Return);
        self.consume(Token::Semicolon);
        expr
    }

    fn parse_expr(&mut self, precedence: Precedence) -> Option<Expr> {
        let mut lhs = match &self.curr_tok {
            Token::Ident(_) => self.parse_ident().map(Expr::Ident),
            Token::Int(i) => Some(Expr::Literal(Literal::Int(*i))),
            Token::Float(f) => Some(Expr::Literal(Literal::Float(*f))),
            Token::Str(s) => Some(Expr::Literal(Literal::Str(s.clone()))),
            Token::Bool(b) => Some(Expr::Literal(Literal::Bool(*b))),
            Token::Bang | Token::Plus | Token::Minus => self.parse_prefix_expr(),
            _ => {
                self.errors.push(ParserError::new(
                    ParserErrorKind::UnexpectedToken,
                    format!(
                        "{}:{} no prefix parse function found for {:?}",
                        self.lexer.line, self.lexer.col, self.curr_tok
                    ),
                ));
                return None;
            }
        };

        while self.next_tok != Token::Semicolon && precedence < Precedence::from(&self.next_tok) {
            match self.next_tok {
                Token::Plus
                | Token::Minus
                | Token::Asterisk
                | Token::Slash
                | Token::Modulo
                | Token::EQ
                | Token::NEQ
                | Token::GT
                | Token::GTE
                | Token::LT
                | Token::LTE => {
                    self.advance();
                    lhs = self.parse_infix_expr(&lhs?);
                }
                Token::Lbracket => {
                    self.advance();
                }
                _ => break,
            }
        }

        lhs
    }

    fn parse_expr_stmt(&mut self) -> Option<Stmt> {
        self.parse_expr(Precedence::Lowest).map(|expr| {
            self.consume(Token::Semicolon);
            Stmt::Expr(expr)
        })
    }

    fn parse_prefix_expr(&mut self) -> Option<Expr> {
        let prefix = Prefix::try_from(&self.curr_tok).ok()?;
        self.advance();
        self.parse_expr(Precedence::Prefix)
            .map(|expr| Expr::Prefix(prefix, Box::new(expr)))
    }

    fn parse_infix_expr(&mut self, lhs: &Expr) -> Option<Expr> {
        let infix = Infix::try_from(&self.curr_tok).ok()?;
        let prec = Precedence::from(&self.curr_tok);
        self.advance();
        let rhs = self.parse_expr(prec)?;
        Some(Expr::Infix(infix, Box::new(lhs.clone()), Box::new(rhs)))
    }

    fn parse_ident(&mut self) -> Option<Ident> {
        match &self.curr_tok {
            Token::Ident(ident) => Some(Ident(ident.clone())),
            _ => None,
        }
    }

    fn parse_type(&mut self) -> Option<Type> {
        match self.curr_tok {
            Token::IntType => Some(Type::Int),
            Token::FloatType => Some(Type::Float),
            Token::StrType => Some(Type::String),
            Token::BoolType => Some(Type::Bool),
            _ => None,
        }
    }

    /// Returns whether the next token matches the given token.
    /// Also, advances if it does, and adds an error if it doesn't.
    fn expect_next_tok(&mut self, token: Token) -> bool {
        if self.next_tok == token {
            self.advance();
            return true;
        }
        self.errors.push(ParserError::new(
            ParserErrorKind::UnexpectedToken,
            format!(
                "{}:{} expected={:?}, got={:?}",
                self.lexer.line, self.lexer.col, token, self.next_tok
            ),
        ));
        false
    }
}

impl<'l> From<&'l str> for Parser<'l> {
    fn from(input: &'l str) -> Self {
        Self::from(Lexer::new(input))
    }
}

impl<'l> From<&'l String> for Parser<'l> {
    fn from(input: &'l String) -> Self {
        Self::from(input.as_str())
    }
}

impl<'l> From<Lexer<'l>> for Parser<'l> {
    fn from(lexer: Lexer<'l>) -> Self {
        let mut parser = Parser {
            lexer,
            curr_tok: Token::Illegal,
            next_tok: Token::Illegal,
            errors: vec![],
        };
        parser.advance();
        parser.advance();
        parser
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_let_statements() {
        let input = "//
// let 5;
let x: int = 5;
let y: int = 10;
let foo: int = 99999;
";
        let program = Program::parse(input);
        let expected = vec![
            Stmt::Let(
                Ident(String::from("x")),
                Type::Int,
                Expr::Literal(Literal::Int(5)),
            ),
            Stmt::Let(
                Ident(String::from("y")),
                Type::Int,
                Expr::Literal(Literal::Int(10)),
            ),
            Stmt::Let(
                Ident(String::from("foo")),
                Type::Int,
                Expr::Literal(Literal::Int(99999)),
            ),
        ];

        assert_eq!(expected, program.statements);
    }

    #[test]
    fn test_return_statements() {
        let input = "
return 7;
return 100;
return 999999;
";
        let program = Program::parse(input);

        for (i, stmt) in program.iter().enumerate() {
            if !matches!(stmt, Stmt::Return(_)) {
                panic!("[{i}] expected=Stmt::Return, got={stmt:?}");
            }
        }
    }

    #[test]
    fn test_identifier_expressions() {
        let input = "foo; bar; foobar;";
        let program = Program::parse(input);
        let expected = vec![
            Stmt::Expr(Expr::Ident(Ident(String::from("foo")))),
            Stmt::Expr(Expr::Ident(Ident(String::from("bar")))),
            Stmt::Expr(Expr::Ident(Ident(String::from("foobar")))),
        ];

        assert_eq!(expected, program.statements);
    }

    #[test]
    fn test_literal_expressions() {
        let input = r#"5; 10.0; false; "foo";"#;
        let program = Program::parse(input);
        let expected = vec![
            Stmt::Expr(Expr::Literal(Literal::Int(5))),
            Stmt::Expr(Expr::Literal(Literal::Float(10.0))),
            Stmt::Expr(Expr::Literal(Literal::Bool(false))),
            Stmt::Expr(Expr::Literal(Literal::Str(String::from("foo")))),
        ];

        assert_eq!(expected, program.statements);
    }

    #[test]
    fn test_prefix_expressions() {
        let input = "!5; -5;";
        let program = Program::parse(input);
        let expected = vec![
            Stmt::Expr(Expr::Prefix(
                Prefix::Not,
                Box::new(Expr::Literal(Literal::Int(5))),
            )),
            Stmt::Expr(Expr::Prefix(
                Prefix::Minus,
                Box::new(Expr::Literal(Literal::Int(5))),
            )),
        ];

        assert_eq!(expected, program.statements);
    }

    #[test]
    fn test_infix_expressions() {
        let input = "
3 + 2;
5 - 2;
3 * 2;
6 / 2;
7 % 2;
3 > 2;
3 < 2;
4 >= 2;
4 <= 2;
4 == 4;
4 != 4;
";
        let program = Program::parse(input);
        let expected = vec![
            Stmt::Expr(Expr::Infix(
                Infix::Plus,
                Box::new(Expr::Literal(Literal::Int(3))),
                Box::new(Expr::Literal(Literal::Int(2))),
            )),
            Stmt::Expr(Expr::Infix(
                Infix::Minus,
                Box::new(Expr::Literal(Literal::Int(5))),
                Box::new(Expr::Literal(Literal::Int(2))),
            )),
            Stmt::Expr(Expr::Infix(
                Infix::Mul,
                Box::new(Expr::Literal(Literal::Int(3))),
                Box::new(Expr::Literal(Literal::Int(2))),
            )),
            Stmt::Expr(Expr::Infix(
                Infix::Div,
                Box::new(Expr::Literal(Literal::Int(6))),
                Box::new(Expr::Literal(Literal::Int(2))),
            )),
            Stmt::Expr(Expr::Infix(
                Infix::Mod,
                Box::new(Expr::Literal(Literal::Int(7))),
                Box::new(Expr::Literal(Literal::Int(2))),
            )),
            Stmt::Expr(Expr::Infix(
                Infix::GT,
                Box::new(Expr::Literal(Literal::Int(3))),
                Box::new(Expr::Literal(Literal::Int(2))),
            )),
            Stmt::Expr(Expr::Infix(
                Infix::LT,
                Box::new(Expr::Literal(Literal::Int(3))),
                Box::new(Expr::Literal(Literal::Int(2))),
            )),
            Stmt::Expr(Expr::Infix(
                Infix::GTE,
                Box::new(Expr::Literal(Literal::Int(4))),
                Box::new(Expr::Literal(Literal::Int(2))),
            )),
            Stmt::Expr(Expr::Infix(
                Infix::LTE,
                Box::new(Expr::Literal(Literal::Int(4))),
                Box::new(Expr::Literal(Literal::Int(2))),
            )),
            Stmt::Expr(Expr::Infix(
                Infix::EQ,
                Box::new(Expr::Literal(Literal::Int(4))),
                Box::new(Expr::Literal(Literal::Int(4))),
            )),
            Stmt::Expr(Expr::Infix(
                Infix::NEQ,
                Box::new(Expr::Literal(Literal::Int(4))),
                Box::new(Expr::Literal(Literal::Int(4))),
            )),
        ];

        for (i, actual) in program.iter().enumerate() {
            assert_eq!(&expected[i], actual);
        }
    }

    #[test]
    fn test_operator_precedence() {
        let tests = vec![
            ("-a * b;", "((-a) * b);"),
            ("!-a;", "(!(-a));"),
            ("a + b + c;", "((a + b) + c);"),
            ("a + b - c;", "((a + b) - c);"),
            ("a * b * c;", "((a * b) * c);"),
            ("a * b / c;", "((a * b) / c);"),
            ("a + b / c;", "(a + (b / c));"),
            ("a + b * c + d / e - f;", "(((a + (b * c)) + (d / e)) - f);"),
            ("5 > 4 == 3 < 4;", "((5 > 4) == (3 < 4));"),
            ("5 < 4 != 3 > 4;", "((5 < 4) != (3 > 4));"),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5;",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)));",
            ),
        ];

        for (input, expected) in tests {
            let actual = Program::parse(input)[0].to_string();
            assert_eq!(expected, actual);
        }
    }
}
