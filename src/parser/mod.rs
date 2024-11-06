pub mod error;
pub mod precedence;

pub use error::ParserError;
use error::ParserErrorKind;
pub use precedence::Precedence;

use crate::{
    ast::{Expr, Ident, IfExpr, Infix, Literal, Prefix, Stmt, Type},
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
            panic!("{e}");
        }
    }

    fn advance(&mut self) {
        self.curr_tok = self.next_tok.clone();
        self.next_tok = self.lexer.next_tok();
    }

    fn advance_n(&mut self, n: usize) {
        for _ in 0..n {
            self.curr_tok = self.next_tok.clone();
            self.next_tok = self.lexer.next_tok();
        }
    }

    fn consume(&mut self, token: Token) {
        if self.next_tok == token {
            self.advance();
        }
    }

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

    fn parse_stmt(&mut self) -> Option<Stmt> {
        match self.curr_tok {
            Token::Let => self.parse_let_stmt(),
            Token::Return => self.parse_return_stmt(),
            _ => self.parse_expr_stmt(),
        }
    }

    fn parse_block_stmt(&mut self) -> Vec<Stmt> {
        self.advance();
        let mut block = vec![];
        while self.curr_tok != Token::Rbrace && self.curr_tok != Token::EOF {
            if let Some(stmt) = self.parse_stmt() {
                block.push(stmt)
            }
            self.advance();
        }
        block
    }

    fn parse_let_stmt(&mut self) -> Option<Stmt> {
        if let Token::Ident(_) = self.next_tok {
            self.advance();
        } else {
            return None;
        }

        let ident = Ident::try_from(&self.curr_tok).ok()?;
        let mut t = if self.next_tok == Token::Colon {
            self.advance_n(2);
            Type::try_from(&self.curr_tok).ok()
        } else {
            None
        };

        if !self.expect_next_tok(Token::Assign) {
            return None;
        }
        self.advance();

        let expr = self.parse_expr(Precedence::Lowest)?;

        if t.is_none() {
            t = Type::try_from(&expr).ok();
        }

        t.map(|t| {
            self.consume(Token::Semicolon);
            Stmt::Let(ident, t, expr)
        })
    }

    fn parse_return_stmt(&mut self) -> Option<Stmt> {
        self.advance();
        let expr = self.parse_expr(Precedence::Lowest).map(Stmt::Return);
        self.consume(Token::Semicolon);
        expr
    }

    fn parse_expr(&mut self, precedence: Precedence) -> Option<Expr> {
        let mut lhs = match &self.curr_tok {
            Token::Ident(_) => Ident::try_from(&self.curr_tok).map(Expr::Ident).ok(),
            Token::Int(i) => Some(Expr::Literal(Literal::Int(*i))),
            Token::Float(f) => Some(Expr::Literal(Literal::Float(*f))),
            Token::Str(s) => Some(Expr::Literal(Literal::Str(s.clone()))),
            Token::Bool(b) => Some(Expr::Literal(Literal::Bool(*b))),
            Token::Bang | Token::Plus | Token::Minus => self.parse_prefix_expr(),
            Token::Lparen => self.parse_grouped_expr(),
            Token::If => self.parse_if_expr(),
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

    fn parse_grouped_expr(&mut self) -> Option<Expr> {
        self.advance();
        let expr = self.parse_expr(Precedence::Lowest);
        if !self.expect_next_tok(Token::Rparen) {
            return None;
        }
        expr
    }

    fn parse_if_expr(&mut self) -> Option<Expr> {
        self.advance();
        let condition = self.parse_expr(Precedence::Lowest)?;

        if !self.expect_next_tok(Token::Lbrace) {
            return None;
        }

        let consequence = self.parse_block_stmt();
        self.advance();

        let mut else_ifs = vec![];
        let mut alternative = None;

        while self.curr_tok == Token::Elif {
            self.advance();
            let condition = self.parse_expr(Precedence::Lowest)?;

            if !self.expect_next_tok(Token::Lbrace) {
                return None;
            }

            else_ifs.push(IfExpr {
                condition: Box::new(condition),
                consequence: self.parse_block_stmt(),
            });
        }

        if self.next_tok == Token::Else {
            self.advance();
            if !self.expect_next_tok(Token::Lbrace) {
                return None;
            }
            alternative = Some(self.parse_block_stmt());
        }

        Some(Expr::If {
            condition: Box::new(condition),
            consequence,
            else_ifs,
            alternative,
        })
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
        parser.advance_n(2);
        parser
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_let_statements() {
        let input = vec![
            "let x: int = 5;",
            "let y: int = 10;",
            "let z: int = 99999;",
            "let foo: Foo = 0;",
        ];
        let program = Program::parse_lines(&input);
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
                Ident(String::from("z")),
                Type::Int,
                Expr::Literal(Literal::Int(99999)),
            ),
            Stmt::Let(
                Ident(String::from("foo")),
                Type::UserDefined(String::from("Foo")),
                Expr::Literal(Literal::Int(0)),
            ),
        ];

        assert_eq!(expected, program.statements);
    }

    #[test]
    fn test_let_statements_inference() {
        let input = r#"
let x = 5;
let y = 5.0;
let z = "hello";
"#;
        let program = Program::parse(input);
        let expected = vec![
            Stmt::Let(
                Ident(String::from("x")),
                Type::Int,
                Expr::Literal(Literal::Int(5)),
            ),
            Stmt::Let(
                Ident(String::from("y")),
                Type::Float,
                Expr::Literal(Literal::Float(5.0)),
            ),
            Stmt::Let(
                Ident(String::from("z")),
                Type::String,
                Expr::Literal(Literal::Str(String::from("hello"))),
            ),
        ];

        assert_eq!(expected, program.statements);
    }

    #[test]
    fn test_return_statements() {
        let input = vec!["return 7;", "return 100;", "return 999999;"];
        let program = Program::parse_lines(&input);

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
        let input = "!5; -5; !true; !false;";
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
            Stmt::Expr(Expr::Prefix(
                Prefix::Not,
                Box::new(Expr::Literal(Literal::Bool(true))),
            )),
            Stmt::Expr(Expr::Prefix(
                Prefix::Not,
                Box::new(Expr::Literal(Literal::Bool(false))),
            )),
        ];

        assert_eq!(expected, program.statements);
    }

    #[test]
    fn test_infix_expressions() {
        let input = vec![
            "3 + 2",
            "5 - 2",
            "3 * 2",
            "6 / 2",
            "7 % 2",
            "3 > 2",
            "3 < 2",
            "4 >= 2",
            "4 <= 2",
            "4 == 4",
            "4 != 4",
            "true == true",
            "false == false",
            "true != false",
        ];
        let program = Program::parse_lines(&input);
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
            Stmt::Expr(Expr::Infix(
                Infix::EQ,
                Box::new(Expr::Literal(Literal::Bool(true))),
                Box::new(Expr::Literal(Literal::Bool(true))),
            )),
            Stmt::Expr(Expr::Infix(
                Infix::EQ,
                Box::new(Expr::Literal(Literal::Bool(false))),
                Box::new(Expr::Literal(Literal::Bool(false))),
            )),
            Stmt::Expr(Expr::Infix(
                Infix::NEQ,
                Box::new(Expr::Literal(Literal::Bool(true))),
                Box::new(Expr::Literal(Literal::Bool(false))),
            )),
        ];

        for (i, actual) in program.iter().enumerate() {
            assert_eq!(&expected[i], actual);
        }
    }

    #[test]
    fn test_operator_precedence() {
        let tests = vec![
            ("-a * b", "((-a) * b)"),
            ("!-a", "(!(-a))"),
            ("a + b + c", "((a + b) + c)"),
            ("a + b - c", "((a + b) - c)"),
            ("a * b * c", "((a * b) * c)"),
            ("a * b / c", "((a * b) / c)"),
            ("a + b / c", "(a + (b / c))"),
            ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
            ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
            ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
            ("true", "true"),
            ("false", "false"),
            ("1 < 2 == true", "((1 < 2) == true)"),
            ("1 > 2 == false", "((1 > 2) == false)"),
            ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
            ("(1 + 1) * 2", "((1 + 1) * 2)"),
            ("1 / (2 + 2)", "(1 / (2 + 2))"),
            ("-(1 + 2)", "(-(1 + 2))"),
            ("!(true == true)", "(!(true == true))"),
        ];

        for (input, expected) in tests {
            assert_eq!(expected, Program::read_line(input));
        }
    }

    #[test]
    fn test_if_expression() {
        let input = "if x < y { return x }";
        let program = Program::parse(input);
        let expected = Stmt::Expr(Expr::If {
            condition: Box::new(Expr::Infix(
                Infix::LT,
                Box::new(Expr::Ident(Ident(String::from("x")))),
                Box::new(Expr::Ident(Ident(String::from("y")))),
            )),
            consequence: vec![Stmt::Return(Expr::Ident(Ident(String::from("x"))))],
            else_ifs: vec![],
            alternative: None,
        });

        assert_eq!(expected, program.statements[0]);
    }

    #[test]
    fn test_elif_expression() {
        let input = r#"
if x < y {
    return x;
} elif x > y {
    return y;
} else {
    return z;
}"#;
        let expected = r#"if (x < y) {
    return x;
} elif (x > y) {
    return y;
} else {
    return z;
}
"#;
        let program = Program::from(input);
        assert_eq!(expected, program.to_string());
    }
}
