use std::fmt;

use crate::{
    ast::{Expr, Ident, Literal, Stmt, Type},
    Lexer, Program, Token,
};

#[derive(Clone, Debug, PartialEq)]
enum Precedence {
    Lowest,
    // Equals,      // ==
    // Lessgreater, // > or <
    // Sum,         // +
    // Product,     // *
    // Prefix,      // -x or !x
    // Call,        // f(x)
    // Index,       // vec[index]
}

#[derive(Clone, Debug)]
pub enum ParserErrorKind {
    UnexpectedToken,
}

impl fmt::Display for ParserErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

#[derive(Clone, Debug)]
pub struct ParserError {
    pub kind: ParserErrorKind,
    pub msg: String,
}

impl ParserError {
    pub fn new(kind: ParserErrorKind, msg: String) -> Self {
        ParserError { kind, msg }
    }
}

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[{}] {}", self.kind, self.msg)
    }
}

#[derive(Clone, Debug)]
pub struct Parser<'l> {
    pub lexer: Lexer<'l>,
    pub curr_tok: Token,
    pub next_tok: Token,
    pub errors: Vec<ParserError>,
}

impl<'l> Parser<'l> {
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
        for error in self.errors.iter() {
            errors += &format!("\nparser error: {error}");
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

        let name = self.parse_ident()?;

        if !self.expect_next_tok(Token::Colon) {
            return None;
        }
        self.advance();

        let t = self.parse_type()?;

        if !self.expect_next_tok(Token::Assign) {
            return None;
        }
        self.advance();

        let expr = self.parse_expr()?;

        if self.next_tok != Token::Semicolon {
            self.advance();
        }

        Some(Stmt::Let(name, t, expr))
    }

    fn parse_return_stmt(&mut self) -> Option<Stmt> {
        self.advance();
        dbg!(&self.next_tok);
        self.parse_expr().map(Stmt::Return)
    }

    fn parse_expr_with_prec(&mut self, _: Precedence) -> Option<Expr> {
        match self.curr_tok.clone() {
            Token::Ident(_) => self.parse_ident().map(Expr::Ident),
            Token::Int(i) => Some(Expr::Literal(Literal::Int(i))),
            Token::Float(f) => Some(Expr::Literal(Literal::Float(f))),
            Token::Str(s) => Some(Expr::Literal(Literal::Str(s))),
            Token::Bool(b) => Some(Expr::Literal(Literal::Bool(b))),
            _ => None,
        }
        // let lhs = match self.curr_tok {
        //     Token::Ident(_) => self.parse_ident_expr(),
        //     Token::Int(_) => self.parse_int_expr(),
        //     _ => None,
        // };
        // dbg!(&prec);
        // lhs
    }

    fn parse_expr(&mut self) -> Option<Expr> {
        self.parse_expr_with_prec(Precedence::Lowest)
    }

    fn parse_expr_stmt(&mut self) -> Option<Stmt> {
        self.parse_expr().map(|expr| {
            self.expect_next_tok(Token::Semicolon);
            Stmt::Expr(expr)
        })
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

    fn expect_next_tok(&mut self, token: Token) -> bool {
        if self.next_tok == token {
            self.advance();
            return true;
        }
        self.err_next_tok(token);
        false
    }

    fn err_next_tok(&mut self, token: Token) {
        self.errors.push(ParserError::new(
            ParserErrorKind::UnexpectedToken,
            format!("expected={}, got={}", token, self.next_tok),
        ));
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
    fn test_parse_let_statements() {
        let input = r#"//
// let 5;
let x: int = 5;
let y: int = 10;
let foo: int = 99999;
"#;
        let mut parser = Parser::from(input);
        let program = parser.parse();
        parser.validate();

        dbg!(&parser.errors);

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
    fn test_parse_return_statements() {
        let input = r#"
return 7;
return 100;
return 999999;
"#;
        let mut parser = Parser::from(input);

        let program = parser.parse();
        parser.validate();

        for (i, stmt) in program.iter().enumerate() {
            if !matches!(stmt, Stmt::Return(_)) {
                panic!("[{i}] expected=Stmt::Return, got={stmt:?}");
            }
        }
    }
}
