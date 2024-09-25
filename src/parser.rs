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
pub struct Parser<'l> {
    pub lexer: Lexer<'l>,
    pub curr_tok: Token,
    pub next_tok: Token,
}

impl<'l> Parser<'l> {
    pub fn new(lexer: Lexer<'l>) -> Self {
        let mut parser = Parser {
            lexer,
            curr_tok: Token::Illegal,
            next_tok: Token::Illegal,
        };
        parser.advance();
        parser.advance();
        parser
    }

    pub fn parse(&mut self) -> Program {
        let mut program = Program::new();
        while self.curr_tok != Token::EOF {
            if let Some(stmt) = self.parse_stmt() {
                program.push(stmt);
            }
            self.advance();
        }
        program
    }

    fn advance(&mut self) {
        self.curr_tok = self.next_tok.clone();
        self.next_tok = self.lexer.next_tok();
    }

    fn parse_stmt(&mut self) -> Option<Stmt> {
        match self.curr_tok {
            Token::Let => self.parse_let_stmt(),
            _ => self.parse_expr_stmt(),
        }
    }

    fn parse_let_stmt(&mut self) -> Option<Stmt> {
        match self.next_tok {
            Token::Ident(_) => self.advance(),
            _ => return None,
        }

        let name = self.parse_ident()?;

        if !self.expect_next(Token::Colon) {
            return None;
        }
        self.advance();

        let t = self.parse_type()?;

        if !self.expect_next(Token::Assign) {
            return None;
        }
        self.advance();

        let expr = self.parse_expr(Precedence::Lowest)?;

        while self.curr_tok != Token::Semicolon {
            self.advance();
        }

        Some(Stmt::Let(name, t, expr))
    }

    fn parse_expr(&mut self, prec: Precedence) -> Option<Expr> {
        let lhs = match self.curr_tok {
            Token::Ident(_) => self.parse_ident_expr(),
            Token::Int(_) => self.parse_int_expr(),
            _ => None,
        };
        dbg!(&lhs);
        dbg!(&prec);
        lhs
    }

    fn parse_expr_stmt(&mut self) -> Option<Stmt> {
        match self.parse_expr(Precedence::Lowest) {
            Some(expr) => {
                self.expect_next(Token::Semicolon);
                Some(Stmt::Expr(expr))
            }
            None => None,
        }
    }

    fn parse_ident(&mut self) -> Option<Ident> {
        match &self.curr_tok {
            Token::Ident(ident) => Some(Ident(ident.clone())),
            _ => None,
        }
    }

    fn parse_ident_expr(&mut self) -> Option<Expr> {
        self.parse_ident().map(Expr::Ident)
    }

    fn parse_int_expr(&mut self) -> Option<Expr> {
        match self.curr_tok {
            Token::Int(i) => Some(Expr::Literal(Literal::Int(i))),
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

    fn expect_next(&mut self, token: Token) -> bool {
        if self.next_tok == token {
            self.advance();
            true
        } else {
            false
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_let_statements() {
        let input = "//
let x: int = 5;
let y: int = 10;
let foo: int = 99999;
";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse();
        if program.len() != 3 {
            panic!("Expected 3 statements, got {}", program.len());
        }

        assert_eq!(
            vec![
                Stmt::Let(
                    Ident(String::from("x")),
                    Type::Int,
                    Expr::Literal(Literal::Int(5))
                ),
                Stmt::Let(
                    Ident(String::from("y")),
                    Type::Int,
                    Expr::Literal(Literal::Int(10))
                ),
                Stmt::Let(
                    Ident(String::from("foo")),
                    Type::Int,
                    Expr::Literal(Literal::Int(99999))
                ),
            ],
            program
        );
    }
}
