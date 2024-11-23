pub mod error;
pub mod precedence;

#[cfg(test)]
mod tests;

pub use error::{ParserError, ParserErrorKind};
pub use precedence::Precedence;

use crate::{
    Expr, Ident, IfExpr, Infix, Lexer, Literal, Node, Prefix, Program, Stmt, Token, Type, Var,
};

#[derive(Clone, Debug)]
pub struct Parser<'l> {
    pub lexer: Lexer<'l>,
    pub curr_node: Node,
    pub next_node: Node,
    pub errors: Vec<ParserError>,
}

impl<'l> Parser<'l> {
    pub fn new(lexer: Lexer<'l>) -> Self {
        let mut parser = Parser {
            lexer,
            curr_node: Node::default(),
            next_node: Node::default(),
            errors: vec![],
        };
        parser.advance_n(2);
        parser
    }

    pub fn parse(&mut self) -> Program {
        let mut program = Program::default();
        while !matches!(
            self.curr_node,
            Node {
                token: Token::EOF,
                ..
            }
        ) {
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
        self.curr_node = self.next_node.clone();
        self.next_node = self.lexer.next_node();
    }

    fn advance_n(&mut self, n: usize) {
        for _ in 0..n {
            self.curr_node = self.next_node.clone();
            self.next_node = self.lexer.next_node();
        }
    }

    fn consume(&mut self, token: Token) {
        if self.next_node.token == token {
            self.advance();
        }
    }

    fn error(&mut self, kind: ParserErrorKind) {
        let Node {
            pos: (line, col), ..
        } = self.curr_node;
        self.errors.push(ParserError::new(kind, line, col));
    }

    fn expect_next(&mut self, token: Token) -> bool {
        if self.next_node.token == token {
            self.advance();
            return true;
        }
        self.error(ParserErrorKind::UnexpectedToken {
            expected: token,
            got: self.next_node.token.clone(),
        });
        false
    }

    fn parse_stmt(&mut self) -> Option<Stmt> {
        match self.curr_node.token {
            Token::Let => self.parse_let_stmt(),
            Token::Return => self.parse_ret_stmt(),
            _ => self.parse_expr_stmt(),
        }
    }

    fn parse_block_stmt(&mut self) -> Vec<Stmt> {
        self.advance();
        let mut block = vec![];
        while self.curr_node.token != Token::Rbrace && self.curr_node.token != Token::EOF {
            if let Some(stmt) = self.parse_stmt() {
                block.push(stmt)
            }
            self.advance();
        }
        block
    }

    fn parse_let_stmt(&mut self) -> Option<Stmt> {
        if let Token::Ident(_) = self.next_node.token {
            self.advance();
        } else {
            return None;
        }

        let ident = Ident::try_from(&self.curr_node.token).ok()?;
        let mut t = if self.next_node.token == Token::Colon {
            self.advance_n(2);
            Type::try_from(&self.curr_node.token).ok()
        } else {
            None
        };

        if !self.expect_next(Token::Assign) {
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

    fn parse_ret_stmt(&mut self) -> Option<Stmt> {
        self.advance();
        let expr = self.parse_expr(Precedence::Lowest).map(Stmt::Return);
        self.consume(Token::Semicolon);
        expr
    }

    fn parse_expr(&mut self, precedence: Precedence) -> Option<Expr> {
        let mut lhs = match &self.curr_node.token {
            Token::Ident(_) => Ident::try_from(&self.curr_node.token).map(Expr::Ident).ok(),
            Token::Int(i) => Some(Expr::Literal(Literal::Int(*i))),
            Token::Float(f) => Some(Expr::Literal(Literal::Float(*f))),
            Token::Str(s) => Some(Expr::Literal(Literal::Str(s.clone()))),
            Token::Bool(b) => Some(Expr::Literal(Literal::Bool(*b))),
            Token::Bang | Token::Plus | Token::Minus => self.parse_prefix_expr(),
            Token::Lparen => self.parse_grouped_expr(),
            Token::If => self.parse_if_expr(),
            Token::Fn => self.parse_fn_expr(),
            _ => {
                self.error(ParserErrorKind::SyntaxError(self.curr_node.token.clone()));
                return None;
            }
        };

        while self.next_node.token != Token::Semicolon
            && precedence < Precedence::from(&self.next_node.token)
        {
            match self.next_node.token {
                Token::Plus
                | Token::Minus
                | Token::Asterisk
                | Token::Slash
                | Token::DoubleSlash
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
                Token::Lparen => {
                    self.advance();
                    lhs = self.parse_call_expr(&lhs?);
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
        let prefix = Prefix::try_from(&self.curr_node.token).ok()?;
        self.advance();
        self.parse_expr(Precedence::Prefix)
            .map(|expr| Expr::Prefix(prefix, Box::new(expr)))
    }

    fn parse_infix_expr(&mut self, lhs: &Expr) -> Option<Expr> {
        let infix = Infix::try_from(&self.curr_node.token).ok()?;
        let prec = Precedence::from(&self.curr_node.token);
        self.advance();
        let rhs = self.parse_expr(prec)?;
        Some(Expr::Infix(infix, Box::new(lhs.clone()), Box::new(rhs)))
    }

    fn parse_call_expr(&mut self, func: &Expr) -> Option<Expr> {
        Some(Expr::Call {
            func: Box::new(func.clone()),
            args: self.parse_expr_list(Token::Rparen)?,
        })
    }

    fn parse_grouped_expr(&mut self) -> Option<Expr> {
        self.advance();
        let expr = self.parse_expr(Precedence::Lowest);
        if !self.expect_next(Token::Rparen) {
            return None;
        }
        expr
    }

    fn parse_if_expr(&mut self) -> Option<Expr> {
        self.advance();
        let cond = self.parse_expr(Precedence::Lowest)?;

        if !self.expect_next(Token::Lbrace) {
            return None;
        }

        let then = self.parse_block_stmt();

        let mut elifs = vec![];
        let mut alt = None;

        while self.next_node.token == Token::Elif {
            self.advance();
            self.advance();
            let cond = self.parse_expr(Precedence::Lowest)?;

            if !self.expect_next(Token::Lbrace) {
                return None;
            }

            elifs.push(IfExpr {
                cond: Box::new(cond),
                then: self.parse_block_stmt(),
            });
        }

        if self.next_node.token == Token::Else {
            self.advance();
            if !self.expect_next(Token::Lbrace) {
                return None;
            }
            alt = Some(self.parse_block_stmt());
        }

        Some(Expr::If {
            cond: Box::new(cond),
            then,
            elifs,
            alt,
        })
    }

    fn parse_fn_expr(&mut self) -> Option<Expr> {
        self.advance();

        let ident = Ident::try_from(&self.curr_node.token).ok()?;
        self.expect_next(Token::Lparen);
        self.advance();

        let args = self.parse_decl_args()?;
        self.expect_next(Token::Arrow);
        self.advance();

        let ret_t = Type::try_from(&self.curr_node.token).ok()?;
        self.advance();

        let body = self.parse_block_stmt();

        Some(Expr::Fn {
            name: ident.name(),
            args,
            ret_t,
            body,
        })
    }

    fn parse_decl_args(&mut self) -> Option<Vec<Var>> {
        let mut args = vec![];

        while let Token::Ident(name) = self.curr_node.token.clone() {
            self.expect_next(Token::Colon);
            self.advance();

            let t = Type::try_from(&self.curr_node.token).ok()?;
            self.consume(Token::Comma);
            self.advance();

            args.push(Var::new(name, t));
        }

        Some(args)
    }

    fn parse_expr_list(&mut self, end_tok: Token) -> Option<Vec<Expr>> {
        let mut list = vec![];

        if self.next_node.token == end_tok {
            self.advance();
            return Some(list);
        }

        self.advance();

        let expr = self.parse_expr(Precedence::Lowest)?;
        list.push(expr);

        while self.next_node.token == Token::Comma {
            self.advance_n(2);
            let expr = self.parse_expr(Precedence::Lowest)?;
            list.push(expr);
        }

        if self.expect_next(end_tok) {
            Some(list)
        } else {
            None
        }
    }
}

impl<'l> From<&'l str> for Parser<'l> {
    fn from(input: &'l str) -> Self {
        Self::new(Lexer::new(input))
    }
}

impl<'l> From<&'l String> for Parser<'l> {
    fn from(input: &'l String) -> Self {
        Self::from(input.as_str())
    }
}