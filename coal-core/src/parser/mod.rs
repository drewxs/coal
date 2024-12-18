pub mod error;
pub mod precedence;

#[cfg(test)]
mod tests;

use crate::{
    Comment, Expr, Ident, IfExpr, Infix, Lexer, LexicalToken, Literal, Prefix, Stmt, Token, Type,
    Var,
};

pub use error::{ParserError, ParserErrorKind};
pub use precedence::Precedence;

#[derive(Clone, Debug)]
pub struct Parser {
    pub lexer: Lexer,
    pub curr_node: LexicalToken,
    pub next_node: LexicalToken,
    pub errors: Vec<ParserError>,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        let mut parser = Parser {
            lexer,
            curr_node: LexicalToken::default(),
            next_node: LexicalToken::default(),
            errors: vec![],
        };
        parser.advance();
        parser.advance();
        parser
    }

    pub fn parse(&mut self) -> Vec<Stmt> {
        let mut stmts = vec![];
        while !matches!(
            self.curr_node,
            LexicalToken {
                token: Token::EOF,
                ..
            }
        ) {
            if let Some(stmt) = self.parse_stmt() {
                stmts.push(stmt);
            }
            self.advance();
        }
        stmts
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
        self.next_node = self
            .lexer
            .next()
            .unwrap_or(LexicalToken::new(Token::EOF, self.next_node.span));
    }

    fn consume(&mut self, token: Token) {
        if self.next_node.token == token {
            self.advance();
        }
    }

    fn error(&mut self, kind: ParserErrorKind) {
        self.errors
            .push(ParserError::new(kind, self.curr_node.span));
    }

    fn expect_next(&mut self, token: Token) -> Option<()> {
        if self.next_node.token == token {
            self.advance();
            return Some(());
        }
        self.error(ParserErrorKind::UnexpectedToken {
            expected: token,
            got: self.next_node.token.clone(),
        });
        None
    }

    fn parse_stmt(&mut self) -> Option<Stmt> {
        match &self.curr_node.token {
            Token::Let => self.parse_let_stmt(),
            Token::Ident(_) => match self.next_node.token {
                Token::Assign => self.parse_assign_stmt(),
                Token::AddAssign => self.parse_op_assign_stmt(Infix::Add),
                Token::SubAssign => self.parse_op_assign_stmt(Infix::Sub),
                Token::MulAssign => self.parse_op_assign_stmt(Infix::Mul),
                Token::DivAssign => self.parse_op_assign_stmt(Infix::Div),
                Token::RemAssign => self.parse_op_assign_stmt(Infix::Rem),
                _ => self.parse_expr_stmt(),
            },
            Token::Return => self.parse_ret_stmt(),
            Token::Comment(c) => Some(Stmt::Comment(Comment(c.clone()))),
            Token::NewLine => Some(Stmt::Newline),
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
        let ident_span = self.curr_node.span;

        let mut t = if self.next_node.token == Token::Colon {
            self.advance();
            match &self.next_node.token {
                Token::Ident(name) => match name.as_str() {
                    "Fn" => {
                        self.advance();
                        self.parse_fn_type()
                    }
                    _ => {
                        self.advance();
                        Type::try_from(&self.curr_node.token).ok()
                    }
                },
                _ => None,
            }
        } else {
            None
        };

        self.expect_next(Token::Assign)?;
        self.advance();

        let expr = self.parse_expr(Precedence::Lowest)?;

        if t.is_none() {
            t = Type::try_from(&expr).ok();
        }

        if let Some(t) = t {
            self.consume(Token::Semicolon);
            Some(Stmt::Let(ident, t, expr))
        } else {
            self.errors.push(ParserError::new(
                ParserErrorKind::TypeAnnotationsNeeded,
                ident_span,
            ));
            Some(Stmt::Let(ident, Type::Unknown, expr))
        }
    }

    fn parse_assign_stmt(&mut self) -> Option<Stmt> {
        let ident = Ident::try_from(&self.curr_node.token).ok()?;
        self.advance();
        self.advance();
        self.parse_expr(Precedence::Lowest).map(|expr| {
            self.consume(Token::Semicolon);
            Stmt::Assign(ident, expr)
        })
    }

    fn parse_op_assign_stmt(&mut self, op: Infix) -> Option<Stmt> {
        let ident = Ident::try_from(&self.curr_node.token).ok()?;
        self.advance();
        self.advance();
        self.parse_expr(Precedence::Lowest).map(|expr| {
            self.consume(Token::Semicolon);
            match op {
                Infix::Add => Stmt::AddAssign(ident, expr),
                Infix::Sub => Stmt::SubAssign(ident, expr),
                Infix::Mul => Stmt::MulAssign(ident, expr),
                Infix::Div => Stmt::DivAssign(ident, expr),
                Infix::Rem => Stmt::RemAssign(ident, expr),
                _ => unreachable!(),
            }
        })
    }

    fn parse_ret_stmt(&mut self) -> Option<Stmt> {
        self.advance();
        let expr = self.parse_expr(Precedence::Lowest).map(Stmt::Return);
        self.consume(Token::Semicolon);
        expr
    }

    fn parse_expr(&mut self, precedence: Precedence) -> Option<Expr> {
        let LexicalToken { token, span } = &self.curr_node;
        let mut lhs = match token {
            Token::Ident(_) => Ident::try_from(&self.curr_node.token)
                .map(|ident| Expr::Ident(ident, *span))
                .ok(),
            Token::I64(i) => Some(Expr::Literal(Literal::I64(*i), *span)),
            Token::F64(f) => Some(Expr::Literal(Literal::F64(*f), *span)),
            Token::Str(s) => Some(Expr::Literal(Literal::Str(s.clone()), *span)),
            Token::Bool(b) => Some(Expr::Literal(Literal::Bool(*b), *span)),
            Token::Bang | Token::Add | Token::Sub => self.parse_prefix_expr(),
            Token::Lparen => self.parse_grouped_expr(),
            Token::If => self.parse_if_expr(),
            Token::While => self.parse_while_expr(),
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
                Token::Add
                | Token::Sub
                | Token::Mul
                | Token::Div
                | Token::IntDiv
                | Token::Rem
                | Token::EQ
                | Token::NEQ
                | Token::GT
                | Token::GTE
                | Token::LT
                | Token::LTE => {
                    lhs = self.parse_infix_expr(&lhs?);
                }
                Token::Lparen => {
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
        let (start, _) = self.curr_node.span;

        let prefix = Prefix::try_from(&self.curr_node.token).ok()?;
        self.advance();

        self.parse_expr(Precedence::Prefix).map(|expr| {
            let (_, end) = expr.span();
            Expr::Prefix(prefix, Box::new(expr.clone()), (start, end))
        })
    }

    fn parse_infix_expr(&mut self, lhs: &Expr) -> Option<Expr> {
        self.advance();

        let infix = Infix::try_from(&self.curr_node.token).ok()?;
        let prec = Precedence::from(&self.curr_node.token);
        self.advance();

        let rhs = self.parse_expr(prec)?;

        let (start, _) = lhs.span();
        let (_, end) = rhs.span();

        Some(Expr::Infix(
            infix,
            Box::new(lhs.clone()),
            Box::new(rhs),
            (start, end),
        ))
    }

    fn parse_call_expr(&mut self, func: &Expr) -> Option<Expr> {
        let (start, _) = self.curr_node.span;
        self.advance();

        let args = self.parse_expr_list(Token::Rparen)?;
        let (_, end) = self.curr_node.span;

        Some(Expr::Call {
            func: Box::new(func.clone()),
            args,
            span: (start, end),
        })
    }

    fn parse_grouped_expr(&mut self) -> Option<Expr> {
        self.advance();
        let expr = self.parse_expr(Precedence::Lowest);
        self.expect_next(Token::Rparen)?;
        expr
    }

    fn parse_if_expr(&mut self) -> Option<Expr> {
        let (start, _) = self.curr_node.span;
        self.advance();

        let cond = self.parse_expr(Precedence::Lowest)?;
        self.expect_next(Token::Lbrace)?;

        let then = self.parse_block_stmt();

        let mut elifs = vec![];
        while self.next_node.token == Token::Elif {
            self.advance();
            self.advance();

            let cond = self.parse_expr(Precedence::Lowest)?;
            self.expect_next(Token::Lbrace)?;

            elifs.push(IfExpr {
                cond: Box::new(cond),
                then: self.parse_block_stmt(),
            });
        }

        let mut alt = None;
        if self.next_node.token == Token::Else {
            self.advance();
            self.expect_next(Token::Lbrace)?;
            alt = Some(self.parse_block_stmt());
        }

        let (_, end) = self.curr_node.span;

        Some(Expr::If {
            cond: Box::new(cond),
            then,
            elifs,
            alt,
            span: (start, end),
        })
    }

    fn parse_while_expr(&mut self) -> Option<Expr> {
        let (start, _) = self.curr_node.span;
        self.advance();

        let cond = self.parse_expr(Precedence::Lowest)?;
        self.expect_next(Token::Lbrace)?;

        let body = self.parse_block_stmt();
        let (_, end) = self.curr_node.span;

        Some(Expr::While {
            cond: Box::new(cond),
            body,
            span: (start, end),
        })
    }

    fn parse_fn_expr(&mut self) -> Option<Expr> {
        let (start, _) = self.curr_node.span;
        self.advance();

        let ident = Ident::try_from(&self.curr_node.token).ok()?;
        self.expect_next(Token::Lparen)?;
        self.advance();

        let args = self.parse_decl_args()?;
        self.expect_next(Token::Arrow)?;
        self.advance();

        let ret_t = match &self.curr_node.token {
            Token::Ident(s) if s == "Fn" => self.parse_fn_type()?,
            _ => Type::try_from(&self.curr_node.token).ok()?,
        };
        self.advance();

        let body = self.parse_block_stmt();
        let (_, end) = self.curr_node.span;

        Some(Expr::Fn {
            name: ident.name(),
            args,
            ret_t,
            body,
            span: (start, end),
        })
    }

    fn parse_decl_args(&mut self) -> Option<Vec<Var>> {
        let mut args = vec![];

        while let Token::Ident(name) = self.curr_node.token.clone() {
            self.expect_next(Token::Colon)?;
            self.advance();

            let t = match &self.curr_node.token {
                Token::Ident(s) if s == "Fn" => self.parse_fn_type()?,
                _ => Type::try_from(&self.curr_node.token).ok()?,
            };

            self.consume(Token::Comma);
            self.advance();

            args.push(Var { name, t });
        }

        Some(args)
    }

    fn parse_fn_type(&mut self) -> Option<Type> {
        self.advance();
        self.advance();

        let mut args = vec![];

        while let Token::Ident(_) = self.curr_node.token.clone() {
            let t = Type::try_from(&self.curr_node.token).ok()?;
            self.consume(Token::Comma);
            self.advance();
            args.push(t);
        }

        self.advance();
        self.advance();

        let ret_t = Type::try_from(&self.curr_node.token).ok()?;

        Some(Type::Fn(args, Box::new(ret_t)))
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
            self.advance();
            self.advance();
            let expr = self.parse_expr(Precedence::Lowest)?;
            list.push(expr);
        }

        self.expect_next(end_tok).map(|_| list)
    }
}

impl From<&str> for Parser {
    fn from(input: &str) -> Self {
        Self::new(Lexer::new(input))
    }
}

impl From<&String> for Parser {
    fn from(input: &String) -> Self {
        Self::from(input.as_str())
    }
}
