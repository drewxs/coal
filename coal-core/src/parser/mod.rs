#[cfg(test)]
mod tests;

mod error;
pub mod precedence;
pub mod symbol_table;

use std::{cell::RefCell, rc::Rc};

use crate::{
    Comment, Expr, Ident, IfExpr, Infix, Lexer, LexicalToken, Literal, Prefix, Stmt, Token, Type,
    Var,
};

pub use error::{ParserError, ParserErrorKind};
pub use precedence::Precedence;
pub use symbol_table::SymbolTable;

#[derive(Clone, Debug, Default)]
pub struct Parser {
    pub lexer: Lexer,
    pub curr_node: LexicalToken,
    pub next_node: LexicalToken,
    pub symbol_table: Rc<RefCell<SymbolTable>>,
    pub errors: Vec<ParserError>,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        let mut parser = Parser {
            lexer,
            curr_node: LexicalToken::default(),
            next_node: LexicalToken::default(),
            symbol_table: Rc::new(RefCell::new(SymbolTable::default())),
            errors: vec![],
        };
        parser.advance();
        parser.advance();
        parser
    }

    pub fn new_with(&self, input: &str, symbol_table: Rc<RefCell<SymbolTable>>) -> Self {
        let mut parser = Parser {
            lexer: Lexer::new(input),
            curr_node: LexicalToken::default(),
            next_node: LexicalToken::default(),
            symbol_table: Rc::new(RefCell::new(SymbolTable::from(symbol_table))),
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
        if self.curr_node.token == token {
            self.advance();
        }
    }

    fn consume_next(&mut self, token: Token) {
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

        self.errors.push(ParserError::new(
            ParserErrorKind::UnexpectedToken(self.next_node.token.clone(), token),
            self.next_node.span,
        ));

        None
    }

    fn parse_block(&mut self) -> Vec<Stmt> {
        let curr_st = Rc::clone(&self.symbol_table);
        self.symbol_table = Rc::new(RefCell::new(SymbolTable::from(Rc::clone(
            &self.symbol_table,
        ))));
        let res = self.parse_stmts();
        self.symbol_table = curr_st;

        res
    }

    fn parse_block_in_scope(&mut self, scope: Rc<RefCell<SymbolTable>>) -> Vec<Stmt> {
        let curr_st = Rc::clone(&self.symbol_table);
        self.symbol_table = scope;
        let res = self.parse_stmts();
        self.symbol_table = curr_st;

        res
    }

    fn parse_stmts(&mut self) -> Vec<Stmt> {
        let mut block = vec![];
        while self.curr_node.token != Token::Rbrace && self.curr_node.token != Token::EOF {
            if let Some(stmt) = self.parse_stmt() {
                block.push(stmt)
            }
            self.advance();
        }

        block
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

    fn parse_let_stmt(&mut self) -> Option<Stmt> {
        if let Token::Ident(_) = self.next_node.token {
            self.advance();
        } else {
            return None;
        }

        let ident = Ident::try_from(&self.curr_node.token).ok()?;
        let ident_span = self.curr_node.span;

        let mut declared_t = match self.next_node.token {
            Token::Colon => {
                self.advance();
                match &self.next_node.token {
                    Token::Ident(_) => {
                        self.advance();
                        self.parse_type()
                    }
                    _ => None,
                }
            }
            _ => None,
        };

        self.expect_next(Token::Assign)?;
        self.advance();

        let expr = self.parse_expr(Precedence::Lowest)?;

        if let Expr::Ident(Ident(name), _, _) = &expr {
            if let Some(t) = self.symbol_table.borrow().get(name) {
                declared_t = Some(t.clone());
            } else if let Some(t) = self.symbol_table.borrow().get(&format!("__{name}__")) {
                declared_t = Some(t.clone());
            }
        } else if let Ok(inferred_t) = Type::try_from(&expr) {
            if let Some(t) = &declared_t {
                if !t.is_numeric() && !inferred_t.is_numeric() {
                    declared_t = Some(inferred_t);
                }
            } else {
                declared_t = Some(inferred_t);
            }
        }

        let t = declared_t.unwrap_or_else(|| {
            self.errors.push(ParserError::new(
                ParserErrorKind::TypeAnnotationsNeeded,
                ident_span,
            ));
            Type::Unknown
        });

        self.symbol_table.borrow_mut().set(ident.name(), t.clone());
        if let Type::Fn(_, ret_t) = &t {
            self.symbol_table
                .borrow_mut()
                .set(format!("__{}__", ident.name()), *ret_t.clone());
        }

        self.consume_next(Token::Semicolon);

        Some(Stmt::Let(ident, t, expr))
    }

    fn parse_assign_stmt(&mut self) -> Option<Stmt> {
        let ident = Ident::try_from(&self.curr_node.token).ok()?;
        self.advance();
        self.advance();
        self.parse_expr(Precedence::Lowest).map(|expr| {
            self.consume_next(Token::Semicolon);
            Stmt::Assign(ident, expr)
        })
    }

    fn parse_op_assign_stmt(&mut self, op: Infix) -> Option<Stmt> {
        let ident = Ident::try_from(&self.curr_node.token).ok()?;
        self.advance();
        self.advance();
        self.parse_expr(Precedence::Lowest).map(|expr| {
            self.consume_next(Token::Semicolon);
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
        self.consume_next(Token::Semicolon);
        expr
    }

    fn parse_expr(&mut self, precedence: Precedence) -> Option<Expr> {
        let LexicalToken { token, span } = &self.curr_node;

        let mut lhs = match token {
            Token::Ident(name) => Ident::try_from(&self.curr_node.token)
                .map(|ident| {
                    Expr::Ident(
                        ident,
                        self.symbol_table
                            .borrow()
                            .get(name)
                            .unwrap_or(Type::Unknown),
                        *span,
                    )
                })
                .ok(),
            Token::U32(i) => Some(Expr::Literal(Literal::U32(*i), *span)),
            Token::U64(i) => Some(Expr::Literal(Literal::U64(*i), *span)),
            Token::I32(i) => Some(Expr::Literal(Literal::I32(*i), *span)),
            Token::I64(i) => Some(Expr::Literal(Literal::I64(*i), *span)),
            Token::I128(i) => Some(Expr::Literal(Literal::I128(*i), *span)),
            Token::F32(f) => Some(Expr::Literal(Literal::F32(*f), *span)),
            Token::F64(f) => Some(Expr::Literal(Literal::F64(*f), *span)),
            Token::Str(s) => Some(Expr::Literal(Literal::Str(s.clone()), *span)),
            Token::Bool(b) => Some(Expr::Literal(Literal::Bool(*b), *span)),
            Token::Bang | Token::Add | Token::Sub => self.parse_prefix_expr(),
            Token::Lparen => self.parse_grouped_expr(),
            Token::Lbracket => self.parse_list_expr(),
            Token::If => self.parse_if_expr(),
            Token::While => self.parse_while_expr(),
            Token::Fn => self.parse_fn_expr(),
            Token::Nil => Some(Expr::Literal(Literal::Nil, *span)),
            _ => {
                self.error(ParserErrorKind::SyntaxError(self.curr_node.token.clone()));
                return None;
            }
        };

        if !matches!(
            lhs,
            Some(Expr::Ident(_, _, _))
                | Some(Expr::Literal(_, _))
                | Some(Expr::Prefix { .. })
                | Some(Expr::Infix(_, _, _, _))
                | Some(Expr::Call { .. })
                | Some(Expr::MethodCall { .. })
        ) {
            return lhs;
        }

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
                    if let Some(Expr::Ident(Ident(name), _, _)) = &lhs {
                        lhs = self.parse_call_expr(name.to_owned());
                    }
                }
                Token::Lbracket => {
                    self.advance();
                    lhs = self.parse_index_expr(&lhs?);
                }
                Token::Dot => {
                    lhs = self.parse_method_call(lhs?);
                }
                _ => break,
            }
        }

        lhs
    }

    fn parse_expr_stmt(&mut self) -> Option<Stmt> {
        self.parse_expr(Precedence::Lowest).map(|expr| {
            self.consume_next(Token::Semicolon);
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

    fn parse_call_expr(&mut self, name: String) -> Option<Expr> {
        let (start, _) = self.curr_node.span;
        self.advance();

        let args = self.parse_expr_list(Token::Rparen)?;
        let (_, end) = self.curr_node.span;
        let ret_t = self
            .symbol_table
            .borrow()
            .get(&format!("__{name}__"))
            .unwrap_or(Type::Unknown);

        Some(Expr::Call {
            name,
            args,
            ret_t,
            span: (start, end),
        })
    }

    fn parse_index_expr(&mut self, lhs: &Expr) -> Option<Expr> {
        let (start, _) = lhs.span();
        self.advance();

        let idx = self.parse_expr(Precedence::Lowest)?;
        self.expect_next(Token::Rbracket)?;

        let (_, end) = self.curr_node.span;

        Some(Expr::Index(
            Box::new(lhs.clone()),
            Box::new(idx),
            (start, end),
        ))
    }

    fn parse_expr_list(&mut self, end_tok: Token) -> Option<Vec<Expr>> {
        let mut list = vec![];

        if self.next_node.token == end_tok {
            self.advance();
            return Some(list);
        }

        self.advance();
        list.push(self.parse_expr(Precedence::Lowest)?);

        while self.next_node.token == Token::Comma {
            self.advance();
            self.advance();
            list.push(self.parse_expr(Precedence::Lowest)?);
        }
        self.expect_next(end_tok);

        Some(list)
    }

    fn parse_uniform_expr_list(&mut self, end_tok: Token) -> Option<(Vec<Expr>, Type)> {
        let mut list = vec![];

        if self.next_node.token == end_tok {
            self.advance();
            return Some((list, Type::Unknown));
        }

        self.advance();

        let expr = self.parse_expr(Precedence::Lowest)?;
        let t = Type::try_from(&expr).ok()?;
        list.push(expr);

        while self.next_node.token == Token::Comma {
            self.advance();
            self.advance();
            let expr = self.parse_expr(Precedence::Lowest)?;
            let curr_t = Type::try_from(&expr).ok()?;
            if curr_t != t {
                self.error(ParserErrorKind::TypeMismatch(t, curr_t));
                return None;
            }
            list.push(expr);
        }

        self.expect_next(end_tok)?;

        Some((list, t))
    }

    fn parse_method_call(&mut self, lhs: Expr) -> Option<Expr> {
        let (start, _) = self.curr_node.span;
        self.advance();
        self.advance();

        let method_name = if let Token::Ident(name) = &self.curr_node.token {
            name.clone()
        } else {
            self.error(ParserErrorKind::SyntaxError(self.curr_node.token.clone()));
            return None;
        };

        let mut args = vec![];
        if self.next_node.token == Token::Lparen {
            self.advance();
            args = self.parse_expr_list(Token::Rparen)?;
        }

        let lhs_t = Type::try_from(&lhs).unwrap_or(Type::Unknown);

        if let Some(method) = lhs_t.sig(&method_name) {
            if args.len() != method.args_t.len() {
                self.error(ParserErrorKind::InvalidArgumentsLength(
                    method.args_t.len(),
                    args.len(),
                ));
                return None;
            }

            let (_, end) = self.curr_node.span;

            Some(Expr::MethodCall {
                lhs: Box::new(lhs),
                name: method_name,
                args,
                ret_t: method.ret_t,
                span: (start, end),
            })
        } else {
            self.advance();
            self.advance();
            self.consume_next(Token::Lparen);

            let (_, end) = self.curr_node.span;

            self.errors.push(ParserError::new(
                ParserErrorKind::MethodNotFound(lhs_t, String::from(&method_name)),
                (start, end),
            ));

            None
        }
    }

    fn parse_grouped_expr(&mut self) -> Option<Expr> {
        self.advance();
        let expr = self.parse_expr(Precedence::Lowest);
        self.expect_next(Token::Rparen)?;
        expr
    }

    fn parse_list_expr(&mut self) -> Option<Expr> {
        let (start, _) = self.curr_node.span;
        let (items, t) = self.parse_uniform_expr_list(Token::Rbracket)?;
        let (_, end) = self.curr_node.span;

        Some(Expr::Literal(Literal::List(items, t), (start, end)))
    }

    fn parse_if_expr(&mut self) -> Option<Expr> {
        let (start, _) = self.curr_node.span;
        self.advance();

        let cond = self.parse_expr(Precedence::Lowest)?;
        self.expect_next(Token::Lbrace)?;
        self.advance();

        let then = self.parse_block();

        let mut elifs = vec![];
        while self.next_node.token == Token::Elif {
            self.advance();
            self.advance();

            let cond = self.parse_expr(Precedence::Lowest)?;
            self.expect_next(Token::Lbrace)?;
            self.advance();

            elifs.push(IfExpr {
                cond: Box::new(cond),
                then: self.parse_block(),
            });
        }

        let mut alt = None;
        if self.next_node.token == Token::Else {
            self.advance();
            self.expect_next(Token::Lbrace)?;
            self.advance();
            alt = Some(self.parse_block());
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
        self.advance();

        let body = self.parse_block();
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
        let args_t = args.iter().map(|arg| arg.t.clone()).collect();
        let declared_ret_t = self.parse_ret_type();
        self.advance();
        self.consume(Token::Lbrace);

        let mut st = SymbolTable::from(Rc::clone(&self.symbol_table));
        for arg in args.iter() {
            st.set(arg.name.clone(), arg.t.clone());
            if let Type::Fn(_, t) = &arg.t {
                st.set(format!("__{}__", arg.name), *t.clone());
            }
        }

        let body = self.parse_block_in_scope(Rc::new(RefCell::new(st)));
        let (_, end) = self.curr_node.span;

        let ret_t = declared_ret_t.unwrap_or_else(|| {
            if let Some(Stmt::Return(expr)) = body.last() {
                if let Ok(t) = Type::try_from(expr) {
                    return t;
                }
            }
            Type::Void
        });

        self.symbol_table
            .borrow_mut()
            .set(format!("__{}__", ident.name()), ret_t.clone());
        self.symbol_table
            .borrow_mut()
            .set(ident.name(), Type::Fn(args_t, Box::new(ret_t.clone())));

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

            let t = self.parse_type()?;

            self.consume_next(Token::Comma);
            self.advance();

            args.push(Var { name, t });
        }

        self.advance();

        Some(args)
    }

    fn parse_ret_type(&mut self) -> Option<Type> {
        match self.curr_node.token {
            Token::Lbrace | Token::EQ => None,
            _ => {
                if !matches!(self.curr_node.token, Token::Ident(_)) {
                    self.advance();
                }
                self.parse_type()
            }
        }
    }

    fn parse_type(&mut self) -> Option<Type> {
        match &self.curr_node.token {
            Token::Ident(s) => match s.as_str() {
                "Fn" => self.parse_fn_type(),
                "list" => self.parse_list_type(),
                _ => Type::try_from(&self.curr_node.token).ok(),
            },
            _ => None,
        }
    }

    fn parse_fn_type(&mut self) -> Option<Type> {
        self.advance();
        self.advance();

        let mut args = vec![];
        while let Token::Ident(_) = self.curr_node.token.clone() {
            let t = self.parse_type().unwrap_or(Type::Unknown);
            self.advance();
            self.consume_next(Token::Comma);
            self.advance();
            args.push(t);
        }
        self.consume(Token::Rparen);

        let ret_t = match self.curr_node.token {
            Token::Arrow => {
                self.advance();
                self.parse_ret_type().unwrap_or(Type::Void)
            }
            _ => Type::Void,
        };

        Some(Type::Fn(args, Box::new(ret_t)))
    }

    fn parse_list_type(&mut self) -> Option<Type> {
        self.advance();
        self.consume(Token::Lbracket);
        let t = self.parse_type().unwrap_or(Type::Unknown);
        self.consume_next(Token::Rbracket);

        Some(Type::List(Box::new(t)))
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
