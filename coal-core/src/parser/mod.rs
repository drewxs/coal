#[cfg(test)]
mod tests;

mod error;
pub mod precedence;
pub mod symbol_table;

use std::{cell::RefCell, rc::Rc};

use crate::{
    Comment, Expr, Ident, IfExpr, Infix, Lexer, Literal, Prefix, Stmt, Token, TokenKind, Type, Var,
    U32,
};

pub use error::{ParserError, ParserErrorKind};
pub use precedence::Precedence;
pub use symbol_table::SymbolTable;

#[derive(Clone, Debug, Default)]
pub struct Parser {
    pub lexer: Lexer,
    pub curr_node: Token,
    pub next_node: Token,
    pub symbol_table: Rc<RefCell<SymbolTable>>,
    pub errors: Vec<ParserError>,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        let mut parser = Parser {
            lexer,
            curr_node: Token::default(),
            next_node: Token::default(),
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
            curr_node: Token::default(),
            next_node: Token::default(),
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
            Token {
                token: TokenKind::EOF,
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
    pub fn check(&mut self) -> Result<(), Vec<ParserError>> {
        if self.errors.is_empty() {
            return Ok(());
        }

        Err(self.errors.clone())
    }

    /// Prints errors if there are any.
    pub fn print_errors(&mut self) {
        if let Err(errors) = self.check() {
            for e in errors {
                println!("{e}");
            }
        }
    }

    /// Runs `check`, then panics if there are errors.
    pub fn validate(&mut self) {
        if let Err(errors) = self.check() {
            panic!(
                "{}",
                errors
                    .iter()
                    .map(|e| e.to_string())
                    .collect::<Vec<_>>()
                    .join("\n")
            );
        }
    }

    fn advance(&mut self) {
        self.curr_node = self.next_node.clone();
        self.next_node = self
            .lexer
            .next()
            .unwrap_or(Token::new(TokenKind::EOF, self.next_node.span));
    }

    fn advance_line(&mut self) {
        while !matches!(
            self.curr_node.token,
            TokenKind::Semicolon | TokenKind::NewLine | TokenKind::EOF
        ) {
            self.advance();
        }
    }

    fn consume(&mut self, token: TokenKind) {
        if self.curr_node.token == token {
            self.advance();
        }
    }

    fn consume_next(&mut self, token: TokenKind) {
        if self.next_node.token == token {
            self.advance();
        }
    }

    fn error(&mut self, kind: ParserErrorKind) {
        self.errors
            .push(ParserError::new(kind, self.curr_node.span));
    }

    fn expect_next(&mut self, token: TokenKind) -> Option<()> {
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

    fn lookup_expr_type(&mut self, expr: &Expr) -> Option<Type> {
        if let Expr::Ident(Ident(name), _, _) = &expr {
            if let Some(t) = self.symbol_table.borrow().get(name) {
                return Some(t.clone());
            } else if let Some(t) = self.symbol_table.borrow().get(&format!("__{name}__")) {
                return Some(t.clone());
            }
        }
        Type::try_from(expr).ok()
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
        while self.curr_node.token != TokenKind::Rbrace && self.curr_node.token != TokenKind::EOF {
            if let Some(stmt) = self.parse_stmt() {
                block.push(stmt)
            }
            self.advance();
        }

        block
    }

    fn parse_stmt(&mut self) -> Option<Stmt> {
        match &self.curr_node.token {
            TokenKind::Let => self.parse_let_stmt(),
            TokenKind::Ident(_) => self.parse_ident_stmt(),
            TokenKind::Return => self.parse_ret_stmt(),
            TokenKind::Comment(c) => Some(Stmt::Comment(Comment(c.clone()))),
            TokenKind::NewLine => Some(Stmt::Newline),
            _ => self.parse_expr_stmt(),
        }
    }

    fn parse_let_stmt(&mut self) -> Option<Stmt> {
        self.advance();
        if !matches!(self.curr_node.token, TokenKind::Ident(_)) {
            self.error(ParserErrorKind::SyntaxError(self.curr_node.token.clone()));
            self.advance_line();
            return None;
        }

        let ident = Ident::try_from(&self.curr_node.token).ok()?;
        let ident_span = self.curr_node.span;

        let mut declared_t = match self.next_node.token {
            TokenKind::Colon => {
                self.advance();
                match &self.next_node.token {
                    TokenKind::Ident(_) => {
                        self.advance();
                        self.parse_type()
                    }
                    _ => None,
                }
            }
            _ => None,
        };

        self.expect_next(TokenKind::Assign)?;
        self.advance();

        let expr = self.parse_expr(Precedence::Lowest)?;

        if let Expr::Ident(Ident(name), _, _) = &expr {
            if let Some(t) = self.symbol_table.borrow().get(name) {
                declared_t = Some(t.clone());
            } else if let Some(t) = self.symbol_table.borrow().get(&format!("__{name}__")) {
                declared_t = Some(t.clone());
            }
        } else if let Ok(inf_t) = Type::try_from(&expr) {
            if inf_t.is_defined() {
                if let Some(decl_t) = &declared_t {
                    let decl_tx = decl_t.extract();
                    let inf_tx = inf_t.extract();

                    if !decl_tx.is_numeric() || !inf_tx.is_numeric() {
                        if matches!(inf_tx, Type::List(_)) {
                            declared_t = Some(decl_t.clone());
                        } else {
                            declared_t = Some(inf_tx.clone());
                        }
                    }
                } else {
                    declared_t = Some(inf_t);
                }
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

        self.consume_next(TokenKind::Semicolon);

        Some(Stmt::Let(ident, t, expr))
    }

    fn parse_ident_stmt(&mut self) -> Option<Stmt> {
        let lhs = self.parse_expr(Precedence::Lowest)?;
        self.advance();

        let infix = match self.curr_node.token {
            TokenKind::AddAssign
            | TokenKind::SubAssign
            | TokenKind::MulAssign
            | TokenKind::DivAssign
            | TokenKind::RemAssign => Infix::try_from(&self.curr_node.token).ok(),
            TokenKind::Assign => None,
            _ => return Some(Stmt::Expr(lhs)),
        };

        self.parse_assign_stmt(lhs, infix)
    }

    fn parse_assign_stmt(&mut self, lhs: Expr, infix: Option<Infix>) -> Option<Stmt> {
        self.advance();

        let lhs_t = self.lookup_expr_type(&lhs)?;

        let rhs = self.parse_expr(Precedence::Lowest)?;
        let rhs_t = self.lookup_expr_type(&rhs)?;

        if lhs_t != rhs_t && !(lhs_t.is_numeric() && rhs_t.is_numeric()) {
            self.errors.push(ParserError::new(
                ParserErrorKind::TypeMismatch(lhs_t, rhs_t),
                rhs.span(),
            ));
            self.advance_line();
            return None;
        }

        self.consume_next(TokenKind::Semicolon);

        if let Some(op) = infix {
            Some(Stmt::OpAssign(op, lhs, rhs))
        } else {
            Some(Stmt::Assign(lhs, rhs))
        }
    }

    fn parse_ret_stmt(&mut self) -> Option<Stmt> {
        self.advance();
        let expr = self.parse_expr(Precedence::Lowest).map(Stmt::Return);
        self.consume_next(TokenKind::Semicolon);
        expr
    }

    fn parse_expr(&mut self, precedence: Precedence) -> Option<Expr> {
        let Token { token, span } = &self.curr_node;

        let mut lhs = match token {
            TokenKind::Ident(name) => Ident::try_from(&self.curr_node.token)
                .map(|ident| {
                    Expr::Ident(
                        ident,
                        self.symbol_table.borrow().get(name).unwrap_or_default(),
                        *span,
                    )
                })
                .ok(),
            TokenKind::U32(i) => Some(Expr::Literal(Literal::U32(*i), *span)),
            TokenKind::U64(i) => Some(Expr::Literal(Literal::U64(*i), *span)),
            TokenKind::I32(i) => Some(Expr::Literal(Literal::I32(*i), *span)),
            TokenKind::I64(i) => Some(Expr::Literal(Literal::I64(*i), *span)),
            TokenKind::I128(i) => Some(Expr::Literal(Literal::I128(*i), *span)),
            TokenKind::F32(f) => Some(Expr::Literal(Literal::F32(*f), *span)),
            TokenKind::F64(f) => Some(Expr::Literal(Literal::F64(*f), *span)),
            TokenKind::Str(s) => Some(Expr::Literal(Literal::Str(s.clone()), *span)),
            TokenKind::Bool(b) => Some(Expr::Literal(Literal::Bool(*b), *span)),
            TokenKind::Bang | TokenKind::Add | TokenKind::Sub => self.parse_prefix_expr(),
            TokenKind::Lparen => self.parse_grouped_expr(),
            TokenKind::Lbracket => self.parse_list_expr(),
            TokenKind::If => self.parse_if_expr(),
            TokenKind::While => self.parse_while_expr(),
            TokenKind::For => self.parse_iter_expr(),
            TokenKind::Fn => self.parse_fn_expr(),
            TokenKind::Nil => Some(Expr::Literal(Literal::Nil, *span)),
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
                | Some(Expr::Index(_, _, _))
                | Some(Expr::Call { .. })
                | Some(Expr::MethodCall { .. })
        ) {
            return lhs;
        }

        while self.next_node.token != TokenKind::Semicolon
            && precedence < Precedence::from(&self.next_node.token)
        {
            match self.next_node.token {
                TokenKind::Add
                | TokenKind::Sub
                | TokenKind::Mul
                | TokenKind::Div
                | TokenKind::IntDiv
                | TokenKind::Rem
                | TokenKind::EQ
                | TokenKind::NEQ
                | TokenKind::GT
                | TokenKind::GTE
                | TokenKind::LT
                | TokenKind::LTE => {
                    lhs = self.parse_infix_expr(&lhs?);
                }
                TokenKind::Lparen => {
                    if let Some(Expr::Ident(Ident(name), _, _)) = &lhs {
                        lhs = self.parse_call_expr(name.to_owned());
                    }
                }
                TokenKind::Lbracket => {
                    lhs = self.parse_index_expr(&lhs?);
                }
                TokenKind::Dot => {
                    lhs = self.parse_method_call(lhs?);
                }
                TokenKind::Range => {
                    lhs = self.parse_range_expr(&lhs?);
                }
                _ => break,
            }
        }

        lhs
    }

    fn parse_expr_stmt(&mut self) -> Option<Stmt> {
        self.parse_expr(Precedence::Lowest).map(|expr| {
            self.consume_next(TokenKind::Semicolon);
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

        let args = self.parse_expr_list(TokenKind::Rparen)?;
        let (_, end) = self.curr_node.span;
        let ret_t = self
            .symbol_table
            .borrow()
            .get(&format!("__{name}__"))
            .unwrap_or_default();

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
        self.advance();

        let idx = self.parse_expr(Precedence::Lowest)?;
        self.expect_next(TokenKind::Rbracket)?;

        let (_, end) = self.curr_node.span;

        Some(Expr::Index(
            Box::new(lhs.clone()),
            Box::new(idx),
            (start, end),
        ))
    }

    fn parse_expr_list(&mut self, end_tok: TokenKind) -> Option<Vec<Expr>> {
        let mut list = vec![];

        if self.next_node.token == end_tok {
            self.advance();
            return Some(list);
        }

        self.advance();
        list.push(self.parse_expr(Precedence::Lowest)?);

        while self.next_node.token == TokenKind::Comma {
            self.advance();
            self.advance();
            list.push(self.parse_expr(Precedence::Lowest)?);
        }
        self.expect_next(end_tok);

        Some(list)
    }

    fn parse_uniform_expr_list(
        &mut self,
        end_tok: TokenKind,
    ) -> Option<(Vec<Expr>, Type, Option<Box<Expr>>)> {
        let mut list = vec![];

        if self.next_node.token == end_tok {
            self.advance();
            return Some((list, Type::Unknown, None));
        }

        self.advance();

        let expr = self.parse_expr(Precedence::Lowest)?;
        let t = Type::try_from(&expr).unwrap_or_default();
        list.push(expr.clone());

        if self.next_node.token == TokenKind::Semicolon {
            self.advance();
            self.advance();

            if let Some(expr_list) = self.parse_repeat_expr_list(&expr, &t) {
                self.expect_next(end_tok)?;
                return Some(expr_list);
            }
        }

        while self.next_node.token == TokenKind::Comma {
            self.advance();
            self.advance();

            let (start, _) = self.curr_node.span;
            let expr = self.parse_expr(Precedence::Lowest)?;

            match Type::try_from(&expr) {
                Ok(curr_t) => {
                    if curr_t != t {
                        let (_, end) = self.curr_node.span;
                        self.errors.push(ParserError::new(
                            ParserErrorKind::TypeMismatch(t, curr_t),
                            (start, end),
                        ));
                        return None;
                    }

                    list.push(expr);
                }
                Err(e) => {
                    let (_, end) = self.curr_node.span;
                    self.errors.push(ParserError::new(
                        ParserErrorKind::InvalidIndex(e),
                        (start, end),
                    ));
                    return None;
                }
            }
        }

        self.expect_next(end_tok)?;

        Some((list, t, None))
    }

    fn parse_repeat_expr_list(
        &mut self,
        expr: &Expr,
        t: &Type,
    ) -> Option<(Vec<Expr>, Type, Option<Box<Expr>>)> {
        let rhs = self.parse_expr(Precedence::Lowest)?;
        let (list, repeat) = match &rhs {
            Expr::Literal(Literal::U32(i), _) => {
                (vec![expr.clone(); *i as usize], Some(Box::new(rhs.clone())))
            }
            Expr::Literal(Literal::U64(i), _) => {
                (vec![expr.clone(); *i as usize], Some(Box::new(rhs.clone())))
            }
            Expr::Literal(Literal::I32(i), _) => {
                (vec![expr.clone(); *i as usize], Some(Box::new(rhs.clone())))
            }
            Expr::Literal(Literal::I64(i), _) => {
                (vec![expr.clone(); *i as usize], Some(Box::new(rhs.clone())))
            }
            Expr::Call { ret_t, .. } | Expr::MethodCall { ret_t, .. } => {
                if !ret_t.is_numeric() {
                    self.errors.push(ParserError::new(
                        ParserErrorKind::TypeMismatch(U32, t.clone()),
                        self.next_node.span,
                    ));
                    (vec![], None)
                } else {
                    (vec![expr.clone()], Some(Box::new(rhs.clone())))
                }
            }
            _ => {
                self.errors.push(ParserError::new(
                    ParserErrorKind::TypeMismatch(U32, t.clone()),
                    self.next_node.span,
                ));
                (vec![], None)
            }
        };

        Some((list, t.clone(), repeat))
    }

    fn parse_method_call(&mut self, lhs: Expr) -> Option<Expr> {
        let (start, _) = self.curr_node.span;
        self.advance();
        self.advance();

        let method_name = if let TokenKind::Ident(name) = &self.curr_node.token {
            name.clone()
        } else {
            self.error(ParserErrorKind::SyntaxError(self.curr_node.token.clone()));
            return None;
        };

        let mut args = vec![];
        if self.next_node.token == TokenKind::Lparen {
            self.advance();
            args = self.parse_expr_list(TokenKind::Rparen)?;
        }

        let lhs_t = Type::try_from(&lhs).unwrap_or_default();

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
            self.consume_next(TokenKind::Lparen);

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
        self.expect_next(TokenKind::Rparen)?;
        expr
    }

    fn parse_list_expr(&mut self) -> Option<Expr> {
        let (start, _) = self.curr_node.span;
        let (items, t, repeat) = self.parse_uniform_expr_list(TokenKind::Rbracket)?;
        let (_, end) = self.curr_node.span;

        Some(Expr::Literal(Literal::List(items, t, repeat), (start, end)))
    }

    fn parse_if_expr(&mut self) -> Option<Expr> {
        let (start, _) = self.curr_node.span;
        self.advance();

        let cond = self.parse_expr(Precedence::Lowest)?;
        self.expect_next(TokenKind::Lbrace)?;
        self.advance();

        let then = self.parse_block();

        let mut elifs = vec![];
        while self.next_node.token == TokenKind::Elif {
            self.advance();
            self.advance();

            let cond = self.parse_expr(Precedence::Lowest)?;
            self.expect_next(TokenKind::Lbrace)?;
            self.advance();

            elifs.push(IfExpr {
                cond: Box::new(cond),
                then: self.parse_block(),
            });
        }

        let mut alt = None;
        if self.next_node.token == TokenKind::Else {
            self.advance();
            self.expect_next(TokenKind::Lbrace)?;
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
        self.expect_next(TokenKind::Lbrace)?;
        self.advance();

        let body = self.parse_block();
        let (_, end) = self.curr_node.span;

        Some(Expr::While {
            cond: Box::new(cond),
            body,
            span: (start, end),
        })
    }

    fn parse_range_expr(&mut self, lhs: &Expr) -> Option<Expr> {
        let (start, _) = lhs.span();
        self.advance();
        self.advance();

        let rhs = self.parse_expr(Precedence::Lowest)?;
        let (_, end) = self.curr_node.span;

        Some(Expr::Range(
            Box::new(lhs.clone()),
            Box::new(rhs),
            (start, end),
        ))
    }

    fn parse_iter_expr(&mut self) -> Option<Expr> {
        let (start, _) = self.curr_node.span;
        self.advance();

        let ident = Ident::try_from(&self.curr_node.token).ok()?;
        self.advance();
        self.consume(TokenKind::In);

        let expr = self.parse_expr(Precedence::Lowest)?;
        self.expect_next(TokenKind::Lbrace)?;
        self.advance();

        let iter_t = Type::try_from(&expr)
            .map(|t| t.extract().to_owned())
            .unwrap_or_default();

        let mut st = SymbolTable::from(Rc::clone(&self.symbol_table));
        st.set(ident.name(), iter_t);
        let body = self.parse_block_in_scope(Rc::new(RefCell::new(st)));

        let (_, end) = self.curr_node.span;

        Some(Expr::Iter {
            ident,
            expr: Box::new(expr),
            body,
            span: (start, end),
        })
    }

    fn parse_fn_expr(&mut self) -> Option<Expr> {
        let (start, _) = self.curr_node.span;
        self.advance();

        let ident = Ident::try_from(&self.curr_node.token).ok()?;
        self.expect_next(TokenKind::Lparen)?;
        self.advance();

        let args = self.parse_decl_args()?;
        let args_t = args.iter().map(|arg| arg.t.clone()).collect();
        let declared_ret_t = self.parse_ret_type();
        self.advance();
        self.consume(TokenKind::Lbrace);

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

        while let TokenKind::Ident(name) = self.curr_node.token.clone() {
            self.expect_next(TokenKind::Colon)?;
            self.advance();

            let t = self.parse_type()?;

            self.consume_next(TokenKind::Comma);
            self.advance();

            args.push(Var { name, t });
        }

        self.advance();

        Some(args)
    }

    fn parse_ret_type(&mut self) -> Option<Type> {
        match self.curr_node.token {
            TokenKind::Lbrace | TokenKind::EQ => None,
            _ => {
                if !matches!(self.curr_node.token, TokenKind::Ident(_)) {
                    self.advance();
                }
                self.parse_type()
            }
        }
    }

    fn parse_type(&mut self) -> Option<Type> {
        match &self.curr_node.token {
            TokenKind::Ident(s) => match s.as_str() {
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
        while let TokenKind::Ident(_) = self.curr_node.token.clone() {
            let t = self.parse_type().unwrap_or_default();
            self.advance();
            self.consume_next(TokenKind::Comma);
            self.advance();
            args.push(t);
        }
        self.consume(TokenKind::Rparen);

        let ret_t = match self.curr_node.token {
            TokenKind::Arrow => {
                self.advance();
                self.parse_ret_type().unwrap_or_default()
            }
            _ => Type::Void,
        };

        Some(Type::Fn(args, Box::new(ret_t)))
    }

    fn parse_list_type(&mut self) -> Option<Type> {
        self.advance();
        self.consume(TokenKind::Lbracket);

        let t = self.parse_type().unwrap_or_default();
        self.consume_next(TokenKind::Rbracket);

        Some(Type::List(Box::new(t)))
    }
}

impl From<&str> for Parser {
    fn from(input: &str) -> Self {
        Self::new(Lexer::new(input))
    }
}
