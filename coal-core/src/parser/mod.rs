#[cfg(test)]
mod tests;

mod error;
mod precedence;
mod symbol_table;
mod warning;

use std::{cell::RefCell, collections::HashSet, rc::Rc};

use crate::{
    Comment, Expr, Ident, IfExpr, Infix, Lexer, List, Literal, Map, Prefix, Stmt, Token, TokenKind,
    Type, Var, U32,
};

pub use error::{ParserError, ParserErrorKind};
pub use precedence::Precedence;
pub use symbol_table::SymbolTable;
pub use warning::{ParserWarning, ParserWarningKind};

#[derive(Clone, Debug, Default)]
pub struct Parser {
    pub lexer: Lexer,
    pub curr_tok: Token,
    pub next_tok: Token,
    pub symbol_table: Rc<RefCell<SymbolTable>>,
    pub errors: Vec<ParserError>,
    pub warnings: Vec<ParserWarning>,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        let mut parser = Parser {
            lexer,
            curr_tok: Token::default(),
            next_tok: Token::default(),
            symbol_table: Rc::new(RefCell::new(SymbolTable::default())),
            errors: vec![],
            warnings: vec![],
        };
        parser.advance();
        parser.advance();
        parser
    }

    pub fn new_with(&self, input: &str, symbol_table: Rc<RefCell<SymbolTable>>) -> Self {
        let mut parser = Parser {
            lexer: Lexer::new(input),
            curr_tok: Token::default(),
            next_tok: Token::default(),
            symbol_table: Rc::new(RefCell::new(SymbolTable::from(symbol_table))),
            errors: vec![],
            warnings: vec![],
        };
        parser.advance();
        parser.advance();
        parser
    }

    pub fn parse(&mut self) -> Vec<Stmt> {
        let mut stmts = vec![];
        while !matches!(
            self.curr_tok,
            Token {
                kind: TokenKind::EOF,
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
        self.curr_tok = self.next_tok.clone();
        self.next_tok = self
            .lexer
            .next()
            .unwrap_or(Token::new(TokenKind::EOF, self.next_tok.span));
    }

    fn advance_line(&mut self) {
        while !matches!(
            self.curr_tok.kind,
            TokenKind::Semicolon | TokenKind::NewLine | TokenKind::EOF
        ) {
            self.advance();
        }
    }

    fn consume(&mut self, token: TokenKind) {
        if self.curr_tok.kind == token {
            self.advance();
        }
    }

    fn consume_next(&mut self, token: TokenKind) {
        if self.next_tok.kind == token {
            self.advance();
        }
    }

    fn expect_next(&mut self, token: TokenKind) -> Option<()> {
        if self.next_tok.kind == token {
            self.advance();
            return Some(());
        }

        self.errors.push(ParserError::new(
            ParserErrorKind::UnexpectedToken(self.next_tok.kind.clone(), token),
            self.next_tok.span,
        ));

        None
    }

    fn error(&mut self, kind: ParserErrorKind) {
        self.errors.push(ParserError::new(kind, self.curr_tok.span));
    }

    fn lookup_expr_type(&mut self, expr: &Expr) -> Option<Type> {
        if let Expr::Ident(Ident(name), _, _) = &expr {
            if let Some(t) = self.symbol_table.borrow().get(name) {
                return Some(t.clone());
            } else if let Some(t) = self.symbol_table.borrow().get_ret_t(name) {
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

    // Returns (blocks, return statements)
    fn parse_fn_block(&mut self, scope: Rc<RefCell<SymbolTable>>) -> (Vec<Stmt>, Vec<Stmt>) {
        let curr_st = Rc::clone(&self.symbol_table);
        self.symbol_table = scope;

        let mut blocks = vec![];
        let mut ret_stmts = vec![];

        while self.curr_tok.kind != TokenKind::Rbrace && self.curr_tok.kind != TokenKind::EOF {
            if let Some(stmt) = self.parse_stmt() {
                ret_stmts.extend(stmt.ret_stmts());
                blocks.push(stmt);
            }
            self.advance();
        }

        self.symbol_table = curr_st;

        (blocks, ret_stmts)
    }

    fn parse_stmts(&mut self) -> Vec<Stmt> {
        let mut block = vec![];
        while self.curr_tok.kind != TokenKind::Rbrace && self.curr_tok.kind != TokenKind::EOF {
            if let Some(stmt) = self.parse_stmt() {
                block.push(stmt)
            }
            self.advance();
        }

        block
    }

    fn parse_stmt(&mut self) -> Option<Stmt> {
        match &self.curr_tok.kind {
            TokenKind::Let => self.parse_let_stmt(),
            TokenKind::Ident(_) => self.parse_ident_stmt(),
            TokenKind::Return => self.parse_ret_stmt(),
            TokenKind::Comment(c) => Some(Stmt::Comment(Comment(c.clone()))),
            TokenKind::NewLine => Some(Stmt::Newline),
            _ => self.parse_expr_stmt(),
        }
    }

    fn parse_let_stmt(&mut self) -> Option<Stmt> {
        self.advance(); // 'let'
        if !matches!(self.curr_tok.kind, TokenKind::Ident(_)) {
            self.error(ParserErrorKind::SyntaxError(self.curr_tok.kind.clone()));
            self.advance_line();
            return None;
        }

        let ident = Ident::try_from(&self.curr_tok.kind).ok()?;
        let ident_span = self.curr_tok.span;

        let mut declared_t = match self.next_tok.kind {
            TokenKind::Colon => {
                self.advance();
                match &self.next_tok.kind {
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

        let mut expr = self.parse_expr(Precedence::Lowest)?;

        match &mut expr {
            Expr::Ident(Ident(name), _, _) => {
                if let Some(t) = self.symbol_table.borrow().get(name) {
                    declared_t = Some(t.clone());
                } else if let Some(t) = self.symbol_table.borrow().get_ret_t(name) {
                    declared_t = Some(t.clone());
                }
            }
            Expr::Literal(l, _) if !l.is_defined() => {
                // Empty composite literals are unknown when they're parsed, and need type annotations
                // Update their types using the declared type, or err
                match &declared_t {
                    Some(Type::List(t)) => {
                        l.set_type(*t.clone(), None);
                    }
                    Some(Type::Map(t)) => {
                        l.set_type(t.0.clone(), Some(t.1.clone()));
                    }
                    _ => self.errors.push(ParserError::new(
                        ParserErrorKind::TypeAnnotationsNeeded,
                        expr.span(),
                    )),
                }
            }
            _ => {
                if let Ok(inf_t) = Type::try_from(&expr) {
                    if inf_t.is_defined() {
                        if let Some(dt) = &declared_t {
                            let decl_tx = dt.extract();
                            let inf_tx = inf_t.extract();

                            if !decl_tx.is_numeric() || !inf_tx.is_numeric() {
                                if inf_tx.is_composite() {
                                    declared_t = Some(dt.clone());
                                } else {
                                    declared_t = Some(inf_tx.clone());
                                }
                            }
                        } else {
                            declared_t = Some(inf_t);
                        }
                    }
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
                .set_ret_t(ident.name(), *ret_t.clone());
        }

        self.consume_next(TokenKind::Semicolon);

        Some(Stmt::Let(ident, t, expr))
    }

    fn parse_ident_stmt(&mut self) -> Option<Stmt> {
        let lhs = self.parse_expr(Precedence::Lowest)?;
        self.advance();

        let infix = match self.curr_tok.kind {
            TokenKind::AddAssign
            | TokenKind::SubAssign
            | TokenKind::MulAssign
            | TokenKind::DivAssign
            | TokenKind::RemAssign => Infix::try_from(&self.curr_tok.kind).ok(),
            TokenKind::Assign => None,
            _ => return Some(Stmt::Expr(lhs)),
        };

        self.parse_assign_stmt(lhs, infix)
    }

    fn parse_assign_stmt(&mut self, lhs: Expr, infix: Option<Infix>) -> Option<Stmt> {
        self.advance(); // '='

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
        self.advance(); // 'return'
        let expr = self.parse_expr(Precedence::Lowest).map(Stmt::Return);
        self.consume_next(TokenKind::Semicolon);
        expr
    }

    fn parse_expr(&mut self, precedence: Precedence) -> Option<Expr> {
        let Token { kind: token, span } = &self.curr_tok;

        let mut lhs = match token {
            TokenKind::Ident(name) => Ident::try_from(&self.curr_tok.kind)
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
            TokenKind::Lbrace => self.parse_map_expr(),
            TokenKind::If => self.parse_if_expr(),
            TokenKind::While => self.parse_while_expr(),
            TokenKind::For => self.parse_iter_expr(),
            TokenKind::Fn => self.parse_fn_expr(),
            TokenKind::Pipe => self.parse_closure_expr(),
            TokenKind::Nil => Some(Expr::Literal(Literal::Nil, *span)),
            _ => {
                self.error(ParserErrorKind::SyntaxError(self.curr_tok.kind.clone()));
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

        while self.next_tok.kind != TokenKind::Semicolon
            && precedence < Precedence::from(&self.next_tok.kind)
        {
            match self.next_tok.kind {
                TokenKind::Add
                | TokenKind::Sub
                | TokenKind::Mul
                | TokenKind::Div
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
                    self.advance();
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
        let (start, _) = self.curr_tok.span;

        let prefix = Prefix::try_from(&self.curr_tok.kind).ok()?;
        self.advance();

        self.parse_expr(Precedence::Prefix).map(|expr| {
            let (_, end) = expr.span();
            Expr::Prefix(prefix, Box::new(expr.clone()), (start, end))
        })
    }

    fn parse_infix_expr(&mut self, lhs: &Expr) -> Option<Expr> {
        self.advance();

        let infix = Infix::try_from(&self.curr_tok.kind).ok()?;
        let prec = Precedence::from(&self.curr_tok.kind);
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
        let (start, _) = self.curr_tok.span;
        self.advance(); // ident

        let args = self.parse_expr_list(TokenKind::Rparen)?;
        let (_, end) = self.curr_tok.span;

        let ret_t = self
            .symbol_table
            .borrow()
            .get_ret_t(&name)
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
        self.advance(); // '['

        let lhs_t = Type::try_from(lhs).ok()?;

        if !lhs.is_indexable() {
            let (_, end) = self.curr_tok.span;
            self.errors.push(ParserError::new(
                ParserErrorKind::NonIndexableType(lhs_t.clone()),
                (start, (end.0, end.1 + 1)),
            ));
        }

        let idx = self.parse_expr(Precedence::Lowest)?;
        let idx_t = Type::try_from(&idx).ok()?;

        match lhs_t {
            Type::List(_) => {
                if !idx_t.is_int() {
                    self.errors.push(ParserError::new(
                        ParserErrorKind::InvalidIndex(lhs_t, idx_t),
                        idx.span(),
                    ));
                }
            }
            Type::Map(_) => {
                if !idx_t.is_hashable() {
                    self.errors.push(ParserError::new(
                        ParserErrorKind::InvalidIndex(lhs_t, idx_t),
                        idx.span(),
                    ));
                } else if lhs_t.extract() != &idx_t {
                    self.errors.push(ParserError::new(
                        ParserErrorKind::InvalidIndex(lhs_t.clone(), idx_t),
                        idx.span(),
                    ));
                }
            }
            _ => {}
        }

        self.expect_next(TokenKind::Rbracket)?;
        let (_, end) = self.curr_tok.span;

        Some(Expr::Index(
            Box::new(lhs.clone()),
            Box::new(idx),
            (start, end),
        ))
    }

    fn parse_expr_list(&mut self, end_tok: TokenKind) -> Option<Vec<Expr>> {
        let mut list = vec![];

        if self.next_tok.kind == end_tok {
            self.advance();
            return Some(list);
        }

        self.advance(); // '(', '[', '|'
        list.push(self.parse_expr(Precedence::Lowest)?);

        while self.next_tok.kind == TokenKind::Comma {
            self.advance();
            self.advance();
            list.push(self.parse_expr(Precedence::Lowest)?);
        }
        self.expect_next(end_tok);

        Some(list)
    }

    fn parse_list(&mut self, end_tok: TokenKind) -> Option<List> {
        let mut data = vec![];

        if self.next_tok.kind == end_tok {
            self.advance();
            return Some(List::new(&data, Type::Unknown));
        }

        self.advance();

        let expr = self.parse_expr(Precedence::Lowest)?;
        let t = Type::try_from(&expr).unwrap_or_default();
        data.push(expr.clone());

        if self.next_tok.kind == TokenKind::Semicolon {
            self.advance();
            self.advance();

            if let Some(expr_list) = self.parse_repeat_expr_list(&expr, &t) {
                self.expect_next(end_tok)?;
                return Some(expr_list);
            }
        }

        while self.next_tok.kind == TokenKind::Comma {
            self.advance();
            if self.next_tok.kind == end_tok {
                break;
            }
            self.advance();

            let (start, _) = self.curr_tok.span;
            let expr = self.parse_expr(Precedence::Lowest)?;

            match Type::try_from(&expr) {
                Ok(curr_t) => {
                    if curr_t != t {
                        let (_, end) = self.curr_tok.span;
                        self.errors.push(ParserError::new(
                            ParserErrorKind::TypeMismatch(t, curr_t),
                            (start, end),
                        ));
                        self.advance_line();
                        return None;
                    }

                    data.push(expr);
                }
                Err(e) => {
                    let (_, end) = self.curr_tok.span;
                    self.errors.push(ParserError::new(
                        ParserErrorKind::TypeMismatch(t, e),
                        (start, end),
                    ));
                    return None;
                }
            }
        }

        self.expect_next(end_tok)?;

        Some(List::new(&data, t))
    }

    fn parse_repeat_expr_list(&mut self, expr: &Expr, t: &Type) -> Option<List> {
        let rhs = self.parse_expr(Precedence::Lowest)?;
        let (data, repeat) = match &rhs {
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
                        self.next_tok.span,
                    ));
                    (vec![], None)
                } else {
                    (vec![expr.clone()], Some(Box::new(rhs.clone())))
                }
            }
            _ => {
                self.errors.push(ParserError::new(
                    ParserErrorKind::TypeMismatch(U32, t.clone()),
                    self.next_tok.span,
                ));
                (vec![], None)
            }
        };

        Some(List {
            data,
            t: t.clone(),
            repeat,
        })
    }

    fn parse_map(&mut self) -> Option<Map> {
        let mut data = vec![];

        if self.next_tok.kind == TokenKind::Rbrace {
            self.advance();
            return Some(Map {
                data,
                t: (Type::Unknown, Type::Unknown),
            });
        }

        self.advance();

        let mut keys = HashSet::new();

        let k = self.parse_expr(Precedence::Lowest)?;
        let kt = Type::try_from(&k).unwrap_or_default();

        self.expect_next(TokenKind::Colon)?;
        self.advance();

        let v = self.parse_expr(Precedence::Lowest)?;
        let vt = Type::try_from(&v).unwrap_or_default();

        keys.insert(k.to_string());
        data.push((k, v));

        while self.next_tok.kind == TokenKind::Comma {
            self.advance();
            if self.next_tok.kind == TokenKind::Rbrace {
                break;
            }
            self.advance();

            let (start, _) = self.curr_tok.span;

            let k = self.parse_expr(Precedence::Lowest)?;
            if !keys.insert(k.to_string()) {
                let (_, end) = self.curr_tok.span;
                self.warnings.push(ParserWarning::new(
                    ParserWarningKind::MapKeyRepeated(k.to_string()),
                    (start, end),
                ));
            }

            if kt != Type::try_from(&k).unwrap_or_default() {
                let (_, end) = self.curr_tok.span;
                self.errors.push(ParserError::new(
                    ParserErrorKind::TypeMismatch(kt, Type::try_from(&k).unwrap_or_default()),
                    (start, end),
                ));
                self.advance_line();
                return None;
            }

            self.expect_next(TokenKind::Colon)?;
            self.advance();

            let v = self.parse_expr(Precedence::Lowest)?;
            if vt != Type::try_from(&v).unwrap_or_default() {
                let (_, end) = self.curr_tok.span;
                self.errors.push(ParserError::new(
                    ParserErrorKind::TypeMismatch(vt, Type::try_from(&v).unwrap_or_default()),
                    (start, end),
                ));
                self.advance_line();
                return None;
            }

            data.push((k, v));
        }

        self.expect_next(TokenKind::Rbrace)?;

        Some(Map { data, t: (kt, vt) })
    }

    fn parse_method_call(&mut self, lhs: Expr) -> Option<Expr> {
        self.advance(); // lhs
        self.advance();

        let (start, _) = lhs.span();

        let method_name = if let TokenKind::Ident(name) = &self.curr_tok.kind {
            name.clone()
        } else {
            self.error(ParserErrorKind::SyntaxError(self.curr_tok.kind.clone()));
            return None;
        };

        let mut args = vec![];
        if self.next_tok.kind == TokenKind::Lparen {
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

            let (_, end) = self.curr_tok.span;

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

            let (_, end) = self.curr_tok.span;

            self.errors.push(ParserError::new(
                ParserErrorKind::MethodNotFound(lhs_t, String::from(&method_name)),
                (start, end),
            ));

            None
        }
    }

    fn parse_grouped_expr(&mut self) -> Option<Expr> {
        self.advance(); // '('
        let expr = self.parse_expr(Precedence::Lowest);
        self.expect_next(TokenKind::Rparen)?;
        expr
    }

    fn parse_list_expr(&mut self) -> Option<Expr> {
        let (start, _) = self.curr_tok.span;
        let list = self.parse_list(TokenKind::Rbracket)?;
        let (_, end) = self.curr_tok.span;

        Some(Expr::Literal(Literal::List(list), (start, end)))
    }

    fn parse_map_expr(&mut self) -> Option<Expr> {
        let (start, _) = self.curr_tok.span;
        let map = self.parse_map()?;
        let (_, end) = self.curr_tok.span;

        Some(Expr::Literal(Literal::Map(map), (start, end)))
    }

    fn parse_if_expr(&mut self) -> Option<Expr> {
        let (start, _) = self.curr_tok.span;
        self.advance(); // 'if'

        let cond = self.parse_expr(Precedence::Lowest)?;
        self.expect_next(TokenKind::Lbrace)?;
        self.advance();

        let then = self.parse_block();

        let mut elifs = vec![];
        while self.next_tok.kind == TokenKind::Elif {
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
        if self.next_tok.kind == TokenKind::Else {
            self.advance();
            self.expect_next(TokenKind::Lbrace)?;
            self.advance();
            alt = Some(self.parse_block());
        }

        let (_, end) = self.curr_tok.span;

        Some(Expr::If {
            cond: Box::new(cond),
            then,
            elifs,
            alt,
            span: (start, end),
        })
    }

    fn parse_while_expr(&mut self) -> Option<Expr> {
        let (start, _) = self.curr_tok.span;
        self.advance(); // 'while'

        let cond = self.parse_expr(Precedence::Lowest)?;
        self.expect_next(TokenKind::Lbrace)?;
        self.advance();

        let body = self.parse_block();
        let (_, end) = self.curr_tok.span;

        Some(Expr::While {
            cond: Box::new(cond),
            body,
            span: (start, end),
        })
    }

    fn parse_range_expr(&mut self, lhs: &Expr) -> Option<Expr> {
        let (start, _) = lhs.span();
        self.advance(); // lhs
        self.advance();

        let rhs = self.parse_expr(Precedence::Lowest)?;
        let (_, end) = self.curr_tok.span;

        Some(Expr::Range(
            Box::new(lhs.clone()),
            Box::new(rhs),
            (start, end),
        ))
    }

    fn parse_iter_expr(&mut self) -> Option<Expr> {
        let (start, _) = self.curr_tok.span;
        self.advance(); // 'for'

        let ident = Ident::try_from(&self.curr_tok.kind).ok()?;
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

        let (_, end) = self.curr_tok.span;

        Some(Expr::Iter {
            ident,
            expr: Box::new(expr),
            body,
            span: (start, end),
        })
    }

    fn parse_fn_expr(&mut self) -> Option<Expr> {
        let (start, _) = self.curr_tok.span;
        self.advance(); // 'fn'

        let ident = Ident::try_from(&self.curr_tok.kind).ok()?;
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
                st.set_ret_t(arg.name.clone(), *t.clone());
            }
        }

        let (body, ret_stmts) = self.parse_fn_block(Rc::new(RefCell::new(st)));
        let (_, end) = self.curr_tok.span;
        self.check_unreachable(&body);

        let ret_t = if let Some(dt) = declared_ret_t {
            if ret_stmts.is_empty() {
                self.errors.push(ParserError::new(
                    ParserErrorKind::TypeMismatch(dt.clone(), Type::Void),
                    (start, end),
                ));
            }

            for (i, stmt) in body.iter().enumerate() {
                if let Err(e) = stmt.ret_t(&dt, i == body.len() - 1) {
                    self.errors.push(e);
                }
            }

            dt
        } else {
            self.infer_ret_t(&body)
        };

        self.symbol_table
            .borrow_mut()
            .set_ret_t(ident.name(), ret_t.clone());
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

    fn parse_closure_expr(&mut self) -> Option<Expr> {
        let (start, _) = self.curr_tok.span;
        self.advance(); // '|'

        let args = self.parse_decl_args()?;
        self.consume(TokenKind::Pipe);
        self.consume(TokenKind::Lbrace);

        let mut st = SymbolTable::from(Rc::clone(&self.symbol_table));
        for arg in args.iter() {
            st.set(arg.name.clone(), arg.t.clone());
            if let Type::Fn(_, t) = &arg.t {
                st.set_ret_t(arg.name.clone(), *t.clone());
            }
        }

        let (body, _) = self.parse_fn_block(Rc::new(RefCell::new(st)));
        let (_, end) = self.curr_tok.span;
        self.check_unreachable(&body);

        let ret_t = self.infer_ret_t(&body);

        Some(Expr::Closure {
            args,
            ret_t,
            body,
            span: (start, end),
        })
    }

    fn parse_decl_args(&mut self) -> Option<Vec<Var>> {
        let mut args = vec![];

        while let TokenKind::Ident(name) = self.curr_tok.kind.clone() {
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
        match self.curr_tok.kind {
            TokenKind::Lbrace | TokenKind::EQ => None,
            _ => {
                if !matches!(self.curr_tok.kind, TokenKind::Ident(_)) {
                    self.advance();
                }
                self.parse_type()
            }
        }
    }

    fn parse_type(&mut self) -> Option<Type> {
        match &self.curr_tok.kind {
            TokenKind::Ident(s) => match s.as_str() {
                "Fn" => self.parse_fn_type(),
                "list" => self.parse_list_type(),
                "map" => self.parse_map_type(),
                _ => Type::try_from(&self.curr_tok.kind).ok(),
            },
            _ => None,
        }
    }

    fn parse_fn_type(&mut self) -> Option<Type> {
        self.advance(); // 'Fn'
        self.consume(TokenKind::Lparen);

        let mut args = vec![];
        while let TokenKind::Ident(_) = self.curr_tok.kind {
            let t = self.parse_type().unwrap_or_default();
            self.advance();
            self.consume_next(TokenKind::Comma);
            self.advance();
            args.push(t);
        }
        self.consume(TokenKind::Rparen);

        let ret_t = match self.curr_tok.kind {
            TokenKind::Arrow => {
                self.advance();
                self.parse_ret_type().unwrap_or_default()
            }
            _ => Type::Void,
        };

        Some(Type::Fn(args, Box::new(ret_t)))
    }

    fn parse_list_type(&mut self) -> Option<Type> {
        self.advance(); // 'list'
        self.consume(TokenKind::Lbracket);

        let t = self.parse_type().unwrap_or_default();
        self.consume_next(TokenKind::Rbracket);

        Some(Type::List(Box::new(t)))
    }

    fn parse_map_type(&mut self) -> Option<Type> {
        self.advance(); // 'map'
        self.consume(TokenKind::Lbracket);

        let kt = self.parse_type().unwrap_or_default();
        self.consume_next(TokenKind::Comma);
        self.advance();

        let vt = self.parse_type().unwrap_or_default();
        self.consume_next(TokenKind::Rbracket);

        Some(Type::Map(Box::new((kt, vt))))
    }

    fn check_unreachable(&mut self, stmts: &[Stmt]) {
        let len = stmts.len();

        for (i, stmt) in stmts.iter().enumerate() {
            if let Stmt::Return(expr) = stmt {
                if i < len - 1 {
                    self.warnings.push(ParserWarning::new(
                        ParserWarningKind::UnreachableStatement,
                        expr.span(),
                    ))
                }
            }
        }
    }

    fn infer_ret_t(&mut self, body: &[Stmt]) -> Type {
        let mut ret_t = Type::Void;

        for stmt in body {
            match stmt {
                Stmt::Return(expr) => {
                    if let Ok(t) = Type::try_from(expr) {
                        ret_t = t;
                    }
                }
                Stmt::Expr(expr) if matches!(expr, Expr::If { .. }) => {
                    if let Ok(t) = Type::try_from(expr) {
                        ret_t = t;
                    }
                }
                _ => {}
            }
        }

        ret_t
    }
}

impl From<&str> for Parser {
    fn from(input: &str) -> Self {
        Self::new(Lexer::new(input))
    }
}
