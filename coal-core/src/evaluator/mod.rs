pub mod env;
pub mod object;

#[cfg(test)]
mod tests;

pub use env::*;
pub use object::*;

use std::{cell::RefCell, rc::Rc};

use crate::{Expr, Ident, IfExpr, Infix, Literal, Parser, Prefix, Span, Stmt, Type, Var};

#[derive(Clone, Debug)]
pub struct Evaluator {
    pub env: Rc<RefCell<Env>>,
}

impl Evaluator {
    pub fn new(env: Rc<RefCell<Env>>) -> Self {
        Self { env }
    }

    pub fn eval(&mut self, input: &str) -> Option<Object> {
        let mut parser = Parser::from(input);
        let stmts = parser.parse();

        if !parser.errors.is_empty() {
            return Some(Object::from(&parser.errors[0]));
        }

        let mut res = None;

        for stmt in stmts {
            if stmt == Stmt::Void {
                continue;
            }

            match self.eval_stmt(stmt) {
                Some(Object::Return(val)) => return Some(*val),
                Some(Object::Error { message, span }) => {
                    return Some(Object::Error { message, span })
                }
                obj => res = obj,
            }
        }

        res
    }

    fn eval_stmt(&mut self, stmt: Stmt) -> Option<Object> {
        match stmt {
            Stmt::Let(ident, t, expr) => self.eval_let_stmt(&ident, &t, &expr),
            Stmt::Assign(ident, expr) => self.eval_assign_stmt(&ident, &expr),
            Stmt::AddAssign(ident, expr) => self.eval_op_assign_stmt(&ident, &expr, Infix::Add),
            Stmt::SubAssign(ident, expr) => self.eval_op_assign_stmt(&ident, &expr, Infix::Sub),
            Stmt::MulAssign(ident, expr) => self.eval_op_assign_stmt(&ident, &expr, Infix::Mul),
            Stmt::DivAssign(ident, expr) => self.eval_op_assign_stmt(&ident, &expr, Infix::Div),
            Stmt::RemAssign(ident, expr) => self.eval_op_assign_stmt(&ident, &expr, Infix::Rem),
            Stmt::Return(expr) => self.eval_return_stmt(&expr),
            Stmt::Expr(expr) => self.eval_expr(&expr),
            _ => None,
        }
    }

    fn eval_stmts(&mut self, stmts: Vec<Stmt>) -> Option<Object> {
        let mut res = None;

        for stmt in stmts {
            if stmt == Stmt::Void {
                continue;
            }

            match self.eval_stmt(stmt) {
                Some(Object::Return(val)) => return Some(Object::Return(val)),
                Some(Object::Error { message, span }) => {
                    return Some(Object::Error { message, span })
                }
                obj => res = obj,
            }
        }

        res
    }

    fn eval_stmts_in_scope(&mut self, stmts: Vec<Stmt>, env: Rc<RefCell<Env>>) -> Option<Object> {
        let curr_env = Rc::clone(&self.env);
        self.env = env;
        let res = self.eval_stmts(stmts);
        self.env = curr_env;

        res
    }

    fn eval_block(&mut self, stmts: Vec<Stmt>) -> Option<Object> {
        let curr_env = Rc::clone(&self.env);
        self.env = Rc::new(RefCell::new(Env::from(Rc::clone(&self.env))));
        let res = self.eval_stmts(stmts);
        self.env = curr_env;

        res
    }

    fn eval_let_stmt(&mut self, ident: &Ident, t: &Type, expr: &Expr) -> Option<Object> {
        let Ident(name) = ident;

        let mut val = self.eval_expr(expr)?;
        if let Object::Error { .. } = val {
            return Some(val);
        }

        let resolved_t = Type::from(&val);
        if t != &resolved_t {
            if let Some(casted) = val.cast(t) {
                val = casted;
            } else {
                return Some(Object::Error {
                    message: format!("type mismatch: expected={t}, got={resolved_t}"),
                    span: expr.span(),
                });
            }
        }

        self.env.borrow_mut().set(name.to_owned(), val);

        None
    }

    fn eval_assign_stmt(&mut self, ident: &Ident, expr: &Expr) -> Option<Object> {
        let Ident(name) = ident;
        let curr = self.env.borrow_mut().get(name);

        if let Some(curr) = curr {
            let val = self.eval_expr(expr)?;
            if let Object::Error { .. } = val {
                return Some(val);
            }

            if let Object::Fn { .. } = curr {
                return Some(Object::Error {
                    message: String::from("cannot assign to function"),
                    span: expr.span(),
                });
            }

            let curr_t = Type::from(&curr);
            let val_t = Type::from(&val);

            if curr_t != val_t {
                return Some(Object::Error {
                    message: format!("type mismatch: expected={curr_t}, got={val_t}"),
                    span: expr.span(),
                });
            }

            self.env.borrow_mut().set(name.to_owned(), val);

            None
        } else {
            let ((line, _), _) = expr.span();
            Some(Object::Error {
                message: format!("identifier not found: {name}"),
                span: ((line, 1), (line, name.len())),
            })
        }
    }

    fn eval_op_assign_stmt(&mut self, ident: &Ident, expr: &Expr, op: Infix) -> Option<Object> {
        let Ident(name) = ident;
        let curr = self.env.borrow_mut().get(name);
        let span = expr.span();

        if let Some(curr) = curr {
            let val = self.eval_expr(expr)?;
            if let Object::Error { .. } = val {
                return Some(val);
            }

            if let Object::Fn { .. } = curr {
                return Some(Object::Error {
                    message: String::from("cannot assign to function"),
                    span: expr.span(),
                });
            }

            let curr_t = Type::from(&curr);
            let val_t = Type::from(&val);

            if curr_t != val_t {
                return Some(Object::Error {
                    message: format!("type mismatch: expected={curr_t}, got={val_t}"),
                    span: expr.span(),
                });
            }

            let updated_val = match op {
                Infix::Add => self.eval_infix_objects(&op, curr, val, &span),
                Infix::Sub => self.eval_infix_objects(&op, curr, val, &span),
                Infix::Mul => self.eval_infix_objects(&op, curr, val, &span),
                Infix::Div => self.eval_infix_objects(&op, curr, val, &span),
                Infix::Rem => self.eval_infix_objects(&op, curr, val, &span),
                _ => unimplemented!(),
            };

            self.env.borrow_mut().set(name.to_owned(), updated_val?);

            None
        } else {
            let ((line, _), _) = expr.span();
            Some(Object::Error {
                message: format!("identifier not found: {name}"),
                span: ((line, 1), (line, name.len())),
            })
        }
    }

    fn eval_return_stmt(&mut self, expr: &Expr) -> Option<Object> {
        self.eval_expr(expr).map(|val| match val {
            Object::Error { .. } => val,
            _ => Object::Return(Box::new(val)),
        })
    }

    fn eval_expr(&mut self, expr: &Expr) -> Option<Object> {
        match expr {
            Expr::Ident(Ident(name), _) => self.env.borrow_mut().get(name).or_else(|| {
                Some(Object::Error {
                    message: format!("identifier not found: {name}"),
                    span: expr.span(),
                })
            }),
            Expr::Literal(literal, _) => self.eval_literal_expr(literal, &expr.span()),
            Expr::Prefix(prefix, rhs, span) => self.eval_prefix_expr(prefix, rhs, span),
            Expr::Infix(op, lhs, rhs, span) => self.eval_infix_expr(op, lhs, rhs, span),
            Expr::If {
                cond,
                then,
                elifs,
                alt,
                ..
            } => self.eval_if_expr(cond, then, elifs, alt),
            Expr::While { cond, body, .. } => self.eval_while_expr(cond, body),
            Expr::Fn {
                name,
                args,
                ret_t,
                body,
                span,
            } => self.eval_fn_expr(name, args, ret_t, body, span),
            Expr::Call { func, args, span } => self.eval_call_expr(func, args, span),
        }
    }

    fn eval_literal_expr(&mut self, literal: &Literal, span: &Span) -> Option<Object> {
        match literal {
            Literal::Str(s) => self.eval_str(s, span),
            _ => Some(Object::from(literal)),
        }
    }

    fn eval_str(&mut self, s: &str, span: &Span) -> Option<Object> {
        let mut res = String::new();
        let mut expr = String::new();
        let mut in_expr = false;

        let mut chars = s.chars().peekable();

        while let Some(c) = chars.next() {
            if c == '\\' {
                if let Some(next_c) = chars.peek() {
                    res.push(*next_c);
                    chars.next();
                }
                continue;
            }

            if c == '{' && !in_expr {
                in_expr = true;
                expr.clear();
            } else if c == '}' && in_expr {
                match self.eval(&expr) {
                    Some(Object::Error {
                        message,
                        span: err_span,
                    }) => {
                        let ((l1, c1), (l2, c2)) = err_span;
                        let ((_, offset), (_, _)) = *span;
                        return Some(Object::Error {
                            message,
                            span: ((l1, c1 + offset + 1), (l2, c2 + offset + 1)),
                        });
                    }
                    Some(obj) => {
                        res.push_str(&obj.to_string());
                    }
                    _ => {}
                }

                in_expr = false;
            } else if in_expr {
                expr.push(c);
            } else {
                res.push(c);
            }
        }

        if in_expr {
            return None;
        }

        Some(Object::Str(res))
    }

    fn eval_prefix_expr(&mut self, prefix: &Prefix, rhs: &Expr, span: &Span) -> Option<Object> {
        let rhs = self.eval_expr(rhs)?;
        let obj = match prefix {
            Prefix::Not => match rhs {
                Object::Nil | FALSE => TRUE,
                _ => FALSE,
            },
            Prefix::Minus => match rhs {
                Object::I64(i) => Object::I64(-i),
                Object::F64(f) => Object::F64(-f),
                TRUE => Object::I64(-1),
                FALSE => Object::I64(0),
                _ => Object::Error {
                    message: format!("bad operand type for unary -: '{}'", Type::from(&rhs)),
                    span: *span,
                },
            },
        };

        Some(obj)
    }

    fn eval_infix_objects(
        &mut self,
        op: &Infix,
        lhs: Object,
        rhs: Object,
        span: &Span,
    ) -> Option<Object> {
        let (lhs_t, rhs_t) = (Type::from(&lhs), Type::from(&rhs));

        let result = match op {
            Infix::Add => lhs + rhs,
            Infix::Sub => lhs - rhs,
            Infix::Mul => lhs * rhs,
            Infix::Div => lhs / rhs,
            Infix::IntDiv => lhs.int_div(rhs),
            Infix::Rem => lhs % rhs,
            Infix::EQ => Some(Object::Bool(lhs == rhs)),
            Infix::NEQ => Some(Object::Bool(lhs != rhs)),
            Infix::LT => Some(Object::Bool(lhs < rhs)),
            Infix::LTE => Some(Object::Bool(lhs <= rhs)),
            Infix::GT => Some(Object::Bool(lhs > rhs)),
            Infix::GTE => Some(Object::Bool(lhs >= rhs)),
        };

        result.or(Some(Object::Error {
            message: format!("unsupported operation: {lhs_t} {op} {rhs_t}"),
            span: *span,
        }))
    }

    fn eval_infix_expr(
        &mut self,
        op: &Infix,
        lhs: &Expr,
        rhs: &Expr,
        span: &Span,
    ) -> Option<Object> {
        let lhs = self.eval_expr(lhs)?;
        let rhs = self.eval_expr(rhs)?;

        self.eval_infix_objects(op, lhs, rhs, span)
    }

    fn eval_if_expr(
        &mut self,
        cond: &Expr,
        then: &Vec<Stmt>,
        elifs: &Vec<IfExpr>,
        alt: &Option<Vec<Stmt>>,
    ) -> Option<Object> {
        let cond = self.eval_expr(cond)?;
        if let Object::Error { .. } = cond {
            return Some(cond);
        }
        if cond.is_truthy() {
            return self.eval_block(then.to_owned());
        }
        for elif in elifs {
            if self.eval_expr(&elif.cond)?.is_truthy() {
                return self.eval_block(elif.then.to_owned());
            }
        }
        self.eval_block(alt.to_owned()?)
    }

    fn eval_while_expr(&mut self, cond: &Expr, body: &Vec<Stmt>) -> Option<Object> {
        let mut resolved_cond = self.eval_expr(cond)?;
        if let Object::Error { .. } = resolved_cond {
            return Some(resolved_cond);
        }

        while resolved_cond.is_truthy() {
            self.eval_block(body.to_owned());
            resolved_cond = self.eval_expr(cond)?;
        }

        None
    }

    fn eval_fn_expr(
        &mut self,
        name: &str,
        args: &[Var],
        ret_t: &Type,
        body: &Vec<Stmt>,
        span: &Span,
    ) -> Option<Object> {
        if self.env.borrow_mut().get(name).is_some() {
            return Some(Object::Error {
                message: format!("identifier '{name}' already exists"),
                span: *span,
            });
        }

        let func = Object::Fn {
            name: name.to_owned(),
            args: args.to_owned(),
            body: body.to_owned(),
            ret_t: ret_t.to_owned(),
        };
        self.env.borrow_mut().set(name.to_owned(), func.to_owned());

        Some(func)
    }

    fn eval_call_expr(&mut self, func: &Expr, args: &[Expr], span: &Span) -> Option<Object> {
        let resolved_args: Vec<_> = args.iter().filter_map(|arg| self.eval_expr(arg)).collect();
        let resolved_fn = self.eval_expr(func)?;

        if let Object::Fn {
            args: fn_args,
            body: fn_body,
            ..
        } = resolved_fn
        {
            if fn_args.len() != resolved_args.len() {
                return Some(Object::Error {
                    message: format!(
                        "expected {} arguments, got {}",
                        fn_args.len(),
                        resolved_args.len()
                    ),
                    span: *span,
                });
            }

            let mut enclosed_env = Env::from(Rc::clone(&self.env));
            fn_args
                .iter()
                .zip(resolved_args.iter())
                .for_each(|(var, value)| {
                    enclosed_env.set(var.name.to_owned(), value.to_owned());
                });

            let mut res =
                self.eval_stmts_in_scope(fn_body.to_owned(), Rc::new(RefCell::new(enclosed_env)));
            if let Some(Object::Return(val)) = res {
                res = Some(*val);
            }

            res
        } else {
            Some(Object::Error {
                message: format!("expected function, got {resolved_fn}"),
                span: *span,
            })
        }
    }
}

impl Default for Evaluator {
    fn default() -> Self {
        Self::new(Rc::new(RefCell::new(Env::default())))
    }
}
