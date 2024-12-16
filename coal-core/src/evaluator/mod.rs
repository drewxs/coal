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
            match self.eval_stmt(stmt, false) {
                Some(Object::Return(val)) => return Some(*val),
                Some(Object::Error { message, span }) => {
                    return Some(Object::Error { message, span })
                }
                obj => res = obj,
            }
        }

        res
    }

    fn eval_stmt(&mut self, stmt: Stmt, tail: bool) -> Option<Object> {
        match stmt {
            Stmt::Let(Ident(name), t, expr) => {
                let val = self.eval_expr(&expr, false)?;
                if let Object::Error { .. } = val {
                    return Some(val);
                }
                let resolved_t = Type::from(&val);
                if t != resolved_t {
                    return Some(Object::Error {
                        message: format!("type mismatch: expected={t}, got={resolved_t}"),
                        span: expr.span(),
                    });
                }
                self.env.borrow_mut().set(name, val);
                None
            }
            Stmt::Expr(expr) => self.eval_expr(&expr, tail),
            Stmt::Return(expr) => {
                let val = self.eval_expr(&expr, tail)?;
                match val {
                    Object::Error { message, span } => Some(Object::Error { message, span }),
                    Object::TailCall { .. } => Some(val),
                    _ => Some(Object::Return(Box::new(val))),
                }
            }
            _ => None,
        }
    }

    fn eval_stmts(&mut self, stmts: Vec<Stmt>, tail: bool) -> Option<Object> {
        let len = stmts.len();
        let mut res = None;

        for (i, stmt) in stmts.into_iter().enumerate() {
            let is_tail_pos = i == len - 1 && tail;
            let obj = self.eval_stmt(stmt, is_tail_pos);
            match obj {
                Some(Object::Return(val)) => return Some(Object::Return(val)),
                Some(Object::Error { message, span }) => {
                    return Some(Object::Error { message, span })
                }
                Some(Object::TailCall { .. }) => {
                    println!("tail call");
                    return obj;
                }
                obj => res = obj,
            }
        }

        res
    }

    fn eval_expr(&mut self, expr: &Expr, tail: bool) -> Option<Object> {
        match expr {
            Expr::Ident(Ident(name), _) => self.env.borrow().get(name).or_else(|| {
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
            } => self.eval_if_expr(cond, then, elifs, alt, tail),
            Expr::Fn {
                name,
                args,
                ret_t,
                body,
                ..
            } => {
                let func = Object::Fn {
                    name: name.to_owned(),
                    args: args.to_owned(),
                    body: body.to_owned(),
                    ret_t: ret_t.to_owned(),
                };
                self.env.borrow_mut().set(name.to_owned(), func.to_owned());
                Some(func)
            }
            Expr::Call { func, args, span } => self.eval_call_expr(func, args, span, tail),
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
        let rhs = self.eval_expr(rhs, false)?;
        let obj = match prefix {
            Prefix::Not => match rhs {
                Object::Nil | FALSE => TRUE,
                _ => FALSE,
            },
            Prefix::Minus => match rhs {
                Object::Int(i) => Object::Int(-i),
                Object::Float(f) => Object::Float(-f),
                TRUE => Object::Int(-1),
                FALSE => Object::Int(0),
                _ => Object::Error {
                    message: format!("bad operand type for unary -: '{}'", Type::from(&rhs)),
                    span: *span,
                },
            },
        };

        Some(obj)
    }

    fn eval_infix_expr(
        &mut self,
        op: &Infix,
        lhs: &Expr,
        rhs: &Expr,
        span: &Span,
    ) -> Option<Object> {
        let lhs = self.eval_expr(lhs, false)?;
        let rhs = self.eval_expr(rhs, false)?;

        match lhs {
            Object::Int(lhs) => match rhs {
                Object::Int(rhs) => Some(self.eval_infix_int_int(op, lhs, rhs)),
                Object::Float(rhs) => Some(self.eval_infix_int_float(op, lhs, rhs)),
                _ => Some(Object::Error {
                    message: format!(
                        "unsupported operation: {} {op} {}",
                        Type::Int,
                        Type::from(&rhs)
                    ),
                    span: *span,
                }),
            },
            Object::Float(lhs) => match rhs {
                Object::Int(rhs) => Some(self.eval_infix_float_int(op, lhs, rhs)),
                Object::Float(rhs) => Some(self.eval_infix_float_float(op, lhs, rhs)),
                _ => Some(Object::Error {
                    message: format!(
                        "unsupported operation: {} {op} {}",
                        Type::Float,
                        Type::from(&rhs)
                    ),
                    span: *span,
                }),
            },
            Object::Str(lhs) => match rhs {
                Object::Str(rhs) => self.eval_infix_str_str(op, &lhs, &rhs, span),
                Object::Int(rhs) => self.eval_infix_str_int(op, &lhs, &rhs, span),
                _ => Some(Object::Error {
                    message: format!(
                        "unsupported operation: {} {op} {}",
                        Type::Str,
                        Type::from(&rhs)
                    ),
                    span: *span,
                }),
            },
            Object::Bool(lhs) => match rhs {
                Object::Bool(rhs) => Some(self.eval_infix_bool_bool(op, &lhs, &rhs, span)),
                _ => Some(FALSE),
            },
            _ => Some(Object::Error {
                message: format!(
                    "unsupported operation: {} {op} {}",
                    Type::from(&lhs),
                    Type::from(&rhs)
                ),
                span: *span,
            }),
        }
    }

    fn eval_infix_int_int(&mut self, op: &Infix, lhs: i64, rhs: i64) -> Object {
        match op {
            Infix::Plus => Object::Int(lhs + rhs),
            Infix::Minus => Object::Int(lhs - rhs),
            Infix::Mul => Object::Int(lhs * rhs),
            Infix::Div => Object::Float(lhs as f64 / rhs as f64),
            Infix::IntDiv => Object::Int(lhs / rhs),
            Infix::Mod => Object::Int(lhs % rhs),
            Infix::EQ => Object::Bool(lhs == rhs),
            Infix::NEQ => Object::Bool(lhs != rhs),
            Infix::LT => Object::Bool(lhs < rhs),
            Infix::LTE => Object::Bool(lhs <= rhs),
            Infix::GT => Object::Bool(lhs > rhs),
            Infix::GTE => Object::Bool(lhs >= rhs),
        }
    }

    fn eval_infix_int_float(&mut self, op: &Infix, lhs: i64, rhs: f64) -> Object {
        match op {
            Infix::Plus => Object::Float((lhs as f64) + rhs),
            Infix::Minus => Object::Float((lhs as f64) - rhs),
            Infix::Mul => Object::Float((lhs as f64) * rhs),
            Infix::Div => Object::Float((lhs as f64) / rhs),
            Infix::IntDiv => Object::Float(((lhs as f64) / rhs).floor()),
            Infix::Mod => Object::Float((lhs as f64) % rhs),
            Infix::EQ => Object::Bool(lhs as f64 == rhs),
            Infix::NEQ => Object::Bool(lhs as f64 != rhs),
            Infix::LT => Object::Bool((lhs as f64) < rhs),
            Infix::LTE => Object::Bool((lhs as f64) <= rhs),
            Infix::GT => Object::Bool((lhs as f64) > rhs),
            Infix::GTE => Object::Bool((lhs as f64) >= rhs),
        }
    }

    fn eval_infix_float_int(&mut self, op: &Infix, lhs: f64, rhs: i64) -> Object {
        match op {
            Infix::Plus => Object::Float(lhs + (rhs as f64)),
            Infix::Minus => Object::Float(lhs - (rhs as f64)),
            Infix::Mul => Object::Float(lhs * (rhs as f64)),
            Infix::Div => Object::Float(lhs / (rhs as f64)),
            Infix::IntDiv => Object::Float((lhs / (rhs as f64)).floor()),
            Infix::Mod => Object::Float(lhs % (rhs as f64)),
            Infix::EQ => Object::Bool(lhs == (rhs as f64)),
            Infix::NEQ => Object::Bool(lhs != (rhs as f64)),
            Infix::LT => Object::Bool(lhs < (rhs as f64)),
            Infix::LTE => Object::Bool(lhs <= (rhs as f64)),
            Infix::GT => Object::Bool(lhs > (rhs as f64)),
            Infix::GTE => Object::Bool(lhs >= (rhs as f64)),
        }
    }

    fn eval_infix_float_float(&mut self, op: &Infix, lhs: f64, rhs: f64) -> Object {
        match op {
            Infix::Plus => Object::Float(lhs + rhs),
            Infix::Minus => Object::Float(lhs - rhs),
            Infix::Mul => Object::Float(lhs * rhs),
            Infix::Div => Object::Float(lhs / rhs),
            Infix::IntDiv => Object::Float((lhs / rhs).floor()),
            Infix::Mod => Object::Float(lhs % rhs),
            Infix::EQ => Object::Bool(lhs == rhs),
            Infix::NEQ => Object::Bool(lhs != rhs),
            Infix::LT => Object::Bool(lhs < rhs),
            Infix::LTE => Object::Bool(lhs <= rhs),
            Infix::GT => Object::Bool(lhs > rhs),
            Infix::GTE => Object::Bool(lhs >= rhs),
        }
    }

    fn eval_infix_str_str(
        &mut self,
        op: &Infix,
        lhs: &str,
        rhs: &str,
        span: &Span,
    ) -> Option<Object> {
        match op {
            Infix::Plus => Some(Object::Str(lhs.to_string() + rhs)),
            Infix::EQ => Some(Object::Bool(lhs == rhs)),
            Infix::NEQ => Some(Object::Bool(lhs != rhs)),
            Infix::LT => Some(Object::Bool(lhs < rhs)),
            Infix::LTE => Some(Object::Bool(lhs <= rhs)),
            Infix::GT => Some(Object::Bool(lhs > rhs)),
            Infix::GTE => Some(Object::Bool(lhs >= rhs)),
            _ => Some(Object::Error {
                message: format!("unsupported operation: {t} {op} {t}", t = Type::Str),
                span: *span,
            }),
        }
    }

    fn eval_infix_str_int(
        &mut self,
        op: &Infix,
        lhs: &str,
        rhs: &i64,
        span: &Span,
    ) -> Option<Object> {
        match op {
            Infix::Mul => Some(Object::Str(lhs.repeat(*rhs as usize))),
            Infix::EQ => Some(FALSE),
            Infix::NEQ => Some(TRUE),
            _ => Some(Object::Error {
                message: format!("unsupported operation: {} {op} {}", Type::Str, Type::Int),
                span: *span,
            }),
        }
    }

    fn eval_infix_bool_bool(&mut self, op: &Infix, lhs: &bool, rhs: &bool, span: &Span) -> Object {
        match op {
            Infix::EQ => Object::Bool(lhs == rhs),
            Infix::NEQ => Object::Bool(lhs != rhs),
            _ => Object::Error {
                message: format!("unsupported operation: {t} {op} {t}", t = Type::Bool),
                span: *span,
            },
        }
    }

    fn eval_if_expr(
        &mut self,
        cond: &Expr,
        then: &Vec<Stmt>,
        elifs: &Vec<IfExpr>,
        alt: &Option<Vec<Stmt>>,
        tail: bool,
    ) -> Option<Object> {
        let cond_obj = self.eval_expr(cond, false)?;
        if let Object::Error { .. } = cond_obj {
            return Some(cond_obj);
        }
        if cond_obj.is_truthy() {
            return self.eval_stmts(then.to_owned(), tail);
        }
        for elif in elifs {
            let elif_cond_obj = self.eval_expr(&elif.cond, false)?;
            if elif_cond_obj.is_truthy() {
                return self.eval_stmts(elif.then.to_owned(), tail);
            }
        }
        self.eval_stmts(alt.to_owned().unwrap_or_default(), tail)
    }

    fn eval_call_expr(
        &mut self,
        func: &Expr,
        args: &[Expr],
        span: &Span,
        tail: bool,
    ) -> Option<Object> {
        let resolved_args: Vec<_> = args
            .iter()
            .filter_map(|arg| self.eval_expr(arg, false))
            .collect();
        let resolved_fn = self.eval_expr(func, false)?;

        match resolved_fn {
            Object::Fn {
                name,
                args: fn_args,
                body: fn_body,
                ret_t,
            } => {
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

                if tail {
                    return Some(Object::TailCall {
                        func: Box::new(Object::Fn {
                            name,
                            args: fn_args,
                            body: fn_body,
                            ret_t,
                        }),
                        args: resolved_args,
                    });
                }

                self.eval_fn_call(fn_args, fn_body, resolved_args, span)
            }
            _ => Some(Object::Error {
                message: format!("expected function, got {resolved_fn}"),
                span: *span,
            }),
        }
    }

    fn eval_fn_call(
        &mut self,
        fn_args: Vec<Var>,
        fn_body: Vec<Stmt>,
        resolved_args: Vec<Object>,
        span: &Span,
    ) -> Option<Object> {
        let curr_env = Rc::clone(&self.env);
        let mut enclosed_env = Env::from(Rc::clone(&self.env));
        for (var, val) in fn_args.iter().zip(resolved_args.iter()) {
            enclosed_env.set(var.name.to_owned(), val.to_owned());
        }
        self.env = Rc::new(RefCell::new(enclosed_env));

        let mut res = self.eval_stmts(fn_body.clone(), true);
        loop {
            match res {
                Some(Object::TailCall { func, args }) => {
                    // Tail call detected, rebind env without using the stack
                    if let Object::Fn {
                        args: fn_args,
                        body: fn_body,
                        ..
                    } = *func
                    {
                        let mut enclosed_env = Env::from(Rc::clone(&curr_env));
                        for (var, val) in fn_args.iter().zip(args.iter()) {
                            enclosed_env.set(var.name.to_owned(), val.to_owned());
                        }
                        self.env = Rc::new(RefCell::new(enclosed_env));
                        res = self.eval_stmts(fn_body.clone(), true);
                    } else {
                        self.env = curr_env;
                        return Some(Object::Error {
                            message: "expected function in tail call".to_string(),
                            span: *span,
                        });
                    }
                }
                Some(Object::Return(val)) => {
                    self.env = curr_env;
                    return Some(*val);
                }
                other => {
                    self.env = curr_env;
                    return other;
                }
            }
        }
    }
}

impl Default for Evaluator {
    fn default() -> Self {
        Self::new(Rc::new(RefCell::new(Env::default())))
    }
}
