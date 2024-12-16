pub mod env;
pub mod object;

#[cfg(test)]
mod tests;

pub use env::*;
pub use object::*;

use std::{cell::RefCell, rc::Rc};

use crate::{Expr, Ident, IfExpr, Infix, Literal, Parser, Prefix, Span, Stmt, Type};

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
            Stmt::Let(Ident(name), t, expr) => {
                let val = self.eval_expr(&expr)?;
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
            Stmt::Expr(expr) => self.eval_expr(&expr),
            Stmt::Return(expr) => {
                let val = self.eval_expr(&expr)?;
                match val {
                    Object::Error { message, span } => Some(Object::Error { message, span }),
                    _ => Some(Object::Return(Box::new(val))),
                }
            }
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

    fn eval_expr(&mut self, expr: &Expr) -> Option<Object> {
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
            } => self.eval_if_expr(cond, then, elifs, alt),
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
        let lhs = self.eval_expr(lhs)?;
        let rhs = self.eval_expr(rhs)?;

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
    ) -> Option<Object> {
        let cond = self.eval_expr(cond)?;
        if let Object::Error { .. } = cond {
            return Some(cond);
        }
        if cond.is_truthy() {
            return self.eval_stmts(then.to_owned());
        }
        for elif in elifs {
            if self.eval_expr(&elif.cond)?.is_truthy() {
                return self.eval_stmts(elif.then.to_owned());
            }
        }
        self.eval_stmts(alt.to_owned()?)
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

            let curr_env = Rc::clone(&self.env);
            self.env = Rc::new(RefCell::new(enclosed_env));

            let mut res = self.eval_stmts(fn_body.to_owned());
            if let Some(Object::Return(val)) = res {
                res = Some(*val);
            }

            self.env = curr_env;

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
