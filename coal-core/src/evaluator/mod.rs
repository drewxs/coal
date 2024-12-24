#[cfg(test)]
mod tests;

pub mod builtins;
pub mod env;
mod error;
pub mod object;

use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{Expr, Ident, IfExpr, Infix, Literal, Parser, Prefix, Span, Stmt, Type, Var};

use builtins::builtins;
pub use builtins::Builtin;
pub use env::Env;
pub use error::{RuntimeError, RuntimeErrorKind};
pub use object::{Object, FALSE, TRUE};

#[derive(Clone, Debug)]
pub struct Evaluator<'s> {
    pub env: Rc<RefCell<Env>>,
    pub parser: Parser,
    pub builtins: HashMap<&'s str, Builtin>,
}

impl Evaluator<'_> {
    pub fn new(env: Rc<RefCell<Env>>) -> Self {
        Self {
            env,
            parser: Parser::default(),
            builtins: builtins(),
        }
    }

    pub fn eval(&mut self, input: &str) -> Option<Object> {
        self.parser = self
            .parser
            .new_with(input, Rc::clone(&self.parser.symbol_table));
        let stmts = self.parser.parse();

        if !self.parser.errors.is_empty() {
            return Some(Object::from(&self.parser.errors[0]));
        }

        let mut res = None;

        for stmt in stmts {
            if stmt == Stmt::Void {
                continue;
            }

            match self.eval_stmt(stmt) {
                Some(Object::Return(val)) => return Some(*val),
                Some(Object::Error(e)) => return Some(Object::Error(e)),
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
                Some(Object::Error(e)) => return Some(Object::Error(e)),
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
                return Some(Object::Error(RuntimeError::new(
                    RuntimeErrorKind::TypeMismatch(t.to_owned(), resolved_t),
                    expr.span(),
                )));
            }
        }

        self.env.borrow_mut().set_in_scope(name.to_owned(), val);

        None
    }

    fn eval_assign_stmt(&mut self, ident: &Ident, expr: &Expr) -> Option<Object> {
        let Ident(name) = ident;
        let curr = self.env.borrow_mut().get(name);

        if let Some(curr) = curr {
            let mut val = self.eval_expr(expr)?;
            if let Object::Error { .. } = val {
                return Some(val);
            }

            if let Object::Fn { .. } = curr {
                return Some(Object::Error(RuntimeError::new(
                    RuntimeErrorKind::ReassignmentToFunction,
                    expr.span(),
                )));
            }

            let curr_t = Type::from(&curr);
            let resolved_t = Type::from(&val);

            if curr_t != resolved_t {
                if let Some(casted) = val.cast(&curr_t) {
                    val = casted;
                } else {
                    return Some(Object::Error(RuntimeError::new(
                        RuntimeErrorKind::TypeMismatch(curr_t, resolved_t),
                        expr.span(),
                    )));
                }
            }

            self.env.borrow_mut().set_in_scope(name.to_owned(), val);

            None
        } else {
            let ((line, _), _) = expr.span();
            Some(Object::Error(RuntimeError::new(
                RuntimeErrorKind::IdentifierNotFound(name.to_owned()),
                ((line, 1), (line, name.len())),
            )))
        }
    }

    fn eval_op_assign_stmt(&mut self, ident: &Ident, expr: &Expr, op: Infix) -> Option<Object> {
        let Ident(name) = ident;
        let curr = self.env.borrow_mut().get(name);
        let span = expr.span();

        if let Some(curr) = curr {
            let mut val = self.eval_expr(expr)?;
            if let Object::Error { .. } = val {
                return Some(val);
            }

            if let Object::Fn { .. } = curr {
                return Some(Object::Error(RuntimeError::new(
                    RuntimeErrorKind::ReassignmentToFunction,
                    expr.span(),
                )));
            }

            let curr_t = Type::from(&curr);
            let resolved_t = Type::from(&val);

            if curr_t != resolved_t {
                if let Some(casted) = val.cast(&curr_t) {
                    val = casted;
                } else {
                    return Some(Object::Error(RuntimeError::new(
                        RuntimeErrorKind::TypeMismatch(curr_t, resolved_t),
                        expr.span(),
                    )));
                }
            }

            let updated_val = self.eval_infix_objects(&op, curr, val, &span);

            self.env
                .borrow_mut()
                .set_in_scope(name.to_owned(), updated_val?);

            None
        } else {
            let ((line, _), _) = expr.span();
            Some(Object::Error(RuntimeError::new(
                RuntimeErrorKind::IdentifierNotFound(name.to_owned()),
                ((line, 1), (line, name.len())),
            )))
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
            Expr::Ident(Ident(name), _, _) => self.env.borrow_mut().get(name).or_else(|| {
                Some(Object::Error(RuntimeError::new(
                    RuntimeErrorKind::IdentifierNotFound(name.to_owned()),
                    expr.span(),
                )))
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
            Expr::Call {
                name,
                args,
                ret_t,
                span,
            } => {
                let expr = Expr::Ident(Ident::from(name), ret_t.clone(), *span);
                if self.builtins.contains_key(name.as_str()) {
                    self.eval_builtin(&expr, args, span)
                } else {
                    self.eval_call_expr(&expr, args, span)
                }
            }
            Expr::MethodCall {
                lhs,
                name,
                args,
                span,
                ..
            } => self.eval_method_call(lhs, name, args, span),
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
                    Some(Object::Error(e)) => {
                        let ((l1, c1), (l2, c2)) = e.span;
                        let ((_, offset), (_, _)) = *span;
                        return Some(Object::Error(RuntimeError::new(
                            e.kind,
                            ((l1, c1 + offset + 1), (l2, c2 + offset + 1)),
                        )));
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
                Object::I32(i) => Object::I32(-i),
                Object::I64(i) => Object::I64(-i),
                Object::I128(i) => Object::I128(-i),
                Object::F32(f) => Object::F32(-f),
                Object::F64(f) => Object::F64(-f),
                TRUE => Object::I32(-1),
                FALSE => Object::I32(0),
                _ => Object::Error(RuntimeError::new(
                    RuntimeErrorKind::BadOperandTypeForUnary('-', Type::from(&rhs)),
                    *span,
                )),
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

        result.or(Some(Object::Error(RuntimeError::new(
            RuntimeErrorKind::UnsupportedOperation(op.clone(), lhs_t, rhs_t),
            *span,
        ))))
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
        if self.env.borrow_mut().has(name) {
            return Some(Object::Error(RuntimeError::new(
                RuntimeErrorKind::IdentifierExists(name.to_owned()),
                *span,
            )));
        }

        let func = Object::Fn {
            name: name.to_owned(),
            args: args.to_owned(),
            body: body.to_owned(),
            ret_t: ret_t.to_owned(),
        };
        self.env
            .borrow_mut()
            .set_in_scope(name.to_owned(), func.to_owned());

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
            let expected_arity = fn_args.len();
            let actual_arity = resolved_args.len();

            if expected_arity != actual_arity {
                return Some(Object::Error(RuntimeError::new(
                    RuntimeErrorKind::InvalidArgumentsLength(expected_arity, actual_arity),
                    *span,
                )));
            }

            let mut enclosed_env = Env::from(Rc::clone(&self.env));
            for (var, value) in fn_args.iter().zip(resolved_args.iter()) {
                if Type::from(value) == var.t {
                    enclosed_env.set_in_store(var.name.to_owned(), value.to_owned());
                } else if let Some(casted) = value.cast(&var.t) {
                    enclosed_env.set_in_store(var.name.to_owned(), casted);
                } else {
                    let expected_t = self.vars_str(&fn_args);
                    let resolved_t = self.objects_str(&resolved_args);

                    return Some(Object::Error(RuntimeError::new(
                        RuntimeErrorKind::InvalidArguments(expected_t, resolved_t),
                        *span,
                    )));
                }
            }

            let mut res =
                self.eval_stmts_in_scope(fn_body.to_owned(), Rc::new(RefCell::new(enclosed_env)));
            if let Some(Object::Return(val)) = res {
                res = Some(*val);
            }

            res
        } else {
            Some(Object::Error(RuntimeError::new(
                RuntimeErrorKind::Mismatch(String::from("function"), Type::from(&resolved_fn)),
                *span,
            )))
        }
    }

    fn eval_builtin(&mut self, expr: &Expr, args: &[Expr], span: &Span) -> Option<Object> {
        let resolved_args: Vec<_> = args.iter().filter_map(|arg| self.eval_expr(arg)).collect();
        let resolved_fn = self.eval_expr(expr)?;

        if let Object::Builtin(Builtin {
            func,
            args: fn_args,
            ..
        }) = resolved_fn
        {
            let expected_arity = fn_args.len();
            let actual_arity = resolved_args.len();

            if expected_arity != actual_arity {
                return Some(Object::Error(RuntimeError::new(
                    RuntimeErrorKind::InvalidArgumentsLength(expected_arity, actual_arity),
                    *span,
                )));
            }

            let mut enclosed_env = Env::from(Rc::clone(&self.env));
            for (var, value) in fn_args.iter().zip(resolved_args.iter()) {
                let t = Type::from(value);
                if matches!(var.t, Type::Any) || matches!(t, Type::Any) || t == var.t {
                    enclosed_env.set_in_store(var.name.to_owned(), value.to_owned());
                } else if let Some(casted) = value.cast(&var.t) {
                    enclosed_env.set_in_store(var.name.to_owned(), casted);
                } else {
                    let expected_t = self.vars_str(&fn_args);
                    let resolved_t = self.objects_str(&resolved_args);

                    return Some(Object::Error(RuntimeError::new(
                        RuntimeErrorKind::InvalidArguments(expected_t, resolved_t),
                        *span,
                    )));
                }
            }

            let mut res = func(&resolved_args);

            if let Some(Object::Return(val)) = res {
                res = Some(*val);
            }

            res
        } else {
            Some(Object::Error(RuntimeError::new(
                RuntimeErrorKind::Mismatch(String::from("function"), Type::from(&resolved_fn)),
                *span,
            )))
        }
    }

    fn vars_str(&self, args: &[Var]) -> String {
        args.iter()
            .map(|arg| format!("{}", arg.t))
            .collect::<Vec<String>>()
            .join(", ")
    }

    fn objects_str(&self, args: &[Object]) -> String {
        args.iter()
            .map(|arg| format!("{}", Type::from(arg)))
            .collect::<Vec<String>>()
            .join(", ")
    }

    fn eval_method_call(
        &mut self,
        lhs: &Expr,
        name: &str,
        args: &[Expr],
        span: &Span,
    ) -> Option<Object> {
        let var = lhs.to_string();
        let args = args
            .iter()
            .map(|expr| {
                self.eval_expr(expr)
                    .unwrap_or(Object::Error(RuntimeError::new(
                        RuntimeErrorKind::FailedToEvaluate,
                        expr.span(),
                    )))
            })
            .collect::<Vec<Object>>();

        {
            let env_ref = self.env.borrow_mut();
            let mut store_ref = env_ref.store.borrow_mut();

            if let Some(obj) = store_ref.get_mut(&var) {
                return obj.call(name, &args, span);
            }

            if let Some(outer_rc) = &env_ref.outer {
                let outer_ref = outer_rc.borrow_mut();
                let mut store_ref = outer_ref.store.borrow_mut();

                if let Some(obj) = store_ref.get_mut(&var) {
                    return obj.call(name, &args, span);
                }
            }
        }

        self.eval_expr(lhs)?.call(name, &args, span)
    }
}

impl Default for Evaluator<'_> {
    fn default() -> Self {
        Self::new(Rc::new(RefCell::new(Env::default())))
    }
}
