#[cfg(test)]
mod tests;

pub mod builtins;
pub mod env;
mod error;
pub mod object;

use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{
    Builtin, Expr, Func, Ident, IfExpr, Infix, List, Literal, Map, Param, Parser, Prefix, Span,
    Stmt, Struct, StructDecl, Type,
};

pub use builtins::Def;
pub use env::Env;
pub use error::{RuntimeError, RuntimeErrorKind};
use object::StructObj;
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
            builtins: builtins::map(),
        }
    }

    pub fn eval(&mut self, input: &str) -> Result<Object, Vec<RuntimeError>> {
        self.parser = self
            .parser
            .new_with(input, Rc::clone(&self.parser.symbol_table));
        let stmts = self.parser.parse();

        if !self.parser.errors.is_empty() {
            return Err(self.parser.errors.iter().map(RuntimeError::from).collect());
        }

        let mut res = None;
        let mut errors = vec![];

        for stmt in &stmts {
            if stmt == &Stmt::Void {
                continue;
            }

            match self.eval_stmt(stmt) {
                Some(Object::Return(val)) => return Ok(*val),
                Some(Object::Error(e)) => errors.push(e),
                obj => res = obj,
            }
        }

        if !errors.is_empty() {
            return Err(errors);
        }

        let main_fn = self.env.borrow().get("main");
        let res = main_fn
            .map(|f| self.eval_call_obj(&f, &[], &Span::default()))
            .unwrap_or(res)
            .unwrap_or(Object::Void);

        Ok(res)
    }

    pub fn eval_scoped(&mut self, input: &str) -> Result<Object, Vec<RuntimeError>> {
        self.parser = self
            .parser
            .new_with(input, Rc::clone(&self.parser.symbol_table));
        let stmts = self.parser.parse();

        if !self.parser.errors.is_empty() {
            return Err(self.parser.errors.iter().map(RuntimeError::from).collect());
        }

        let mut res = None;
        let mut errors = vec![];

        for stmt in &stmts {
            if stmt == &Stmt::Void {
                continue;
            }

            match self.eval_stmt(stmt) {
                Some(Object::Return(val)) => return Ok(*val),
                Some(Object::Error(e)) => errors.push(e),
                obj => res = obj,
            }
        }

        if !errors.is_empty() {
            return Err(errors);
        }

        Ok(res.unwrap_or(Object::Void))
    }

    fn eval_stmt(&mut self, stmt: &Stmt) -> Option<Object> {
        match stmt {
            Stmt::Let(ident, _, expr) => self.eval_let_stmt(ident, expr),
            Stmt::Assign(lhs, rhs) => self.eval_assign_stmt(lhs, rhs, None),
            Stmt::OpAssign(op, lhs, rhs) => self.eval_assign_stmt(lhs, rhs, Some(op)),
            Stmt::Return(expr) => self.eval_return_stmt(expr),
            Stmt::StructDecl(s, _) => self.eval_struct_decl(s),
            Stmt::Expr(expr) => self.eval_expr(expr),
            _ => None,
        }
    }

    fn eval_stmts(&mut self, stmts: &[Stmt]) -> Option<Object> {
        let mut res = None;

        for stmt in stmts {
            if stmt == &Stmt::Void {
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

    fn eval_stmts_in_scope(&mut self, stmts: &[Stmt], env: Rc<RefCell<Env>>) -> Option<Object> {
        let curr_env = Rc::clone(&self.env);
        self.env = env;
        let res = self.eval_stmts(stmts);
        self.env = curr_env;

        res
    }

    fn eval_block(&mut self, stmts: &[Stmt]) -> Option<Object> {
        let curr_env = Rc::clone(&self.env);
        self.env = Rc::new(RefCell::new(Env::from(Rc::clone(&self.env))));
        let res = self.eval_stmts(stmts);
        self.env = curr_env;

        res
    }

    fn eval_let_stmt(&mut self, ident: &Ident, expr: &Expr) -> Option<Object> {
        let val = self.eval_expr(expr)?;
        if let Object::Error { .. } = val {
            return Some(val);
        }

        self.env.borrow_mut().set_in_scope(ident.name(), val);

        None
    }

    fn eval_assign_stmt(&mut self, lhs: &Expr, rhs: &Expr, op: Option<&Infix>) -> Option<Object> {
        // Create list of indices for nested indexing
        let (name, indices) = match lhs {
            Expr::Ident(ident, _, _) => (ident.name(), vec![]),
            Expr::Index(expr, idx, _) => {
                let mut indices = Vec::new();
                indices.push(*idx.clone());

                let mut curr_expr = expr.clone();
                while let Expr::Index(inner_expr, inner_idx, _) = curr_expr.as_ref() {
                    indices.push(*inner_idx.clone());
                    curr_expr = inner_expr.clone();
                }

                if let Expr::Ident(ident, _, _) = curr_expr.as_ref() {
                    (ident.name(), indices.into_iter().rev().collect())
                } else {
                    unreachable!()
                }
            }
            _ => unreachable!(),
        };
        let lhs_t = Type::try_from(lhs).ok()?;

        let curr = match self.env.borrow_mut().get(&name) {
            Some(value) => value,
            None => {
                return Some(Object::Error(RuntimeError::new(
                    RuntimeErrorKind::IdentifierNotFound(name),
                    lhs.span(),
                )))
            }
        };

        let mut val = self.eval_expr(rhs)?;
        if let Object::Error { .. } = val {
            return Some(val);
        }

        let val_t = Type::from(&val);
        if val_t.is_numeric() && val_t != lhs_t {
            if let Some(casted) = val.cast(&lhs_t) {
                val = casted;
            } else {
                return None;
            }
        }

        if let Object::Fn { .. } = curr {
            return Some(Object::Error(RuntimeError::new(
                RuntimeErrorKind::ReassignmentToFunction,
                rhs.span(),
            )));
        }

        match curr {
            Object::List { mut data, t } => {
                let mut target = &mut data;

                // Nested indexing (e.g. list[0][1])
                for (i, index) in indices.iter().enumerate() {
                    if let Ok(idx) = self.eval_expr(index)?.try_into() {
                        if idx >= target.len() {
                            return Some(Object::Error(RuntimeError::new(
                                RuntimeErrorKind::IndexOutOfBounds(idx, target.len()),
                                index.span(),
                            )));
                        }

                        // Final index, assign value
                        if i == indices.len() - 1 {
                            if let Some(op) = op {
                                target[idx] = self.eval_infix_objects(
                                    op,
                                    target[idx].clone(),
                                    val,
                                    &index.span(),
                                )?;
                            } else {
                                target[idx] = val;
                            }

                            self.env
                                .borrow()
                                .set_in_scope(name, Object::List { data, t });

                            return None;
                        }

                        if let Object::List {
                            data: inner_data, ..
                        } = &mut target[idx]
                        {
                            // Drill down into the nested list
                            target = inner_data;
                            continue;
                        }

                        return None;
                    }

                    return None;
                }
            }
            Object::Map { mut data, t } => {
                if let Some(idx) = indices.first() {
                    if let Some(i) = self.eval_expr(idx) {
                        data.insert(i, val);
                        self.env
                            .borrow()
                            .set_in_scope(name, Object::Map { data, t });
                    } else {
                        return None;
                    }
                }
            }
            _ => {
                let updated_val = match op {
                    Some(op) => self.eval_infix_objects(op, curr, val, &rhs.span())?,
                    None => val,
                };

                self.env.borrow().set_in_scope(name, updated_val);
            }
        }

        None
    }

    fn eval_return_stmt(&mut self, expr: &Expr) -> Option<Object> {
        self.eval_expr(expr).map(|val| match val {
            Object::Error { .. } => val,
            _ => Object::Return(Box::new(val)),
        })
    }

    fn eval_struct_decl(&mut self, s: &StructDecl) -> Option<Object> {
        let obj = Object::from(s);
        self.env
            .borrow_mut()
            .set_in_store(s.name.to_owned(), obj.clone());

        Some(obj)
    }

    fn eval_expr(&mut self, expr: &Expr) -> Option<Object> {
        match expr {
            Expr::Ident(Ident(name), _, _) => self
                .builtins
                .get(name.as_str())
                .map(|b| Object::Builtin(b.clone()))
                .or_else(|| {
                    self.env.borrow().get(name).or_else(|| {
                        Some(Object::Error(RuntimeError::new(
                            RuntimeErrorKind::IdentifierNotFound(name.to_owned()),
                            expr.span(),
                        )))
                    })
                }),
            Expr::Literal(literal, _) => self.eval_literal_expr(literal, &expr.span()),
            Expr::Prefix(prefix, rhs, span) => self.eval_prefix_expr(prefix, rhs, span),
            Expr::Infix(op, lhs, rhs, span) => self.eval_infix_expr(op, lhs, rhs, span),
            Expr::Index(lhs, rhs, span) => self.eval_index_expr(lhs, rhs, span),
            Expr::Range(start, end, _) => self.eval_range_expr(start, end),
            Expr::If {
                cond,
                then,
                elifs,
                alt,
                ..
            } => self.eval_if_expr(cond, then, elifs, alt),
            Expr::While { cond, body, .. } => self.eval_while_expr(cond, body),
            Expr::Iter {
                ident, expr, body, ..
            } => self.eval_iter_expr(ident, expr, body),
            Expr::Fn(Func {
                name,
                args,
                ret_t,
                body,
                span,
            }) => self.eval_fn_expr(name, args, ret_t, body, span),
            Expr::Closure {
                args, ret_t, body, ..
            } => self.eval_closure_expr(args, ret_t, body),
            Expr::Struct(s, span) => self.eval_struct_expr(s, span),
            Expr::Call {
                name,
                args,
                ret_t,
                span,
            } => {
                let expr = Expr::Ident(Ident::from(name.as_str()), ret_t.clone(), *span);
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
            Expr::AttrAccess { lhs, name, .. } => self.eval_attr_access(lhs, name),
        }
    }

    fn eval_literal_expr(&mut self, literal: &Literal, span: &Span) -> Option<Object> {
        match literal {
            Literal::Str(s) => self.eval_str(s, span),
            Literal::List(l) => self.eval_list_expr(l),
            Literal::Map(m) => self.eval_map_expr(m),
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
                match self.eval_scoped(&expr) {
                    Ok(Object::Error(e)) => {
                        let ((l1, c1), (l2, c2)) = e.span;
                        let ((_, offset), (_, _)) = *span;
                        return Some(Object::Error(RuntimeError::new(
                            e.kind,
                            ((l1, c1 + offset + 1), (l2, c2 + offset + 1)),
                        )));
                    }
                    Ok(obj) => {
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

    fn eval_list_expr(&mut self, list: &List) -> Option<Object> {
        let mut data = vec![];

        if let Some(repeat) = &list.repeat {
            if let Some(i) = list.data.first() {
                let item = self.eval_expr(i)?;
                let n_obj = self.eval_expr(repeat)?;
                let n: usize = n_obj.try_into().ok()?;
                data = vec![item; n];
            }
        } else {
            for item in &list.data {
                if let Some(val) = self.eval_expr(item) {
                    if let Object::Error { .. } = val {
                        return Some(val);
                    }
                    data.push(val);
                }
            }
        }

        Some(Object::List {
            data,
            t: list.t.clone(),
        })
    }

    fn eval_map_expr(&mut self, m: &Map) -> Option<Object> {
        let mut data = HashMap::new();

        for (k, v) in &m.data {
            let k = self.eval_expr(k)?;
            let v = self.eval_expr(v)?;

            if let Object::Error { .. } = k {
                return Some(k);
            }
            if let Object::Error { .. } = v {
                return Some(v);
            }

            data.insert(k, v);
        }

        Some(Object::Map {
            data,
            t: m.t.clone(),
        })
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

    fn eval_index_expr(&mut self, lhs: &Expr, rhs: &Expr, span: &Span) -> Option<Object> {
        let mut lhs = self.eval_expr(lhs)?;
        let rhs = self.eval_expr(rhs)?;

        match lhs {
            Object::List { .. } => lhs.call("get", &[rhs], span),
            Object::Map { .. } => lhs.call("get", &[rhs], span),
            _ => None,
        }
    }

    fn eval_if_expr(
        &mut self,
        cond: &Expr,
        then: &[Stmt],
        elifs: &[IfExpr],
        alt: &Option<Vec<Stmt>>,
    ) -> Option<Object> {
        let cond = self.eval_expr(cond)?;
        if let Object::Error { .. } = cond {
            return Some(cond);
        }
        if cond.is_truthy() {
            return self.eval_block(then);
        }
        for elif in elifs {
            if self.eval_expr(&elif.cond)?.is_truthy() {
                return self.eval_block(&elif.then);
            }
        }
        if let Some(alt) = alt {
            self.eval_block(alt)
        } else {
            None
        }
    }

    fn eval_while_expr(&mut self, cond: &Expr, body: &[Stmt]) -> Option<Object> {
        let mut resolved_cond = self.eval_expr(cond)?;
        if let Object::Error { .. } = resolved_cond {
            return Some(resolved_cond);
        }

        while resolved_cond.is_truthy() {
            self.eval_block(body);
            resolved_cond = self.eval_expr(cond)?;
        }

        None
    }

    fn eval_range_expr(&mut self, start: &Expr, end: &Expr) -> Option<Object> {
        let start: usize = self.eval_expr(start)?.try_into().ok()?;
        let end: usize = self.eval_expr(end)?.try_into().ok()?;

        Some(Object::Range(start, end))
    }

    fn eval_iter_expr(&mut self, ident: &Ident, expr: &Expr, body: &[Stmt]) -> Option<Object> {
        let curr_env = Rc::clone(&self.env);
        self.env = Rc::new(RefCell::new(Env::from(Rc::clone(&self.env))));

        match self.eval_expr(expr)? {
            Object::Range(start, end) => {
                for i in start..end {
                    self.env
                        .borrow_mut()
                        .set_in_store(ident.name(), Object::U64(i as u64));
                    self.eval_stmts(body);
                }
            }
            Object::List { data, .. } => {
                for item in data {
                    self.env.borrow_mut().set_in_store(ident.name(), item);
                    self.eval_stmts(body);
                }
            }
            _ => {}
        }

        self.env = curr_env;

        None
    }

    fn eval_fn_expr(
        &mut self,
        name: &str,
        args: &[Param],
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
            .borrow()
            .set_in_scope(name.to_owned(), func.to_owned());

        Some(func)
    }

    fn eval_closure_expr(
        &mut self,
        args: &[Param],
        ret_t: &Type,
        body: &Vec<Stmt>,
    ) -> Option<Object> {
        Some(Object::Closure {
            args: args.to_owned(),
            body: body.to_owned(),
            ret_t: ret_t.to_owned(),
        })
    }

    fn eval_struct_expr(&mut self, s: &Struct, span: &Span) -> Option<Object> {
        let decl = self.env.borrow().get(&s.name);
        let mut attrs = vec![];
        let mut funcs = vec![];

        match decl {
            Some(Object::StructDecl(decl)) => {
                for (k, v) in &s.state {
                    let v = self.eval_expr(v)?;
                    if let Object::Error { .. } = v {
                        return Some(v);
                    }
                    attrs.push((k.clone(), v));
                }

                for (param, default_val) in &decl.attrs {
                    if attrs.iter().any(|(k, _)| k == &param.name) {
                        continue;
                    }
                    if let Some(v) = default_val {
                        let v = self.eval_expr(v)?;
                        if let Object::Error { .. } = v {
                            return Some(v);
                        }
                        attrs.push((param.name.clone(), v));
                    }
                }

                for f in &decl.funcs {
                    funcs.push((f.name.clone(), Object::from(f)));
                }

                let obj = Object::Struct(StructObj {
                    name: s.name.clone(),
                    attrs,
                    funcs,
                });
                self.env
                    .borrow_mut()
                    .set_in_store(s.name.clone(), obj.clone());

                Some(obj)
            }
            _ => Some(Object::Error(RuntimeError::new(
                RuntimeErrorKind::IdentifierNotFound(s.name.to_owned()),
                *span,
            ))),
        }
    }

    fn eval_call_expr(&mut self, func: &Expr, args: &[Expr], span: &Span) -> Option<Object> {
        let resolved_args: Vec<Object> =
            args.iter().filter_map(|arg| self.eval_expr(arg)).collect();
        let resolved_fn = self.eval_expr(func)?;

        if let Object::Fn { name, .. } = &resolved_fn
            && name == "main"
        {
            return None;
        }

        self.eval_call_obj(&resolved_fn, &resolved_args, span)
    }

    fn eval_call_obj(&mut self, func: &Object, argsc: &[Object], span: &Span) -> Option<Object> {
        if let Object::Fn { args, body, .. } | Object::Closure { args, body, .. } = func {
            let expected_arity = args.len();
            let actual_arity = argsc.len();

            if expected_arity != actual_arity {
                return Some(Object::Error(RuntimeError::new(
                    RuntimeErrorKind::InvalidArgumentsLength(expected_arity, actual_arity),
                    *span,
                )));
            }

            let enclosed_env = Env::from(Rc::clone(&self.env));
            for (param, value) in args.iter().zip(argsc.iter()) {
                if Type::from(value) == param.t {
                    enclosed_env.set_in_store(param.name.clone(), value.to_owned());
                } else if let Some(casted) = value.cast(&param.t) {
                    enclosed_env.set_in_store(param.name.clone(), casted);
                } else {
                    let expected_t = self.vars_str(args);
                    let resolved_t = self.objects_str(argsc);

                    return Some(Object::Error(RuntimeError::new(
                        RuntimeErrorKind::InvalidArguments(expected_t, resolved_t),
                        *span,
                    )));
                }
            }

            let mut res = self.eval_stmts_in_scope(body, Rc::new(RefCell::new(enclosed_env)));
            if let Some(Object::Return(val)) = res {
                res = Some(*val);
            }

            res
        } else {
            Some(Object::Error(RuntimeError::new(
                RuntimeErrorKind::Mismatch(String::from("function"), Type::from(func)),
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

            let enclosed_env = Env::from(Rc::clone(&self.env));
            for (var, value) in fn_args.iter().zip(resolved_args.iter()) {
                let t = Type::from(value);
                if matches!(var.t, Type::Any) || matches!(t, Type::Any) || t == var.t {
                    enclosed_env.set_in_store(var.name.clone(), value.to_owned());
                } else if let Some(casted) = value.cast(&var.t) {
                    enclosed_env.set_in_store(var.name.clone(), casted);
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

    fn vars_str(&self, args: &[Param]) -> String {
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
        let mut var = lhs.to_string();
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

        if let Expr::Ident(_, Type::Struct(name, _), _) = &lhs {
            var = name.to_owned();
        }

        let mut obj = self.env.borrow().get(&var);
        match obj {
            Some(Object::List { ref data, .. }) => match name {
                "map" => {
                    if !args[0].is_fn() {
                        return Some(Object::Error(RuntimeError::new(
                            RuntimeErrorKind::InvalidArguments(
                                String::from("function"),
                                args[0].to_string(),
                            ),
                            *span,
                        )));
                    }

                    Some(Object::List {
                        data: data
                            .iter()
                            .filter_map(|item| self.eval_call_obj(&args[0], &[item.clone()], span))
                            .collect(),
                        t: Type::List(Box::new(Type::Unknown)),
                    })
                }
                _ => {
                    let res = obj.as_mut().unwrap().call(name, &args, span);
                    self.env.borrow().set_in_scope(var, obj.unwrap());
                    res
                }
            },
            Some(Object::Struct(s)) => self.eval_struct_method_call(&s, name, &args, span),
            Some(mut obj) => {
                let res = obj.call(name, &args, span);
                self.env.borrow_mut().set_in_scope(var, obj);
                res
            }
            None => self.eval_expr(lhs)?.call(name, &args, span),
        }
    }

    fn eval_struct_method_call(
        &mut self,
        s: &StructObj,
        name: &str,
        argsc: &[Object],
        span: &Span,
    ) -> Option<Object> {
        if let Some((_, Object::Fn { args, body, .. })) = s.funcs.iter().find(|(n, _)| n == name) {
            let expected_arity = args.len();
            let actual_arity = argsc.len();

            if expected_arity != actual_arity {
                return Some(Object::Error(RuntimeError::new(
                    RuntimeErrorKind::InvalidArgumentsLength(expected_arity, actual_arity),
                    *span,
                )));
            }

            let enclosed_env = Env::from(Rc::clone(&self.env));
            for (param, value) in args.iter().zip(argsc.iter()) {
                if Type::from(value) == param.t {
                    enclosed_env.set_in_store(param.name.clone(), value.to_owned());
                } else if let Some(casted) = value.cast(&param.t) {
                    enclosed_env.set_in_store(param.name.clone(), casted);
                } else {
                    let expected_t = self.vars_str(args);
                    let resolved_t = self.objects_str(argsc);

                    return Some(Object::Error(RuntimeError::new(
                        RuntimeErrorKind::InvalidArguments(expected_t, resolved_t),
                        *span,
                    )));
                }
            }

            let mut res = self.eval_stmts_in_scope(body, Rc::new(RefCell::new(enclosed_env)));
            if let Some(Object::Return(val)) = res {
                res = Some(*val);
            }

            res
        } else {
            Some(Object::Error(RuntimeError::new(
                RuntimeErrorKind::MethodNotFound(name.to_owned()),
                *span,
            )))
        }
    }

    fn eval_attr_access(&mut self, lhs: &Expr, name: &str) -> Option<Object> {
        if let Object::Struct(s) = &self.eval_expr(lhs)? {
            s.get(name).cloned()
        } else {
            None
        }
    }
}

impl Default for Evaluator<'_> {
    fn default() -> Self {
        Self::new(Rc::new(RefCell::new(Env::default())))
    }
}
