use std::{cell::RefCell, collections::HashMap, rc::Rc};

use coal_core::{
    ast, Expr,  Ident, ElifExpr, Infix, List, Literal, Map, Param, Parser, Prefix, Span, Stmt,
    Struct, StructDecl, Type,
};
use coal_objects::{builtin_defs, Builtin, Closure,  Func, Object, FALSE, TRUE};

use crate::{
    Env,
    error::{RuntimeError, RuntimeErrorKind},
};

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
            builtins: builtin_defs(),
        }
    }

    pub fn eval(&mut self, input: &str) -> Result<Object, Vec<RuntimeError>> {
        self.parser.extend(input);
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
                Ok(Object::Return(val)) => return Ok(*val),
                Ok(obj) => res = Some(obj),
                Err(e) => errors.push(e),
            }
        }

        if !errors.is_empty() {
            return Err(errors);
        }

        let main_fn = self.env.borrow().get("main");
        let res = main_fn
            .map(|f| self.eval_call_obj(&f, &[], &Span::default()))
            .unwrap_or(Object::Void);

        Ok(res)
    }

    pub fn eval_scoped(&mut self, input: &str) -> Result<Object, Vec<RuntimeError>> {
        self.parser.extend(input);
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
                Ok(Object::Return(val)) => return Ok((*val).clone()),
                Ok(obj) => res = Some(obj),
                Err(e) => errors.push(e),
            }
        }

        if !errors.is_empty() {
            return Err(errors);
        }

        Ok(res.unwrap_or(Object::Void))
    }

    fn eval_stmt(&mut self, stmt: &Stmt) -> Result<Object, RuntimeError> {
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

    fn eval_stmts(&mut self, stmts: &[Stmt]) -> Result<Option<Object>, RuntimeError> {
        for (i, stmt) in stmts.iter().enumerate() {
            if stmt == &Stmt::Void {
                continue;
            }

            match self.eval_stmt(stmt) {
                Ok(Object::Return(val)) => return Ok(Some(Object::Return(val))),
                Err(e) => return Err(e),
                obj if i == stmts.len() - 1 => return Ok(obj.ok()),
                _ => {}
            }
        }

        Ok(None)
    }

    fn eval_stmts_in_scope(&mut self, stmts: &[Stmt], env: Env) -> Result<Object, RuntimeError> {
        let curr_env = Rc::clone(&self.env);
        self.env = Rc::new(RefCell::new(env));
        let res = self.eval_stmts(stmts);
        self.env = curr_env;

        res
    }

    fn eval_block(&mut self, stmts: &[Stmt]) -> Result<Object, RuntimeError> {
        let curr_env = Rc::clone(&self.env);
        self.env = Env::new_enclosed(Rc::clone(&self.env));
        let res = self.eval_stmts(stmts);
        self.env = curr_env;

        res
    }

    fn eval_let_stmt(&mut self, ident: &Ident, expr: &Expr) -> Result<(), RuntimeError> {
        let val = self.eval_expr(expr)?;
        self.env.borrow().set(ident.name(), val);
        Ok(())
    }

    fn eval_assign_stmt(
        &mut self,
        lhs: &Expr,
        rhs: &Expr,
        op: Option<&Infix>,
    ) -> Result<(), RuntimeError> {
        // Create list of indices for nested indexing
        let (name, indices, attrs) = match lhs {
            Expr::Ident(ident, _, _) => (ident.name(), vec![], vec![]),
            Expr::Index(expr, idx, _) => {
                let mut indices = vec![];
                indices.push(*idx.clone());

                let mut curr_expr = expr.clone();
                while let Expr::Index(inner_expr, inner_idx, _) = curr_expr.as_ref() {
                    indices.push(*inner_idx.clone());
                    curr_expr = inner_expr.clone();
                }

                if let Expr::Ident(ident, _, _) = curr_expr.as_ref() {
                    (ident.name(), indices.into_iter().rev().collect(), vec![])
                } else {
                    unreachable!()
                }
            }
            Expr::AttrAccess { lhs, name, .. } => {
                let mut attrs = vec![];
                attrs.push(name.clone());

                let mut curr_expr = lhs.clone();
                while let Expr::AttrAccess { lhs, name, .. } = curr_expr.as_ref() {
                    attrs.push(name.clone());
                    curr_expr = lhs.clone();
                }

                if let Expr::Ident(ident, _, _) = curr_expr.as_ref() {
                    (ident.name(), vec![], attrs.into_iter().rev().collect())
                } else {
                    unreachable!()
                }
            }
            _ => unreachable!(),
        };
        let lhs_t = Type::try_from(lhs)?;

        let curr = match self.env.borrow().get(&name) {
            Some(value) => value,
            None => {
                return Err(RuntimeError::new(
                    RuntimeErrorKind::IdentifierNotFound(name),
                    lhs.span(),
                ));
            }
        };

        let mut val = self.eval_expr(rhs)?;

        let val_t = Type::from(&val);
        if val_t.is_numeric() && val_t != lhs_t {
            if let Some(casted) = val.cast(&lhs_t) {
                val = casted;
            } else {
                return None;
            }
        }

        match curr {
            Object::Func(_) => {
                return Err(RuntimeError::new(
                    RuntimeErrorKind::ReassignmentToFunction,
                    rhs.span(),
                ));
            }
            Object::List(mut data) => {
                let mut target = &mut data;

                // Nested indexing (e.g. list[0][1])
                for (i, index) in indices.iter().enumerate() {
                    if let Ok(idx) = self.eval_expr(index)?.try_into() {
                        if idx >= target.len() {
                            return Err(RuntimeError::new(
                                RuntimeErrorKind::IndexOutOfBounds(idx, target.len()),
                                index.span(),
                            ));
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

                            self.env.borrow().set(name, Object::List(data));

                            return None;
                        }

                        if let Object::List(inner_data) = &mut target[idx] {
                            // Drill down into the nested list
                            target = inner_data;
                            continue;
                        }

                        return None;
                    }

                    return None;
                }
            }
            Object::Map(mut data) => {
                if let Some(idx) = indices.first() {
                    if let Ok(i) = self.eval_expr(idx) {
                        data.insert(i, val);
                        self.env.borrow().set(name, Object::Map(data));
                    } else {
                        return None;
                    }
                }
            }
            Object::Struct(mut s) => {
                s.set(&attrs, val);
                self.env.borrow_mut().set(name, Object::Struct(s));
            }
            _ => {
                let updated_val = match op {
                    Some(op) => self.eval_infix_objects(op, curr, val, &rhs.span())?,
                    None => val,
                };

                self.env.borrow().set(name, updated_val);
            }
        }

        None
    }

    fn eval_return_stmt(&mut self, expr: &Expr) -> Result<Object, RuntimeError> {
        self.eval_expr(expr).map(|val| Object::Return(Rc::new(val)))
    }

    fn eval_struct_decl(&mut self, s: &StructDecl) -> Result<Object, RuntimeError> {
        let obj = Object::from(s);
        self.env.borrow().set_local(s.name.to_owned(), obj.clone());

        Ok(obj)
    }

    fn eval_expr(&mut self, expr: &Expr) -> Result<Object, RuntimeError> {
        match expr {
            Expr::Ident(Ident(name), _, _) => self
                .builtins
                .get(name.as_str())
                .map(|b| Object::Builtin(b.clone()))
                .or_else(|| self.env.borrow().get(name).map(|obj| obj.clone()))
                .ok_or_else(|| {
                    Err(RuntimeError::new(
                        RuntimeErrorKind::IdentifierNotFound(name.to_owned()),
                        expr.span(),
                    ))
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
            Expr::Fn(ast::Func {
                name,
                args,
                ret_t,
                body,
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

    fn eval_literal_expr(
        &mut self,
        literal: &Literal,
        span: &Span,
    ) -> Result<Object, RuntimeError> {
        match literal {
            Literal::Str(s) => self.eval_str(s, span),
            Literal::List(l) => self.eval_list_expr(l),
            Literal::Map(m) => self.eval_map_expr(m),
            _ => Ok(Object::from(literal)),
        }
    }

    fn eval_str(&mut self, s: &str, span: &Span) -> Result<Option<Object>, RuntimeError> {
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
                    Ok(obj) => {
                        res.push_str(&obj.to_string_raw());
                    }
                    Err(e) => {
                        let ((l1, c1), (l2, c2)) = e.span;
                        let ((_, offset), (_, _)) = *span;
                        return Err(RuntimeError::new(
                            e.kind,
                            ((l1, c1 + offset + 1), (l2, c2 + offset + 1)),
                        ));
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
            return Ok(None);
        }

        Ok(Some(Object::Str(res)))
    }

    fn eval_list_expr(&mut self, list: &List) -> Result<Object, RuntimeError> {
        let mut data = vec![];

        if let Some(repeat) = &list.repeat {
            if let Some(i) = list.data.first() {
                let item = self.eval_expr(i)?;
                let n_obj = self.eval_expr(repeat)?;
                let n: usize = n_obj.try_into()?;
                data = vec![item; n];
            }
        } else {
            for item in &list.data {
                if let Ok(val) = self.eval_expr(item) {
                    data.push(val);
                }
            }
        }

        Ok(Object::List(data))
    }

    fn eval_map_expr(&mut self, m: &Map) -> Result<Object, RuntimeError> {
        let mut data = HashMap::new();

        for (k, v) in &m.data {
            let k = self.eval_expr(k)?;
            let v = self.eval_expr(v)?;

            if let Object::Error { .. } = k {
                return Ok(k);
            }
            if let Object::Error { .. } = v {
                return Ok(v);
            }

            data.insert(k, v);
        }

        Some(Object::Map(data))
    }

    fn eval_prefix_expr(
        &mut self,
        prefix: &Prefix,
        rhs: &Expr,
        span: &Span,
    ) -> Result<Object, RuntimeError> {
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
                _ => {
                    return Err(RuntimeError::new(
                        RuntimeErrorKind::BadOperandTypeForUnary('-', Type::from(&rhs)),
                        *span,
                    ));
                }
            },
        };

        Ok(obj)
    }

    fn eval_infix_objects(
        &mut self,
        op: &Infix,
        lhs: Object,
        rhs: Object,
        span: &Span,
    ) -> Result<Object, RuntimeError> {
        let (lhs_t, rhs_t) = (Type::from(&lhs), Type::from(&rhs));

        let result = match op {
            Infix::Add => lhs + rhs,
            Infix::Sub => lhs - rhs,
            Infix::Mul => lhs * rhs,
            Infix::Div => lhs / rhs,
            Infix::Rem => lhs % rhs,
            Infix::EQ => Ok(Object::Bool(lhs == rhs)),
            Infix::NEQ => Ok(Object::Bool(lhs != rhs)),
            Infix::LT => Ok(Object::Bool(lhs < rhs)),
            Infix::LTE => Ok(Object::Bool(lhs <= rhs)),
            Infix::GT => Ok(Object::Bool(lhs > rhs)),
            Infix::GTE => Ok(Object::Bool(lhs >= rhs)),
        };

        result.or_else(|| {
            Err(RuntimeError::new(
                RuntimeErrorKind::UnsupportedOperation(op.clone(), lhs_t, rhs_t),
                *span,
            ))
        })
    }

    fn eval_infix_expr(
        &mut self,
        op: &Infix,
        lhs: &Expr,
        rhs: &Expr,
        span: &Span,
    ) -> Result<Object, RuntimeError> {
        let lhs = self.eval_expr(lhs)?;
        let rhs = self.eval_expr(rhs)?;

        self.eval_infix_objects(op, lhs, rhs, span)
    }

    fn eval_index_expr(
        &mut self,
        lhs: &Expr,
        rhs: &Expr,
        span: &Span,
    ) -> Result<Object, RuntimeError> {
        let mut lhs = self.eval_expr(lhs)?;
        let rhs = self.eval_expr(rhs)?;

        match lhs {
            Object::List(_) => lhs.call("get", &[rhs], span),
            Object::Map(_) => lhs.call("get", &[rhs], span),
            _ => unreachable!(),
        }
    }

    fn eval_if_expr(
        &mut self,
        cond: &Expr,
        then: &[Stmt],
        elifs: &[ElifExpr],
        alt: &Option<Vec<Stmt>>,
    ) -> Result<(), RuntimeError> {
        let cond = self.eval_expr(cond)?;
        if let Object::Error { .. } = cond {
            return Ok(cond);
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
            Ok(())
        }
    }

    fn eval_while_expr(&mut self, cond: &Expr, body: &[Stmt]) -> Result<(), RuntimeError> {
        let mut resolved_cond = self.eval_expr(cond)?;

        while resolved_cond.is_truthy() {
            self.eval_block(body);
            resolved_cond = self.eval_expr(cond)?;
        }

        Ok(())
    }

    fn eval_range_expr(&mut self, start: &Expr, end: &Expr) -> Result<Object, RuntimeError> {
        let start: usize = self.eval_expr(start)?.try_into()?;
        let end: usize = self.eval_expr(end)?.try_into()?;

        Ok(Object::Range(start, end))
    }

    fn eval_iter_expr(
        &mut self,
        ident: &Ident,
        expr: &Expr,
        body: &[Stmt],
    ) -> Result<(), RuntimeError> {
        let iterable = self.eval_expr(expr)?;

        let curr_env = Rc::clone(&self.env);
        self.env = Env::new_enclosed(Rc::clone(&curr_env));

        match iterable {
            Object::Range(start, end) => {
                for i in start..end {
                    self.env
                        .borrow()
                        .set_local(ident.name(), Object::U64(i as u64));
                    self.eval_stmts(body);
                }
            }
            Object::List(data) => {
                for item in data {
                    self.env.borrow().set_local(ident.name(), item);
                    self.eval_stmts(body);
                }
            }
            _ => {}
        }

        self.env = curr_env;

        Ok(())
    }

    fn eval_fn_expr(
        &mut self,
        name: &str,
        args: &[Param],
        ret_t: &Type,
        body: &Vec<Stmt>,
        span: &Span,
    ) -> Result<Object, RuntimeError> {
        if self.env.borrow().has(name) {
            return Err(RuntimeError::new(
                RuntimeErrorKind::IdentifierExists(name.to_owned()),
                *span,
            ));
        }

        let func = Object::Func(Func {
            name: name.to_owned(),
            args: args.to_owned(),
            body: body.to_owned(),
            ret_t: ret_t.to_owned(),
            span: *span,
        });
        self.env.borrow().set(name.to_owned(), func.to_owned());

        Ok(func)
    }

    fn eval_closure_expr(
        &mut self,
        args: &[Param],
        ret_t: &Type,
        body: &Vec<Stmt>,
    ) -> Result<Object, RuntimeError> {
        Ok(Object::Closure(Closure {
            args: args.to_owned(),
            body: body.to_owned(),
            func: CompiledFunc
            free: vec![],
        }))
    }

    fn eval_struct_expr(&mut self, s: &Struct, span: &Span) -> Result<Object, RuntimeError> {
        let decl = self.env.borrow().get(&s.name);
        let mut attrs = vec![];
        let mut funcs = vec![];

        match decl {
            Some(Object::StructDecl(decl)) => {
                for (k, v) in &s.state {
                    let v = self.eval_expr(v)?;
                    attrs.push((k.clone(), v));
                }

                for (param, default_val) in &decl.attrs {
                    if attrs.iter().any(|(k, _)| k == &param.name) {
                        continue;
                    }
                    if let Some(v) = default_val {
                        let v = self.eval_expr(v)?;
                        attrs.push((param.name.clone(), v));
                    }
                }

                for f in &decl.funcs {
                    funcs.push((f.name.clone(), Object::from(f)));
                }

                let obj = Object::Struct(Struct {
                    name: s.name.clone(),
                    state: vec![],
                });
                self.env.borrow().set_local(s.name.clone(), obj.clone());

                Ok(obj)
            }
            _ => Err(RuntimeError::new(
                RuntimeErrorKind::IdentifierNotFound(s.name.to_owned()),
                *span,
            )),
        }
    }

    fn eval_call_expr(
        &mut self,
        func: &Expr,
        args: &[Expr],
        span: &Span,
    ) -> Result<Object, RuntimeError> {
        let resolved_args: Vec<Object> =
            args.iter().filter_map(|arg| self.eval_expr(arg)).collect();
        let resolved_fn = self.eval_expr(func)?;

        if let Object::Func(Func { name, .. }) = &resolved_fn
            && name == "main"
        {
            return None;
        }

        self.eval_call_obj(&resolved_fn, &resolved_args, span)
    }

    fn eval_call_obj(
        &mut self,
        func: &Object,
        argsc: &[Object],
        span: &Span,
    ) -> Result<Object, RuntimeError> {
        if let Object::Func(Func { args, body, .. }) | Object::Closure(Closure { args, body, .. }) =
            func
        {
            let expected_arity = args.len();
            let actual_arity = argsc.len();

            if expected_arity != actual_arity {
                return Err(RuntimeError::new(
                    RuntimeErrorKind::InvalidArgumentsLength(expected_arity, actual_arity),
                    *span,
                ));
            }

            let enclosed_env = Env::from(Rc::clone(&self.env));

            for (param, value) in args.iter().zip(argsc.iter()) {
                if Type::from(value) == param.t {
                    enclosed_env.set_local(param.name.clone(), value.to_owned());
                } else if let Some(casted) = value.cast(&param.t) {
                    enclosed_env.set_local(param.name.clone(), casted);
                } else {
                    let expected_t = self.vars_str(args);
                    let resolved_t = self.objects_str(argsc);

                    return Err(RuntimeError::new(
                        RuntimeErrorKind::InvalidArguments(expected_t, resolved_t),
                        *span,
                    ));
                }
            }

            let mut res = self.eval_stmts_in_scope(body, enclosed_env);
            if let Ok(Object::Return(val)) = res {
                res = Ok(*val);
            }

            res
        } else {
            Err(RuntimeError::new(
                RuntimeErrorKind::Mismatch(String::from("function"), Type::from(func)),
                *span,
            ))
        }
    }

    fn eval_builtin(
        &mut self,
        expr: &Expr,
        args: &[Expr],
        span: &Span,
    ) -> Result<Object, RuntimeError> {
        let resolved_args: Vec<_> = args.iter().filter_map(|arg| self.eval_expr(arg)).collect();
        let resolved_fn = self.eval_expr(expr)?;

        if let Object::Builtin(Builtin(b)) = resolved_fn {
            let expected_arity = fn_args.len();
            let actual_arity = resolved_args.len();

            if expected_arity != actual_arity {
                return Err(RuntimeError::new(
                    RuntimeErrorKind::InvalidArgumentsLength(expected_arity, actual_arity),
                    *span,
                ));
            }

            for (var, value) in fn_args.iter().zip(resolved_args.iter()) {
                let t = Type::from(value);
                if var.t != Type::Any
                    && t != Type::Any
                    && (t != var.t || value.cast(&var.t).is_none())
                {
                    let expected_t = self.vars_str(&fn_args);
                    let resolved_t = self.objects_str(&resolved_args);

                    return Err(RuntimeError::new(
                        RuntimeErrorKind::InvalidArguments(expected_t, resolved_t),
                        *span,
                    ));
                }
            }

            let mut res = func(&resolved_args);

            if let Some(Object::Return(val)) = res {
                res = Some(*val);
            }

            res
        } else {
            Err(RuntimeError::new(
                RuntimeErrorKind::M(String::from("function")),
                *span,
            ))
        }
    }

    fn vars_str(&self, args: &[Param]) -> String {
        args.iter()
            .map(|arg| format!("{}", arg.t))
            .collect::<Vec<String>>()
            .join(", ")
    }

    fn eval_method_call(
        &mut self,
        lhs: &Expr,
        name: &str,
        args: &[Expr],
        span: &Span,
    ) -> Result<Object, RuntimeError> {
        let mut var = lhs.to_string();
        let args = args
            .iter()
            .map(|expr| {
                self.eval_expr(expr)
                    .map_err(|_| RuntimeError::new(RuntimeErrorKind::FailedToEvaluate, expr.span()))
            })
            .collect::<Vec<Object>>();

        if let Expr::Ident(_, Type::Struct(name, _), _) = &lhs {
            var = name.to_owned();
        }

        let mut obj = self.env.borrow().get(&var);
        if obj.is_none() {
            obj = self.eval_expr(lhs);
        }

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
                    self.env.borrow().set(var, obj.unwrap());
                    res
                }
            },
            Some(Object::Struct(s)) => self.eval_struct_method_call(&s, name, &args, span),
            Some(mut obj) => {
                let res = obj.call(name, &args, span);
                self.env.borrow().set(var, obj);
                res
            }
            None => self.eval_expr(lhs)?.call(name, &args, span),
        }
    }

    fn eval_struct_method_call(
        &mut self,
        s: &Struct,
        name: &str,
        argsc: &[Object],
        span: &Span,
    ) -> Result<Object, RuntimeError> {
        if let Some((_, Object::Func(Func { args, body, .. }))) =
            s.funcs.iter().find(|(n, _)| n == name)
        {
            let expected_arity = args.len();
            let actual_arity = argsc.len();

            if expected_arity != actual_arity {
                return Err(RuntimeError::new(
                    RuntimeErrorKind::InvalidArgumentsLength(expected_arity, actual_arity),
                    *span,
                ));
            }

            let enclosed_env = Env::from(Rc::clone(&self.env));

            for (param, value) in args.iter().zip(argsc.iter()) {
                if Type::from(value) == param.t {
                    enclosed_env.set_local(param.name.clone(), value.to_owned());
                } else if let Some(casted) = value.cast(&param.t) {
                    enclosed_env.set_local(param.name.clone(), casted);
                } else {
                    let expected_t = self.vars_str(&args);
                    let resolved_t = self.objects_str(argsc);

                    return Err(RuntimeError::new(
                        RuntimeErrorKind::InvalidArguments(expected_t, resolved_t),
                        *span,
                    ));
                }
            }

            let mut res = self.eval_stmts_in_scope(body, enclosed_env);
            if let Ok(Object::Return(val)) = res {
                res = Ok(*val);
            }

            res
        } else {
            Err(RuntimeError::new(
                RuntimeErrorKind::MethodNotFound(name.to_owned()),
                *span,
            ))
        }
    }

    fn eval_attr_access(&mut self, lhs: &Expr, name: &str) -> Result<Object, RuntimeError> {
        if let Object::Struct(s) = &self.eval_expr(lhs)? {
            s.get(name).cloned()
        } else {
            Err(RuntimeError(
                RuntimeErrorKind::IdentifierNotFound(name.to_owned()),
                lhs.span(),
            ))
        }
    }
}

impl Default for Evaluator<'_> {
    fn default() -> Self {
        Self::new(Rc::new(RefCell::new(Env::default())))
    }
}
