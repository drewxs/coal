pub mod env;
pub mod object;

#[cfg(test)]
mod tests;

use std::{cell::RefCell, rc::Rc};

pub use env::*;
pub use object::*;

use crate::{Expr, Ident, Program, Stmt};

#[derive(Clone, Debug)]
pub struct Evaluator {
    pub env: Rc<RefCell<Env>>,
}

impl Evaluator {
    pub fn new(env: Rc<RefCell<Env>>) -> Self {
        Self { env }
    }

    pub fn eval(&mut self, input: &str) -> Option<Object> {
        let program = Program::parse(input);
        self.eval_program(program)
    }

    fn eval_program(&mut self, program: Program) -> Option<Object> {
        let mut res = None;

        for stmt in program {
            if let Stmt::Void = stmt {
                continue;
            }

            res = self.eval_stmt(stmt);
        }

        res
    }

    fn eval_stmt(&mut self, stmt: Stmt) -> Option<Object> {
        match stmt {
            Stmt::Let(Ident(name), _, expr) => {
                let val = self.eval_expr(&expr)?;
                self.env.borrow_mut().set(name, &val);
                Some(Object::Int(0))
            }
            Stmt::Expr(expr) => self.eval_expr(&expr),
            _ => None,
        }
    }

    fn eval_expr(&mut self, expr: &Expr) -> Option<Object> {
        match expr {
            Expr::Ident(Ident(name)) => self.env.borrow().get(name),
            Expr::Literal(literal) => Some(Object::from(literal)),
            _ => None,
        }
    }
}

impl Default for Evaluator {
    fn default() -> Self {
        Self::new(Rc::new(RefCell::new(Env::default())))
    }
}
