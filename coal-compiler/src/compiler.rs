use std::rc::Rc;

use coal_core::{Expr, Ident, Infix, Literal, Parser, Prefix, Span, Stmt};
use coal_objects::Object;

use crate::{
    Bytecode, CompileError, CompileErrorKind, Instructions, Opcode, Symbol, SymbolScope,
    SymbolTable, make,
};

#[derive(Clone, Debug, PartialEq, Default)]
struct EmittedInstruction {
    pub opcode: Opcode,
    pub pos: usize,
}

#[derive(Clone, Debug, PartialEq, Default)]
struct CompilationScope {
    pub instructions: Instructions,
    pub last_instruction: EmittedInstruction,
    pub prev_instruction: EmittedInstruction,
}

#[derive(Clone, Debug, Default, PartialEq)]
pub struct Compiler {
    pub constants: Vec<Rc<Object>>,
    pub symbol_table: SymbolTable,
    scopes: Vec<CompilationScope>,
    scope_idx: usize,
}

impl Compiler {
    pub fn new() -> Self {
        Compiler {
            constants: vec![],
            symbol_table: SymbolTable::default(),
            scopes: vec![CompilationScope::default()],
            scope_idx: 0,
        }
    }

    pub fn compile(&mut self, input: &str) -> Result<Bytecode, Vec<CompileError>> {
        let mut parser = Parser::from(input);
        let stmts = parser.parse();

        let mut errors = vec![];

        for stmt in stmts {
            if let Err(e) = self.compile_stmt(&stmt) {
                errors.push(e);
            }
        }

        Ok(self.bytecode())
    }

    pub fn bytecode(&mut self) -> Bytecode {
        Bytecode {
            instructions: self.instructions().clone(),
            constants: self.constants.clone(),
        }
    }

    fn compile_stmt(&mut self, stmt: &Stmt) -> Result<(), CompileError> {
        match stmt {
            Stmt::Let(ident, _, expr) => {
                let symbol = self.symbol_table.set(ident.name());
                self.compile_expr(expr)?;
                if symbol.scope == SymbolScope::Global {
                    self.emit(Opcode::SetGlobal, &[symbol.idx]);
                } else {
                    self.emit(Opcode::SetLocal, &[symbol.idx]);
                }
            }
            Stmt::Assign(lhs, rhs) => {
                let symbol = match lhs {
                    Expr::Ident(ident, _, _) => self.symbol_table.get(&ident.0).unwrap(),
                    // TODO: handle index/attr access
                    _ => unreachable!(),
                };
                self.compile_expr(rhs)?;
                if symbol.scope == SymbolScope::Global {
                    self.emit(Opcode::SetGlobal, &[symbol.idx]);
                } else {
                    self.emit(Opcode::SetLocal, &[symbol.idx]);
                }
            }
            Stmt::OpAssign(op, lhs, rhs) => {
                let symbol = match lhs {
                    Expr::Ident(ident, _, _) => self.symbol_table.get(&ident.0).unwrap(),
                    // TODO: handle index/attr access
                    _ => unreachable!(),
                };
                self.compile_infix(op, lhs, rhs)?;
                if symbol.scope == SymbolScope::Global {
                    self.emit(Opcode::SetGlobal, &[symbol.idx]);
                } else {
                    self.emit(Opcode::SetLocal, &[symbol.idx]);
                }
            }
            Stmt::Return(expr) => {
                self.compile_expr(expr)?;
                self.emit(Opcode::Ret, &[]);
            }
            Stmt::Expr(expr) => {
                self.compile_expr(expr)?;
                self.emit(Opcode::Pop, &[]);
            }
            _ => {}
        }

        Ok(())
    }

    fn compile_expr(&mut self, expr: &Expr) -> Result<(), CompileError> {
        match expr {
            Expr::Ident(ident, _, span) => self.compile_ident(ident, span),
            Expr::Literal(l, _) => self.compile_literal(l),
            Expr::Prefix(op, expr, _) => self.compile_prefix(op, expr),
            Expr::Infix(op, lhs, rhs, _) => self.compile_infix(op, lhs, rhs),
            _ => Ok(()),
        }
    }

    fn compile_ident(&mut self, ident: &Ident, span: &Span) -> Result<(), CompileError> {
        if let Some(s) = self.symbol_table.get(&ident.0) {
            self.load_symbol(&s);
            Ok(())
        } else {
            Err(CompileError::new(
                CompileErrorKind::NotFound(ident.name()),
                *span,
            ))
        }
    }

    fn compile_literal(&mut self, literal: &Literal) -> Result<(), CompileError> {
        match literal {
            Literal::Bool(b) => {
                self.emit(Opcode::from(*b), &[]);
            }
            Literal::Str(s) => {
                let operands = &[self.add_constant(Object::Str(s.clone()))];
                self.emit(Opcode::Const, operands);
            }
            Literal::U32(i) => {
                let operands = &[self.add_constant(Object::U32(*i))];
                self.emit(Opcode::Const, operands);
            }
            Literal::U64(i) => {
                let operands = &[self.add_constant(Object::U64(*i))];
                self.emit(Opcode::Const, operands);
            }
            Literal::I32(i) => {
                let operands = &[self.add_constant(Object::I32(*i))];
                self.emit(Opcode::Const, operands);
            }
            Literal::I64(i) => {
                let operands = &[self.add_constant(Object::I64(*i))];
                self.emit(Opcode::Const, operands);
            }
            Literal::I128(i) => {
                let operands = &[self.add_constant(Object::I128(*i))];
                self.emit(Opcode::Const, operands);
            }
            Literal::F32(f) => {
                let operands = &[self.add_constant(Object::F32(*f))];
                self.emit(Opcode::Const, operands);
            }
            Literal::F64(f) => {
                let operands = &[self.add_constant(Object::F64(*f))];
                self.emit(Opcode::Const, operands);
            }
            Literal::List(l) => {
                for e in &l.data {
                    self.compile_expr(e)?;
                }
                self.emit(Opcode::List, &[l.data.len()]);
            }
            Literal::Map(m) => {
                for (k, v) in &m.data {
                    self.compile_expr(k)?;
                    self.compile_expr(v)?;
                }
                self.emit(Opcode::Hash, &[m.data.len() * 2]);
            }
            Literal::Nil => {
                self.emit(Opcode::Nil, &[]);
            }
        }

        Ok(())
    }

    fn compile_prefix(&mut self, op: &Prefix, expr: &Expr) -> Result<(), CompileError> {
        self.compile_expr(expr)?;
        self.emit(Opcode::from(op), &[]);

        Ok(())
    }

    fn compile_infix(&mut self, op: &Infix, lhs: &Expr, rhs: &Expr) -> Result<(), CompileError> {
        self.compile_expr(lhs)?;
        self.compile_expr(rhs)?;
        self.emit(Opcode::from(op), &[]);

        Ok(())
    }

    fn scope(&mut self) -> &mut CompilationScope {
        &mut self.scopes[self.scope_idx]
    }

    fn instructions(&mut self) -> &mut Instructions {
        &mut self.scope().instructions
    }

    fn last_instruction(&mut self) -> &mut EmittedInstruction {
        &mut self.scope().last_instruction
    }

    fn prev_instruction(&mut self) -> &mut EmittedInstruction {
        &mut self.scope().prev_instruction
    }

    fn emit(&mut self, opcode: Opcode, operands: &[usize]) -> usize {
        let ins = make(opcode, operands);
        let pos = self.instructions().len();

        self.instructions().extend_from_slice(&ins);

        *self.prev_instruction() = self.last_instruction().clone();
        *self.last_instruction() = EmittedInstruction { opcode, pos };

        pos
    }

    fn add_constant(&mut self, obj: Object) -> usize {
        self.constants.push(obj.into());
        self.constants.len() - 1
    }

    fn load_symbol(&mut self, symbol: &Rc<Symbol>) {
        match symbol.scope {
            SymbolScope::Global => self.emit(Opcode::GetGlobal, &[symbol.idx]),
            SymbolScope::Local => self.emit(Opcode::GetLocal, &[symbol.idx]),
            SymbolScope::Builtin => self.emit(Opcode::GetBuiltin, &[symbol.idx]),
            SymbolScope::Free => self.emit(Opcode::GetFree, &[symbol.idx]),
            SymbolScope::Func => self.emit(Opcode::CurrClosure, &[]),
        };
    }

    // fn enter_scope(&mut self) {
    //     self.scopes.push(CompilationScope::default());
    //     self.scope_idx += 1;
    //     self.symbol_table = SymbolTable::new_enclosed(&self.symbol_table);
    // }
    //
    // fn leave_scope(&mut self) -> Instructions {
    //     let instructions = self.instructions().clone();
    //
    //     self.scopes.pop();
    //     self.scope_idx -= 1;
    //
    //     if let Some(outer) = &self.symbol_table.outer {
    //         self.symbol_table = outer.as_ref().clone();
    //     }
    //
    //     instructions
    // }
}
