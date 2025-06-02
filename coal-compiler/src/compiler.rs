use std::rc::Rc;

use coal_core::{ElifExpr, Expr, Ident, Infix, Literal, Parser, Prefix, Span, Stmt};
use coal_objects::{Constant, Object};

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

#[derive(Clone, Debug, Default)]
pub struct Compiler {
    pub parser: Parser,
    pub constants: Vec<Rc<Constant>>,
    pub symbol_table: SymbolTable,
    scopes: Vec<CompilationScope>,
    scope_idx: usize,
}

impl Compiler {
    pub fn new() -> Self {
        Compiler {
            parser: Parser::default(),
            constants: vec![],
            symbol_table: SymbolTable::default(),
            scopes: vec![CompilationScope::default()],
            scope_idx: 0,
        }
    }

    pub fn new_with(constants: Vec<Rc<Constant>>, symbol_table: SymbolTable) -> Self {
        Compiler {
            parser: Parser::default(),
            constants,
            symbol_table,
            scopes: vec![CompilationScope::default()],
            scope_idx: 0,
        }
    }

    pub fn compile(&mut self, input: &str) -> Result<Bytecode, Vec<CompileError>> {
        self.parser.extend(input);
        let stmts = self.parser.parse();

        let mut errors = vec![];
        for stmt in stmts {
            if let Err(e) = self.compile_stmt(&stmt) {
                errors.push(e);
            }
        }

        if !errors.is_empty() {
            return Err(errors);
        }

        Ok(self.bytecode())
    }

    pub fn bytecode(&mut self) -> Bytecode {
        Bytecode {
            instructions: self.instructions_mut().clone(),
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

    fn compile_stmts(&mut self, stmts: &[Stmt]) -> Result<(), CompileError> {
        for stmt in stmts {
            self.compile_stmt(stmt)?;
        }

        Ok(())
    }

    fn compile_expr(&mut self, expr: &Expr) -> Result<(), CompileError> {
        match expr {
            Expr::Ident(ident, _, span) => self.compile_ident(ident, span),
            Expr::Literal(l, _) => self.compile_literal(l),
            Expr::Prefix(op, expr, _) => self.compile_prefix(op, expr),
            Expr::Infix(op, lhs, rhs, _) => self.compile_infix(op, lhs, rhs),
            Expr::Index(lhs, idx, _) => self.compile_index(lhs, idx),
            Expr::If {
                cond,
                then,
                elifs,
                alt,
                ..
            } => self.compile_if(cond, then, elifs, alt),
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

    fn compile_index(&mut self, lhs: &Expr, idx: &Expr) -> Result<(), CompileError> {
        self.compile_expr(lhs)?;
        self.compile_expr(idx)?;
        self.emit(Opcode::Index, &[]);

        Ok(())
    }

    fn compile_if(
        &mut self,
        cond: &Expr,
        then: &[Stmt],
        _elifs: &[ElifExpr],
        alt: &Option<Vec<Stmt>>,
    ) -> Result<(), CompileError> {
        self.compile_expr(cond)?;
        let jump_not_truthy = self.emit(Opcode::JumpFalse, &[9999]);

        self.compile_stmts(then)?;
        if self.last_instruction_mut().opcode == Opcode::Pop {
            self.remove_last_pop();
        }

        let jump_pos = self.emit(Opcode::Jump, &[9999]);
        let after_then_pos = self.instructions().len();

        self.change_operand(jump_not_truthy, after_then_pos);

        if let Some(alt) = alt {
            self.compile_stmts(alt)?;
            if self.last_instruction().opcode == Opcode::Pop {
                self.remove_last_pop();
            }
        } else {
            self.emit(Opcode::Nil, &[]);
        }

        let after_alt_pos = self.instructions().len();
        self.change_operand(jump_pos, after_alt_pos);

        Ok(())
    }

    fn scope(&self) -> &CompilationScope {
        &self.scopes[self.scope_idx]
    }

    fn scope_mut(&mut self) -> &mut CompilationScope {
        &mut self.scopes[self.scope_idx]
    }

    fn instructions(&self) -> &Instructions {
        &self.scope().instructions
    }

    fn instructions_mut(&mut self) -> &mut Instructions {
        &mut self.scope_mut().instructions
    }

    fn last_instruction(&self) -> &EmittedInstruction {
        &self.scope().last_instruction
    }

    fn last_instruction_mut(&mut self) -> &mut EmittedInstruction {
        &mut self.scope_mut().last_instruction
    }

    fn prev_instruction(&self) -> &EmittedInstruction {
        &self.scope().prev_instruction
    }

    fn prev_instruction_mut(&mut self) -> &mut EmittedInstruction {
        &mut self.scope_mut().prev_instruction
    }

    fn emit(&mut self, opcode: Opcode, operands: &[usize]) -> usize {
        let ins = make(opcode, operands);
        let pos = self.instructions_mut().len();

        self.instructions_mut().extend_from_slice(&ins);

        *self.prev_instruction_mut() = self.last_instruction_mut().clone();
        *self.last_instruction_mut() = EmittedInstruction { opcode, pos };

        pos
    }

    fn add_constant(&mut self, obj: Object) -> usize {
        self.constants.push(Rc::new(obj.into()));
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

    fn remove_last_pop(&mut self) {
        let last = self.last_instruction().clone();

        self.instructions_mut().truncate(last.pos);
        *self.last_instruction_mut() = self.prev_instruction().clone();
    }

    fn change_operand(&mut self, pos: usize, operand: usize) {
        let op = Opcode::from(self.instructions()[pos]);
        let ins = make(op, &[operand]);

        self.replace_instruction(pos, &ins);
    }

    fn replace_instruction(&mut self, pos: usize, new_ins: &[u8]) {
        self.instructions_mut().0[pos..pos + new_ins.len()].copy_from_slice(new_ins);
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
