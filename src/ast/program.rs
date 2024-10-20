use crate::ast::Stmt;

#[derive(Clone, Debug, PartialEq)]
pub struct Program {
    pub statements: Vec<Stmt>,
}

impl Program {
    pub fn new() -> Self {
        Self { statements: vec![] }
    }

    pub fn iter(&self) -> ProgramIter {
        ProgramIter {
            program: self,
            index: 0,
        }
    }
}

impl Default for Program {
    fn default() -> Self {
        Self::new()
    }
}

impl Iterator for Program {
    type Item = Stmt;

    fn next(&mut self) -> Option<Self::Item> {
        self.statements.pop()
    }
}

/// An iterator over the statements in a program.
pub struct ProgramIter<'a> {
    program: &'a Program,
    index: usize,
}

impl<'a> Iterator for ProgramIter<'a> {
    type Item = &'a Stmt;

    fn next(&mut self) -> Option<Self::Item> {
        if self.index < self.program.statements.len() {
            let stmt = &self.program.statements[self.index];
            self.index += 1;
            Some(stmt)
        } else {
            None
        }
    }
}
