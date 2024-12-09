use std::{fmt, ops::Index};

use crate::Parser;

use super::Stmt;

#[derive(Clone, Debug, PartialEq, Default)]
pub struct Program {
    pub statements: Vec<Stmt>,
}

impl Program {
    pub fn new(statements: Vec<Stmt>) -> Self {
        Self { statements }
    }

    pub fn parse(input: &str) -> Self {
        let mut parser = Parser::from(input);
        Self::new(parser.parse())
    }

    pub fn parse_validate(input: &str) -> Self {
        let mut parser = Parser::from(input);
        let stmts = parser.parse();
        parser.validate();
        Self::new(stmts)
    }

    pub fn read_line(input: &str) -> String {
        Program::parse(input)[0].to_string()
    }

    pub fn iter(&self) -> ProgramIter {
        ProgramIter {
            program: self,
            index: 0,
        }
    }
}

impl From<&str> for Program {
    fn from(input: &str) -> Self {
        Self::new(Parser::from(input).parse())
    }
}

impl From<&String> for Program {
    fn from(input: &String) -> Self {
        Self::new(Parser::from(input).parse())
    }
}

impl From<&Vec<&str>> for Program {
    fn from(input: &Vec<&str>) -> Self {
        Self::new(Parser::from(&input.join("\n")).parse())
    }
}

impl Index<usize> for Program {
    type Output = Stmt;

    fn index(&self, index: usize) -> &Self::Output {
        &self.statements[index]
    }
}

impl Iterator for Program {
    type Item = Stmt;

    fn next(&mut self) -> Option<Self::Item> {
        self.statements.pop()
    }
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for stmt in &self.statements {
            write!(f, "{stmt}")?;
        }
        Ok(())
    }
}

pub struct ProgramIter<'a> {
    program: &'a Program,
    index: usize,
}

impl<'a> Iterator for ProgramIter<'a> {
    type Item = &'a Stmt;

    fn next(&mut self) -> Option<Self::Item> {
        if self.index < self.program.statements.len() {
            let stmt = &self.program[self.index];
            self.index += 1;
            Some(stmt)
        } else {
            None
        }
    }
}
