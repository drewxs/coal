use std::fmt;
use std::ops::Index;

use crate::Parser;

use super::Stmt;

#[derive(Clone, Debug, PartialEq)]
pub struct Program {
    pub statements: Vec<Stmt>,
}

impl Program {
    pub fn new() -> Self {
        Self { statements: vec![] }
    }

    pub fn parse(input: &str) -> Self {
        let mut parser = Parser::from(input);
        let program = parser.parse();
        parser.validate();
        program
    }

    pub fn parse_lines(lines: &Vec<&str>) -> Self {
        let input = lines.join("\n");
        let mut parser = Parser::from(&input);
        let program = parser.parse();
        parser.validate();
        program
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

impl Default for Program {
    fn default() -> Self {
        Self::new()
    }
}

impl From<&str> for Program {
    fn from(input: &str) -> Self {
        Parser::from(input).parse()
    }
}

impl From<&Vec<&str>> for Program {
    fn from(input: &Vec<&str>) -> Self {
        Parser::from(&input.join("\n")).parse()
    }
}

impl Iterator for Program {
    type Item = Stmt;

    fn next(&mut self) -> Option<Self::Item> {
        self.statements.pop()
    }
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for stmt in &self.statements {
            writeln!(f, "{stmt}")?;
        }
        Ok(())
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

impl Index<usize> for Program {
    type Output = Stmt;

    fn index(&self, index: usize) -> &Self::Output {
        &self.statements[index]
    }
}
