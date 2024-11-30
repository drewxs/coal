use std::fmt;

#[derive(Clone, Debug, PartialEq)]
pub struct Comment(pub String);

impl fmt::Display for Comment {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "// {}", self.0)
    }
}
