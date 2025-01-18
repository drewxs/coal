use std::fmt;

use super::{Expr, List, Map};

#[derive(Clone, Debug, PartialEq)]
pub enum Literal {
    U32(u32),
    U64(u64),
    I32(i32),
    I64(i64),
    I128(i128),
    F32(f32),
    F64(f64),
    Str(String),
    Bool(bool),
    List(List),
    Map(Map),
    Nil,
}

impl Literal {
    pub fn fmt_with_indent(&self, f: &mut fmt::Formatter, indent_level: usize) -> fmt::Result {
        let indent = "    ".repeat(indent_level);

        match self {
            Literal::Str(s) => {
                let mut result = String::new();
                let mut inside_braces = false;
                let mut brace_content = String::new();

                for c in s.chars() {
                    match c {
                        '{' if !inside_braces => {
                            inside_braces = true;
                            result.push('{');
                        }
                        '}' if inside_braces => {
                            inside_braces = false;
                            let trimmed = brace_content.trim();
                            let processed = if trimmed.is_empty() {
                                String::new()
                            } else {
                                trimmed.split_whitespace().collect::<Vec<_>>().join(" ")
                            };
                            result.push_str(&processed);
                            result.push('}');
                            brace_content.clear();
                        }
                        _ if inside_braces => {
                            brace_content.push(c);
                        }
                        _ => {
                            result.push(c);
                        }
                    }
                }

                // Unclosed braces, just add what we have
                if inside_braces {
                    result.push_str(&brace_content);
                }

                write!(f, "{}\"{result}\"", indent)
            }
            Literal::U32(i) => write!(f, "{}{i}", indent),
            Literal::U64(i) => write!(f, "{}{i}", indent),
            Literal::I32(i) => write!(f, "{}{i}", indent),
            Literal::I64(i) => write!(f, "{}{i}", indent),
            Literal::I128(i) => write!(f, "{}{i}", indent),
            Literal::F32(x) => write!(f, "{}{x:?}", indent),
            Literal::F64(x) => write!(f, "{}{x:?}", indent),
            Literal::Bool(b) => write!(f, "{}{b}", indent),
            Literal::List(l) => {
                if let (Some(n), Some(e)) = (&l.repeat, l.data.first()) {
                    write!(f, "{}[{e}; {n}]", indent)
                } else {
                    match l.data.len() {
                        0 => write!(f, "{}[]", indent),
                        1 => write!(f, "{}[{}]", indent, l.data.first().unwrap()),
                        _ => {
                            writeln!(f, "{}[", indent)?;
                            for i in &l.data {
                                match i {
                                    Expr::Ident(i, _, _) => {
                                        i.fmt_with_indent(f, indent_level + 1)?;
                                    }
                                    Expr::Literal(i, _) => {
                                        i.fmt_with_indent(f, indent_level + 1)?;
                                    }
                                    _ => {
                                        i.fmt_with_indent(f, indent_level + 1)?;
                                    }
                                }
                                writeln!(f, ",")?;
                            }
                            write!(f, "{}]", indent)
                        }
                    }
                }
            }
            Literal::Map(m) => match m.data.len() {
                0 => write!(f, "{}{{}}", indent),
                1 => {
                    let (k, v) = m.data.first().unwrap();
                    write!(f, "{}{{{k}: {v}}}", indent)
                }
                _ => {
                    writeln!(f, "{}{{", indent)?;
                    for (k, v) in &m.data {
                        match k {
                            Expr::Ident(k, _, _) => {
                                k.fmt_with_indent(f, indent_level + 1)?;
                            }
                            Expr::Literal(k, _) => {
                                k.fmt_with_indent(f, indent_level + 1)?;
                            }
                            _ => {
                                k.fmt_with_indent(f, indent_level + 1)?;
                            }
                        }
                        writeln!(f, ": {v},")?;
                    }
                    write!(f, "{}}}", indent)
                }
            },
            Literal::Nil => write!(f, "{}nil", indent),
        }
    }
}

impl From<&str> for Literal {
    fn from(s: &str) -> Self {
        match s.parse::<f64>() {
            Ok(f) => {
                if f.fract() == 0.0 {
                    Literal::I64(f as i64)
                } else {
                    Literal::F64(f)
                }
            }
            Err(_) => match s {
                "true" => Literal::Bool(true),
                "false" => Literal::Bool(false),
                "nil" => Literal::Nil,
                _ => Literal::Str(s.to_string()),
            },
        }
    }
}

impl TryInto<usize> for Literal {
    type Error = ();

    fn try_into(self) -> Result<usize, Self::Error> {
        match self {
            Literal::U32(i) => Ok(i as usize),
            Literal::U64(i) => Ok(i as usize),
            Literal::I32(i) => Ok(i as usize),
            Literal::I64(i) => Ok(i as usize),
            _ => Err(()),
        }
    }
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.fmt_with_indent(f, 0)
    }
}
