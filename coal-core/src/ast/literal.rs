use std::fmt;

use super::{List, Map, Type};

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
    pub fn is_primitive(&self) -> bool {
        !self.is_composite()
    }

    pub fn is_composite(&self) -> bool {
        matches!(self, Literal::List(_) | Literal::Map(_))
    }

    pub fn is_indexable(&self) -> bool {
        matches!(self, Literal::Str(_) | Literal::List(_) | Literal::Map(_))
    }

    pub fn is_defined(&self) -> bool {
        match self {
            Literal::List(l) => l.t.is_defined(),
            Literal::Map(m) => m.t.0.is_defined() && m.t.1.is_defined(),
            _ => true,
        }
    }

    pub fn is_empty(&self) -> bool {
        match self {
            Literal::List(l) => l.data.is_empty(),
            Literal::Map(m) => m.data.is_empty(),
            _ => false,
        }
    }

    pub fn set_type(&mut self, t1: Type, t2: Option<Type>) {
        match self {
            Literal::List(l) => l.t = t1,
            Literal::Map(m) => {
                m.t.0 = t1;
                if let Some(t2) = t2 {
                    m.t.1 = t2;
                }
            }
            _ => {}
        }
    }

    pub fn fmt_with_indent(
        &self,
        f: &mut fmt::Formatter,
        indent_level: usize,
        inner: bool,
    ) -> fmt::Result {
        let indent = "    ".repeat(indent_level);
        let inner_indent = if inner { "" } else { &indent };

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

                write!(f, "{}\"{result}\"", inner_indent)
            }
            Literal::U32(i) => write!(f, "{}{i}", inner_indent),
            Literal::U64(i) => write!(f, "{}{i}", inner_indent),
            Literal::I32(i) => write!(f, "{}{i}", inner_indent),
            Literal::I64(i) => write!(f, "{}{i}", inner_indent),
            Literal::I128(i) => write!(f, "{}{i}", inner_indent),
            Literal::F32(x) => write!(f, "{}{x:?}", inner_indent),
            Literal::F64(x) => write!(f, "{}{x:?}", inner_indent),
            Literal::Bool(b) => write!(f, "{}{b}", inner_indent),
            Literal::List(l) => {
                if let (Some(n), Some(e)) = (&l.repeat, l.data.first()) {
                    write!(f, "{}[{e}; {n}]", inner_indent)
                } else {
                    match l.data.len() {
                        0 => write!(f, "{}[]", inner_indent),
                        1 | 2 => {
                            let v = l
                                .data
                                .iter()
                                .map(|x| x.to_string())
                                .collect::<Vec<_>>()
                                .join(", ");
                            write!(f, "{}[{v}]", inner_indent)
                        }
                        _ => {
                            writeln!(f, "{}[", inner_indent)?;
                            for item in &l.data {
                                item.fmt_inner(f, indent_level + 1)?;
                                writeln!(f, ",")?;
                            }
                            write!(f, "{}]", indent)
                        }
                    }
                }
            }
            Literal::Map(m) => match m.data.len() {
                0 => write!(f, "{}{{}}", inner_indent),
                1 => {
                    let (k, v) = m.data.first().unwrap();
                    write!(f, "{}{{{k}: {v}}}", inner_indent)
                }
                _ => {
                    writeln!(f, "{}{{", inner_indent)?;
                    for (k, v) in &m.data {
                        k.fmt_inner(f, indent_level + 1)?;
                        writeln!(f, ": {v},")?;
                    }
                    write!(f, "{}}}", indent)
                }
            },
            Literal::Nil => write!(f, "{}nil", inner_indent),
        }
    }
}

impl From<&str> for Literal {
    fn from(s: &str) -> Self {
        match s.parse::<f64>() {
            Ok(f) => match f.fract() {
                0.0 => Literal::I64(f as i64),
                _ => Literal::F64(f),
            },
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
        self.fmt_with_indent(f, 0, false)
    }
}
