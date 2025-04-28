use std::fmt;

use crate::indent;

use super::{F32, F64, I32, I64, I128, List, Map, Type, U32, U64};

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

    pub fn cast(&self, to: &Type) -> Literal {
        match (self, to) {
            (Literal::U32(from), &U64) => Literal::U64(*from as u64),
            (Literal::U32(from), &I32) => Literal::I32(*from as i32),
            (Literal::U32(from), &I64) => Literal::I64(*from as i64),
            (Literal::U32(from), &I128) => Literal::I128(*from as i128),
            (Literal::U32(from), &F32) => Literal::F32(*from as f32),
            (Literal::U32(from), &F64) => Literal::F64(*from as f64),

            (Literal::U64(from), &U32) => Literal::U32(*from as u32),
            (Literal::U64(from), &I32) => Literal::I32(*from as i32),
            (Literal::U64(from), &I64) => Literal::I64(*from as i64),
            (Literal::U64(from), &I128) => Literal::I128(*from as i128),
            (Literal::U64(from), &F32) => Literal::F32(*from as f32),
            (Literal::U64(from), &F64) => Literal::F64(*from as f64),

            (Literal::I32(from), &U32) => Literal::U32(*from as u32),
            (Literal::I32(from), &U64) => Literal::U64(*from as u64),
            (Literal::I32(from), &I64) => Literal::I64(*from as i64),
            (Literal::I32(from), &I128) => Literal::I128(*from as i128),
            (Literal::I32(from), &F32) => Literal::F32(*from as f32),
            (Literal::I32(from), &F64) => Literal::F64(*from as f64),

            (Literal::I64(from), &U32) => Literal::U32(*from as u32),
            (Literal::I64(from), &U64) => Literal::U64(*from as u64),
            (Literal::I64(from), &I32) => Literal::I32(*from as i32),
            (Literal::I64(from), &I128) => Literal::I128(*from as i128),
            (Literal::I64(from), &F32) => Literal::F32(*from as f32),
            (Literal::I64(from), &F64) => Literal::F64(*from as f64),

            (Literal::I128(from), &U32) => Literal::U32(*from as u32),
            (Literal::I128(from), &U64) => Literal::U64(*from as u64),
            (Literal::I128(from), &I32) => Literal::I32(*from as i32),
            (Literal::I128(from), &I64) => Literal::I64(*from as i64),
            (Literal::I128(from), &F32) => Literal::F32(*from as f32),
            (Literal::I128(from), &F64) => Literal::F64(*from as f64),

            (Literal::F32(from), &U32) => Literal::U32(*from as u32),
            (Literal::F32(from), &U64) => Literal::U64(*from as u64),
            (Literal::F32(from), &I32) => Literal::I32(*from as i32),
            (Literal::F32(from), &I64) => Literal::I64(*from as i64),
            (Literal::F32(from), &I128) => Literal::I128(*from as i128),
            (Literal::F32(from), &F64) => Literal::F64(*from as f64),

            (Literal::F64(from), &U32) => Literal::U32(*from as u32),
            (Literal::F64(from), &U64) => Literal::U64(*from as u64),
            (Literal::F64(from), &I32) => Literal::I32(*from as i32),
            (Literal::F64(from), &I64) => Literal::I64(*from as i64),
            (Literal::F64(from), &I128) => Literal::I128(*from as i128),
            (Literal::F64(from), &F32) => Literal::F32(*from as f32),

            _ => self.clone(),
        }
    }

    pub fn fmt_with_indent(
        &self,
        f: &mut fmt::Formatter,
        indent_level: usize,
        inner: bool,
    ) -> fmt::Result {
        let base_indent = indent(indent_level);
        let inner_indent = if inner { "" } else { &base_indent };

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
            Literal::List(l) => l.fmt_with_indent(f, indent_level, inner),
            Literal::Map(m) => m.fmt_with_indent(f, indent_level, inner),
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
