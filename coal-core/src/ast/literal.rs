use std::fmt;

use super::{Expr, Type};

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
    List(Vec<Expr>, Type),
    Nil,
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

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
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

                write!(f, "\"{result}\"")
            }
            Literal::U32(i) => write!(f, "{i}"),
            Literal::U64(i) => write!(f, "{i}"),
            Literal::I32(i) => write!(f, "{i}"),
            Literal::I64(i) => write!(f, "{i}"),
            Literal::I128(i) => write!(f, "{i}"),
            Literal::F32(x) => write!(f, "{x:?}"),
            Literal::F64(x) => write!(f, "{x:?}"),
            Literal::Bool(b) => write!(f, "{b}"),
            Literal::List(l, _) => {
                let list = l
                    .iter()
                    .map(|e| e.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "[{list}]")
            }
            Literal::Nil => write!(f, "nil"),
        }
    }
}
