use std::{
    io::{self, IsTerminal, Read},
    process,
};

use crate::read_file_to_str;

pub fn read_stdin() -> Option<String> {
    let mut input = io::stdin();
    if input.is_terminal() {
        return None;
    }

    let mut buf = String::new();
    input.read_to_string(&mut buf).ok()?;

    Some(buf)
}

pub fn resolve_input(input: Option<String>, path: Option<String>) -> String {
    if let Some(input) = input {
        input
    } else if let Some(path) = path {
        read_file_to_str(&path)
    } else if let Some(input) = read_stdin() {
        input
    } else {
        eprintln!("No input provided");
        process::exit(1);
    }
}
