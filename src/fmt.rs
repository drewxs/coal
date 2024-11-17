use std::fs;

use crate::Program;

pub fn fmt(input: &str) -> String {
    Program::from(input).to_string()
}

pub fn fmt_file(path: &str, write: bool) -> Result<String, String> {
    let input = fs::read_to_string(path).unwrap_or(String::from("failed to read file"));
    let out = fmt(&input);

    if write {
        fs::write(path, out)
            .map(|_| String::from("file formatted"))
            .map_err(|_| String::from("failed to write file"))
    } else {
        Ok(out)
    }
}
