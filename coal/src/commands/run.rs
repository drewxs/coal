use std::{fs::File, io::Read, process};

use coal_core::Program;

pub fn run(path: &str) {
    let mut file = File::open(path).unwrap_or_else(|_| {
        eprintln!("file not found: {path}");
        process::exit(1);
    });

    let mut input = String::new();
    file.read_to_string(&mut input).unwrap_or_else(|_| {
        eprintln!("failed to read file: {path}");
        process::exit(1);
    });

    println!("{:#?}", Program::parse(&input).statements);
}
