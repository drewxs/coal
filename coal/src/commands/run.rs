use std::{fs::File, io::Read, process};

use coal_core::{Lexer, Parser};

use super::eval;

pub fn run(path: &str) {
    eval(&contents(path));
}

pub fn print_tokens(input: &str) {
    Lexer::new(input).print_tokens();
}

pub fn print_ast(input: &str) {
    for stmt in Parser::from(input).parse() {
        println!("{stmt:?}");
    }
}

pub fn print_errors(input: &str) {
    let mut parser = Parser::from(input);
    parser.parse();
    parser.print_errors();
}

pub fn print_file_tokens(path: &str) {
    print_tokens(&contents(path));
}

pub fn print_file_ast(path: &str) {
    print_ast(&contents(path));
}

fn contents(path: &str) -> String {
    let mut file = File::open(path).unwrap_or_else(|_| {
        eprintln!("file not found: {path}");
        process::exit(1);
    });

    let mut input = String::new();
    file.read_to_string(&mut input).unwrap_or_else(|_| {
        eprintln!("failed to read file: {path}");
        process::exit(1);
    });

    input
}
