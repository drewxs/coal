use std::{env, fs::File, io::Read, process};

use coal::{repl, Lexer, Token};

fn main() {
    let args: Vec<String> = env::args().collect();
    match args.len() {
        2 => match args[1].as_str() {
            "--help" | "-h" => help(),
            filename => run(filename),
        },
        1 => repl(),
        _ => help(),
    }
}

fn run(filename: &str) {
    let mut file = File::open(filename).unwrap_or_else(|_| {
        eprintln!("file not found: {filename}");
        process::exit(1);
    });

    let mut input = String::new();
    file.read_to_string(&mut input).unwrap_or_else(|_| {
        eprintln!("failed to read file: {filename}");
        process::exit(1);
    });

    let mut lexer = Lexer::new(&input);

    let mut token = lexer.next_tok();
    while token != Token::EOF {
        println!("{token}");
        token = lexer.next_tok();
    }
}

fn help() {
    println!("Usage: coal [file?]")
}
