use std::{env, fs::File, io::Read};

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
    let mut file = File::open(filename).unwrap();
    let mut input = String::new();
    file.read_to_string(&mut input).unwrap();
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
