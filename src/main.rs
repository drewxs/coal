use std::{env, fs::File, io::Read};

use coal::{lexer::Lexer, repl::repl, token::Token};

fn main() {
    let args: Vec<String> = env::args().collect();
    match args.len() {
        2 => run(&args[1]),
        1 => repl(),
        _ => println!("Usage: coal [file?]"),
    }
}

fn run(filename: &str) {
    let mut file = File::open(filename).unwrap();
    let mut input = String::new();
    file.read_to_string(&mut input).unwrap();
    let mut lexer = Lexer::new(&input);

    let mut token = lexer.next_token();
    while token != Token::EOF {
        println!("{token}");
        token = lexer.next_token();
    }
}
