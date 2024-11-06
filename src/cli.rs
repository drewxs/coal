use std::{fs::File, io::Read, process};

use crate::Program;
use std::io::{self, Write};

use crate::{config::VERSION, Parser};

pub fn help() {
    println!("Usage: coal [file?]")
}

pub fn run(filename: &str) {
    let mut file = File::open(filename).unwrap_or_else(|_| {
        eprintln!("file not found: {filename}");
        process::exit(1);
    });

    let mut input = String::new();
    file.read_to_string(&mut input).unwrap_or_else(|_| {
        eprintln!("failed to read file: {filename}");
        process::exit(1);
    });

    println!("{:#?}", Program::parse(&input).statements);
}

pub fn repl() {
    println!("Coal {VERSION}");
    loop {
        print!(">> ");
        io::stdout().flush().unwrap();

        let mut input = String::new();
        io::stdin().read_line(&mut input).unwrap();

        match input.trim() {
            "exit" | "quit" => break,
            "clear" => {
                println!("\x1B[2J\x1B[1;1H");
                continue;
            }
            _ => {}
        }

        let mut parser = Parser::from(&input);
        let program = parser.parse();
        parser.print_errors();

        println!("{:#?}", program.statements);
    }
}
