use std::io::{self, Write};

use crate::{config::VERSION, Parser};

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
