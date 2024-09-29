use std::io::{self, Write};

use crate::{config::VERSION, Lexer, Parser};

pub fn repl() {
    println!("Coal {VERSION}");
    loop {
        print!(">> ");
        io::stdout().flush().unwrap();

        let mut input = String::new();
        io::stdin().read_line(&mut input).unwrap();

        match input.trim() {
            "exit" | "quit" => break,
            _ => {}
        }

        let lexer = Lexer::new(&input);
        let mut parser = Parser::new(lexer.clone());
        parser.parse();
        parser.check();

        // for token in lexer {
        //     println!("{token}");
        //     io::stdout().flush().unwrap();
        // }
    }
}
