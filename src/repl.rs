use std::io::{self, Write};

use crate::{config::VERSION, Lexer};

pub fn repl() {
    println!("Coal {VERSION}");
    loop {
        print!(">> ");
        io::stdout().flush().unwrap();

        let mut input = String::new();
        io::stdin().read_line(&mut input).unwrap();

        if input.trim() == "exit" {
            break;
        }

        let lexer = Lexer::new(&input);

        for token in lexer {
            println!("{token}");
            io::stdout().flush().unwrap();
        }
    }
}
