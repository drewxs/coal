use coal_core::{Lexer, Parser};

pub fn print_tokens(input: &str) {
    for token in Lexer::new(input) {
        println!("{token:?}");
    }
}

pub fn print_ast(input: &str) {
    for stmt in Parser::from(input).parse() {
        println!("{stmt:?}");
    }
}
