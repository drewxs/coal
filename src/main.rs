use std::io::Read;

use coal::lexer::Lexer;

fn main() {
    let mut input = String::new();
    std::io::stdin().read_to_string(&mut input).unwrap();

    let mut lexer = Lexer::new(&input);

    loop {
        println!("{}", lexer.next_token());
    }
}
