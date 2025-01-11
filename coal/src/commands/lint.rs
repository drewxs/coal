use coal_core::{clean_input, Parser};

pub fn lint(input: &str) {
    let mut parser = Parser::from(input);
    parser.parse();

    if parser.errors.is_empty() {
        println!("\x1b[32mNo errors\x1b[0m");
        return;
    }

    for e in parser.errors {
        let ((l1, c1), (_, c2)) = e.span;

        if input.lines().count() > 1 {
            let line = input.lines().nth(l1 - 1).unwrap();
            println!("\x1b[31m{}\x1b[0m", "-".repeat(75));
            println!("{}", clean_input(line));
            println!(
                "\x1b[31m{}\x1b[0m",
                " ".repeat(c1 - 1) + &"^".repeat(c2 - c1 + 1),
            );
            println!("{e}");
        } else {
            println!("\x1b[31m{}\x1b[0m", "-".repeat(75));
            println!("{}", clean_input(input));
            println!(
                "\x1b[31m{}\x1b[0m",
                " ".repeat(c1 - 1) + &"^".repeat(c2 - c1 + 1),
            );
            println!("{}\n", e.kind);
        }
    }
}
