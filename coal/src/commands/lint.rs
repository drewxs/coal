use terminal_size::{terminal_size, Width};

use coal_core::{clean_input, Parser};

pub fn lint(input: &str) {
    let mut parser = Parser::from(input);
    parser.parse();

    if parser.errors.is_empty() && parser.warnings.is_empty() {
        println!("\x1b[32mNo errors\x1b[0m");
        return;
    }

    let term_w = terminal_size().map(|(w, _)| w).unwrap_or(Width(80)).0 as usize;

    for e in parser.errors {
        let ((l1, c1), (_, c2)) = e.span;

        if input.lines().count() > 1 {
            let line = input.lines().nth(l1 - 1).unwrap();
            println!("\x1b[31m{}\x1b[0m", "-".repeat(term_w));
            println!("{}", clean_input(line));
            println!(
                "\x1b[31m{}\x1b[0m",
                " ".repeat(c1 - 1) + &"^".repeat(c2.saturating_sub(c1) + 1),
            );
            println!("\x1b[31merror\x1b[0m: {}", e);
        } else {
            println!("\x1b[31m{}\x1b[0m", "-".repeat(term_w));
            println!("{}", clean_input(input));
            println!(
                "\x1b[31m{}\x1b[0m",
                " ".repeat(c1 - 1) + &"^".repeat(c2.saturating_sub(c1) + 1),
            );
            println!("\x1b[31merror\x1b[0m: {}", e.kind);
        }
    }

    for w in parser.warnings {
        let ((l1, c1), (_, c2)) = w.span;

        if input.lines().count() > 1 {
            let line = input.lines().nth(l1 - 1).unwrap();
            println!("\x1b[33m{}\x1b[0m", "-".repeat(term_w));
            println!("{}", clean_input(line));
            println!(
                "\x1b[33m{}\x1b[0m",
                " ".repeat(c1 - 1) + &"^".repeat(c2.saturating_sub(c1) + 1),
            );
            println!("\x1b[33mwarning\x1b[0m: {}", w);
        } else {
            println!("\x1b[33m{}\x1b[0m", "-".repeat(term_w));
            println!("{}", clean_input(input));
            println!(
                "\x1b[33m{}\x1b[0m",
                " ".repeat(c1 - 1) + &"^".repeat(c2.saturating_sub(c1) + 1),
            );
            println!("\x1b[33mwarning\x1b[0m: {}", w.kind);
        }
    }
}
