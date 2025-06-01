use std::{
    fs,
    io::{self, IsTerminal, Read},
};

use terminal_size::{Width, terminal_size};

use coal_core::{Parser, clean_input};

use crate::path::program_files;

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
            println!("\x1b[31merror\x1b[0m: {e}");
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
            println!("\x1b[33mwarning\x1b[0m: {w}");
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

pub fn lint_path(path: &str) {
    for entry in program_files(path) {
        let path = entry.path().to_str().expect("invalid utf-8");
        let input = fs::read_to_string(path).unwrap_or(String::from("failed to read file"));
        lint(&input);
    }
}

pub fn lint_stdin() {
    let mut input = io::stdin();
    if input.is_terminal() {
        return;
    }

    let mut buf = String::new();
    let _ = input
        .read_to_string(&mut buf)
        .map_err(|_| String::from("Failed to read from stdin"));

    lint(&buf);
}
