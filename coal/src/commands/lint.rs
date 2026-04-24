use std::fs;

use colored::Colorize;
use terminal_size::{Width, terminal_size};

use coal_core::{Parser, clean_input};

use crate::path::program_files;

pub fn lint(input: &str) {
    let mut parser = Parser::from(input);
    parser.parse();

    if parser.errors.is_empty() && parser.warnings.is_empty() {
        println!("{}", "No errors".green());
        return;
    }

    let term_w = terminal_size().map(|(w, _)| w).unwrap_or(Width(80)).0 as usize;

    for e in parser.errors {
        let ((l1, c1), (_, c2)) = e.span;
        let caret = " ".repeat(c1 - 1) + &"^".repeat(c2.saturating_sub(c1) + 1);

        if input.lines().count() > 1 {
            let line = input.lines().nth(l1 - 1).unwrap();
            println!("{}", "-".repeat(term_w).red());
            println!("{}", clean_input(line));
            println!("{}", caret.red());
            println!("{}: {e}", "error".red());
        } else {
            println!("{}", "-".repeat(term_w).red());
            println!("{}", clean_input(input));
            println!("{}", caret.red());
            println!("{}: {}", "error".red(), e.kind);
        }
    }

    for w in parser.warnings {
        let ((l1, c1), (_, c2)) = w.span;
        let caret = " ".repeat(c1 - 1) + &"^".repeat(c2.saturating_sub(c1) + 1);

        if input.lines().count() > 1 {
            let line = input.lines().nth(l1 - 1).unwrap();
            println!("{}", "-".repeat(term_w).yellow());
            println!("{}", clean_input(line));
            println!("{}", caret.yellow());
            println!("{}: {w}", "warning".yellow());
        } else {
            println!("{}", "-".repeat(term_w).yellow());
            println!("{}", clean_input(input));
            println!("{}", caret.yellow());
            println!("{}: {}", "warning".yellow(), w.kind);
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
