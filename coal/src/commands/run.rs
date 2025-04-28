use std::{fs::File, io::Read, process};

use terminal_size::{Width, terminal_size};

use coal_compiler::Compiler;
use coal_core::{Lexer, Parser, clean_input};
use coal_vm::VM;

pub fn run(path: &str) {
    exec(&contents(path));
}

pub fn exec(input: &str) {
    let mut compiler = Compiler::new();
    exec_with(input, &mut compiler);
}

pub fn exec_with(input: &str, compiler: &mut Compiler) {
    match compiler.compile(input) {
        Ok(bytecode) => {
            let mut vm = VM::from(bytecode.clone());
            vm.run().unwrap();

            println!("{}", vm.last_stack_obj());
        }
        Err(errs) => errs.iter().for_each(|e| {
            let ((l1, c1), (_, c2)) = e.span;
            let term_w = terminal_size().map(|(w, _)| w).unwrap_or(Width(80)).0 as usize;

            if input.lines().count() > 1 {
                let line = input.lines().nth(l1 - 1).unwrap();
                println!("\x1b[31m{}\x1b[0m", "-".repeat(term_w));
                println!("{}", clean_input(line));
                println!(
                    "\x1b[31m{}\x1b[0m",
                    " ".repeat(c1 - 1) + &"^".repeat(c2.saturating_sub(c1) + 1),
                );
                println!("{e}");
            } else {
                println!("\x1b[31m{}\x1b[0m", "-".repeat(term_w));
                println!("{}", clean_input(input));
                println!(
                    "\x1b[31m{}\x1b[0m",
                    " ".repeat(c1 - 1) + &"^".repeat(c2.saturating_sub(c1) + 1),
                );
                println!("{}\n", e.kind);
            }
        }),
    }
}

pub fn print_tokens(input: &str) {
    Lexer::new(input).print_tokens();
}

pub fn print_ast(input: &str) {
    for stmt in Parser::from(input).parse() {
        println!("{stmt:?}");
    }
}

pub fn print_file_tokens(path: &str) {
    print_tokens(&contents(path));
}

pub fn print_file_ast(path: &str) {
    print_ast(&contents(path));
}

fn contents(path: &str) -> String {
    let mut file = File::open(path).unwrap_or_else(|_| {
        eprintln!("file not found: {path}");
        process::exit(1);
    });

    let mut input = String::new();
    file.read_to_string(&mut input).unwrap_or_else(|_| {
        eprintln!("failed to read file: {path}");
        process::exit(1);
    });

    input
}
