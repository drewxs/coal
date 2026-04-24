use std::{
    fs::File,
    io::{BufReader, Read},
    rc::Rc,
};

use colored::Colorize;
use terminal_size::{Width, terminal_size};

use coal_compiler::{Bytecode, Compiler};
use coal_core::{ParserError, clean_input};
use coal_objects::Object;
use coal_vm::VM;

use rkyv::{from_bytes, rancor};

use crate::{compile, path::resolve_bin, read_file_to_str};

/// Run a binary (e.g. `main.coal.bin`)
pub fn run(path: &str) {
    let target = resolve_bin("target", path).unwrap_or_else(|_| {
        compile(".", false);
        resolve_bin("target", path).unwrap()
    });
    let file = File::open(&target).unwrap();
    let reader = BufReader::new(file);

    let Ok(bytes) = reader.bytes().collect::<Result<Vec<u8>, _>>() else {
        eprintln!("Failed to read bytecode");
        return;
    };
    let Ok(bytecode) = from_bytes::<Bytecode, rancor::Error>(&bytes) else {
        eprintln!("Failed to deserialize bytecode");
        return;
    };

    let mut vm = VM::from(bytecode);
    vm.run();
}

/// Compile and run a given file
pub fn exec(path: &str) {
    exec_input(&read_file_to_str(path), &mut Compiler::new());
}

/// Compile and run the given input
pub fn exec_input(input: &str, c: &mut Compiler) {
    match c.compile(input) {
        Ok(bytecode) => {
            let mut vm = VM::from(bytecode);
            vm.run();
        }
        Err(errs) => print_errs(input, &errs),
    }
}

/// Compile and run a single REPL input, carrying `globals` across calls.
/// Each call gets fresh instructions, persisting constants, symbol table, and globals.
/// Prints last statement if it's an expression.
pub fn exec_repl_input(input: &str, c: &mut Compiler, globals: &mut Vec<Rc<Object>>) {
    match c.compile_repl(input) {
        Ok((bytecode, echo_last)) => {
            let mut vm = VM::from(bytecode);
            if !globals.is_empty() {
                vm.globals = std::mem::take(globals);
            }
            vm.run();
            *globals = std::mem::take(&mut vm.globals);

            if echo_last {
                let val = vm.last_stack_obj();
                if *val != Object::Nil {
                    println!("{val}");
                }
            }
        }
        Err(errs) => print_errs(input, &errs),
    }
}

fn print_errs(input: &str, errs: &[ParserError]) {
    let term_w = terminal_size().map(|(w, _)| w).unwrap_or(Width(80)).0 as usize;
    let multiline = input.lines().count() > 1;
    for e in errs {
        let ((l1, c1), (_, c2)) = e.span;
        let caret = " ".repeat(c1 - 1) + &"^".repeat(c2.saturating_sub(c1) + 1);
        println!("{}", "-".repeat(term_w).red());
        if multiline {
            let line = input.lines().nth(l1 - 1).unwrap_or("");
            println!("{}", clean_input(line));
            println!("{}", caret.red());
            println!("{e}");
        } else {
            println!("{}", clean_input(input));
            println!("{}", caret.red());
            println!("{}\n", e.kind);
        }
    }
}
