use std::{fs::File, io::BufReader};

use bincode::decode_from_std_read;
use terminal_size::{Width, terminal_size};

use coal_compiler::{Bytecode, Compiler};
use coal_core::clean_input;
use coal_vm::VM;

use crate::{compile, path::resolve_bin, read_file_to_str};

/// Run a binary (e.g. `main.coal.bin`)
pub fn run(path: &str) {
    let path_res = resolve_bin("target", path);
    if path_res.is_err() {
        compile(".");
    }

    let path = resolve_bin("target", path).unwrap();
    let file = File::open(&path).unwrap();
    let mut reader = BufReader::new(file);

    let config = bincode::config::standard();
    let bytecode: Bytecode = decode_from_std_read(&mut reader, config).unwrap();
    let mut vm = VM::from(bytecode);

    vm.run();

    println!("{}", vm.last_stack_obj());
}

/// JIT compile and run a given file
pub fn compile_and_run(path: &str) {
    compile_and_exec(&read_file_to_str(path), &mut Compiler::new());
}

/// JIT compile and run the given input
pub fn compile_and_exec(input: &str, c: &mut Compiler) {
    match c.compile(input) {
        Ok(bytecode) => {
            let mut vm = VM::from(bytecode);
            vm.run();
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
