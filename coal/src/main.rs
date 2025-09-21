use std::fs;

use clap::Parser;

use coal::{CacheCommand, Cli, Command, DataCommand, path, resolve_input};

fn main() {
    let args = Cli::parse();

    let Some(cmd) = args.cmd else {
        return coal::repl();
    };

    match cmd {
        Command::New { name } => match coal::new(&name) {
            Ok(msg) => println!("{msg}"),
            Err(err) => eprintln!("{err}"),
        },
        Command::Compile => {
            coal::compile(".");
        }
        Command::Run => {
            coal::run("main");
        }
        Command::Exec { path } => {
            coal::compile_and_run(&path);
        }
        Command::Parse {
            input,
            path,
            tokens,
            ast,
        } => {
            let input = resolve_input(input, path);
            if tokens {
                coal::print_tokens(&input);
            } else if ast {
                coal::print_ast(&input);
            }
        }
        Command::Lint { input, path } => {
            let input = resolve_input(input, path);
            coal::lint(&input);
        }
        Command::Fmt {
            input,
            path,
            dry_run,
        } => {
            if let Some(path) = path {
                match coal::fmt_path(&path, dry_run) {
                    Ok(output) => println!("{output}"),
                    Err(err) => eprintln!("{err}"),
                }
            } else {
                let input = resolve_input(input, path);
                coal::fmt(&input);
            }
        }
        Command::Data { cmd } => match cmd {
            DataCommand::Clear => match fs::remove_dir_all(path::data()) {
                Ok(_) => println!("Data cleared"),
                Err(err) => eprintln!("{err}"),
            },
        },
        Command::Cache { cmd } => match cmd {
            CacheCommand::Clear => match fs::remove_dir_all(path::data()) {
                Ok(_) => println!("Cache cleared"),
                Err(err) => eprintln!("{err}"),
            },
        },
    }
}
