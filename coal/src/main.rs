use std::fs;

use clap::Parser;

use coal::{path, CacheCommand, Cli, Command, DataCommand};

fn main() {
    let args = Cli::parse();

    if let Some(cmd) = args.cmd {
        return match cmd {
            Command::New { name } => match coal::new(&name) {
                Ok(msg) => println!("{msg}"),
                Err(err) => eprintln!("{err}"),
            },
            Command::Run { path, tokens, ast } => match path::main_program(path) {
                Ok(path) => {
                    println!("path: {path}");
                    if tokens {
                        coal::print_file_tokens(&path);
                    } else if ast {
                        coal::print_file_ast(&path);
                    } else {
                        coal::run(&path);
                    }
                }
                Err(err) => eprintln!("{err}"),
            },
            Command::Eval { input, tokens, ast } => {
                println!("input: {input:?}");
                if tokens {
                    // coal::print_tokens(&input);
                } else if ast {
                    // coal::print_ast(&input);
                } else {
                    // coal::eval(&input);
                }
            }
            Command::Lint { input, path } => {
                if let Some(path) = path {
                    coal::lint_path(&path);
                } else if let Some(input) = input {
                    coal::lint(&input);
                } else {
                    coal::lint_stdin();
                }
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
                } else if let Some(input) = input {
                    println!("{}", coal::fmt(&input));
                } else {
                    match coal::fmt_stdin() {
                        Ok(output) => println!("{output}"),
                        Err(_) => match coal::fmt_path(".", dry_run) {
                            Ok(output) => println!("{output}"),
                            Err(err) => eprintln!("{err}"),
                        },
                    }
                }
            }
            Command::Data { cmd } => match cmd {
                DataCommand::Clear => match fs::remove_dir_all(coal::path::data()) {
                    Ok(_) => println!("Data cleared"),
                    Err(err) => eprintln!("{err}"),
                },
            },
            Command::Cache { cmd } => match cmd {
                CacheCommand::Clear => match fs::remove_dir_all(coal::path::data()) {
                    Ok(_) => println!("Cache cleared"),
                    Err(err) => eprintln!("{err}"),
                },
            },
        };
    }

    coal::repl()
}
