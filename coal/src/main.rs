use std::fs;

use clap::Parser;

use coal::{path, CacheCommand, Cli, Command, DataCommand};

fn main() {
    let args = Cli::parse();

    if let Some(cmd) = args.cmd {
        return match cmd {
            Command::Run { path, tokens } => match path::main_program(path) {
                Ok(path) => {
                    if tokens {
                        return coal::print_file_tokens(&path);
                    }
                    coal::run_file(&path);
                }
                Err(err) => eprintln!("{err}"),
            },
            Command::Eval { input } => coal::eval(&input),
            Command::Fmt { path, dry_run } => {
                let path = path.unwrap_or(String::from("."));
                match coal::fmt_path(&path, dry_run) {
                    Ok(output) => println!("{output}"),
                    Err(err) => eprintln!("{err}"),
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
