use std::fs;

use clap::Parser;

use coal::{CacheCommand, Cli, Command, DataCommand};

fn main() {
    let args = Cli::parse();

    match args.cmd {
        Some(Command::Fmt { path, dry_run }) => {
            let path = path.unwrap_or(String::from("."));
            match coal::fmt_path(&path, dry_run) {
                Ok(output) => println!("{output}"),
                Err(err) => eprintln!("{err}"),
            }
        }
        Some(Command::Data { cmd }) => match cmd {
            DataCommand::Clear => match fs::remove_dir_all(coal::path::data()) {
                Ok(_) => println!("Data cleared"),
                Err(err) => eprintln!("{err}"),
            },
        },
        Some(Command::Cache { cmd }) => match cmd {
            CacheCommand::Clear => match fs::remove_dir_all(coal::path::data()) {
                Ok(_) => println!("Cache cleared"),
                Err(err) => eprintln!("{err}"),
            },
        },
        None => match args.file {
            Some(path) => coal::run(&path),
            None => coal::repl().unwrap(),
        },
    }
}
