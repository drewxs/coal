use clap::Parser;

use coal::Cli;

fn main() {
    let args = Cli::parse();

    match args.cmd {
        Some(coal::Command::Fmt { path, dry_run }) => {
            let path = path.unwrap_or(String::from("."));
            match coal::fmt_path(path.as_str(), dry_run) {
                Ok(output) => println!("{output}"),
                Err(err) => eprintln!("{err}"),
            }
        }
        None => match args.file {
            Some(path) => coal::run(&path),
            None => coal::repl(),
        },
    }
}
