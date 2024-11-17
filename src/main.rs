use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();
    match args.len() {
        4 => match args[1].as_str() {
            "fmt" => match args[2].as_str() {
                "--write" | "-w" => match coal::fmt_file(args[3].as_str(), true) {
                    Ok(output) => println!("{output}"),
                    Err(err) => eprintln!("{err}"),
                },
                _ => coal::help(),
            },
            _ => coal::help(),
        },
        3 => match args[1].as_str() {
            "fmt" => match coal::fmt_file(args[2].as_str(), false) {
                Ok(output) => println!("{output}"),
                Err(err) => eprintln!("{err}"),
            },
            _ => coal::help(),
        },
        2 => match args[1].as_str() {
            "help" | "--help" | "-h" => coal::help(),
            filename => coal::run(filename),
        },
        1 => coal::repl(),
        _ => coal::help(),
    }
}
