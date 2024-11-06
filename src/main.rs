use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();
    match args.len() {
        2 => match args[1].as_str() {
            "--help" | "-h" => coal::help(),
            filename => coal::run(filename),
        },
        1 => coal::repl(),
        _ => coal::help(),
    }
}
