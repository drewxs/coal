use rustyline::{error::ReadlineError, DefaultEditor, Result};

use crate::{config::VERSION, path, Parser};

pub fn repl() -> Result<()> {
    println!("Coal {VERSION}");

    let history_path = path::history();

    let mut rl = DefaultEditor::new()?;
    let _ = rl.load_history(&history_path);

    loop {
        match rl.readline(">> ") {
            Ok(line) => match line.trim() {
                "exit" | "quit" => break,
                "clear" => println!("\x1B[2J\x1B[1;1H"),
                _ => {
                    let mut parser = Parser::from(&line);
                    let program = parser.parse();
                    parser.print_errors();
                    let _ = rl.add_history_entry(line);

                    println!("{program}");
                }
            },
            Err(ReadlineError::Interrupted) => {
                println!("CTRL-C");
                break;
            }
            Err(ReadlineError::Eof) => {
                println!("CTRL-D");
                break;
            }
            Err(err) => {
                println!("Error: {err:?}");
                break;
            }
        }
    }

    let _ = rl.save_history(&history_path);

    Ok(())
}
