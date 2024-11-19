use rustyline::{error::ReadlineError, DefaultEditor, Result};

use crate::config::VERSION;
use crate::Parser;

pub fn repl() -> Result<()> {
    println!("Coal {VERSION}");

    let mut rl = DefaultEditor::new()?;
    if rl.load_history("history.txt").is_err() {
        println!("No previous history.");
    }

    loop {
        let readline = rl.readline(">> ");
        match readline {
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

    let _ = rl.save_history("history.txt");

    Ok(())
}
