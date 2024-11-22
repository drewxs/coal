use rustyline::{error::ReadlineError, DefaultEditor, Result};

use crate::{config::VERSION, path, Evaluator};

pub fn repl() -> Result<()> {
    println!("Coal {VERSION}");

    let history_path = path::history();

    let mut rl = DefaultEditor::new()?;
    let _ = rl.load_history(&history_path);
    let mut evaluator = Evaluator::default();

    loop {
        match rl.readline(">> ") {
            Ok(line) => match line.trim() {
                "exit" | "quit" => break,
                "clear" => println!("\x1B[2J\x1B[1;1H"),
                _ => {
                    if let Some(evaluated) = evaluator.eval(&line) {
                        println!("{evaluated}");
                    }
                    let _ = rl.add_history_entry(line);
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
