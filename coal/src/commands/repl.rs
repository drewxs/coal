use coal_core::Evaluator;
use rustyline::{error::ReadlineError, DefaultEditor, Result};

pub const VERSION: &str = env!("CARGO_PKG_VERSION");

pub fn repl() -> Result<()> {
    println!("Coal {VERSION}");

    let history_path = crate::path::history();

    let mut rl = DefaultEditor::new()?;
    let _ = rl.load_history(&history_path);
    let mut evaluator = Evaluator::default();

    loop {
        match rl.readline(">> ") {
            Ok(line) => match line.trim() {
                "exit" | "quit" => break,
                "clear" => println!("\x1B[2J\x1B[1;1H"),
                _ => {
                    evaluator.print_eval(&line);
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
