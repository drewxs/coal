use std::{
    env, fs,
    path::{Path, PathBuf},
};

use rustyline::{error::ReadlineError, DefaultEditor, Result};

use crate::{config::VERSION, Parser};

pub fn repl() -> Result<()> {
    println!("Coal {VERSION}");

    let history_path = get_history_path();

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

    if let Some(path) = history_path.parent() {
        let _ = fs::create_dir(path);
        let _ = rl.save_history(&history_path);
    }

    Ok(())
}

fn get_history_path() -> PathBuf {
    let home_dir = dirs::home_dir().unwrap_or_default();
    let data_dir = env::var("XDG_DATA_HOME")
        .unwrap_or(String::from("~/.local/share"))
        .replace("~", &home_dir.to_string_lossy());
    let data_path = Path::new(&data_dir);

    if !data_path.exists() {
        return home_dir.join(".coal_history");
    }

    let path = data_path.join("coal/history");
    let _ = fs::create_dir_all(&path.parent().unwrap());
    path
}
