use std::{
    fs::{self, File},
    io::{BufWriter, Write},
    path::Path,
};

use bincode::encode_into_std_write;

use coal_compiler::Compiler;

use crate::{path::resolve_main, read_file_to_str};

/// Compile the project at the given working directory
pub fn compile(working_dir: &str) {
    let mut root = Path::new(working_dir);
    if root.join("src").exists() {
        root = root.parent().unwrap();
    }

    let Ok(path) = resolve_main(working_dir) else {
        eprintln!("main.coal not found");
        return;
    };
    let filename = Path::new(&path).file_name().unwrap().to_str().unwrap();
    let input = read_file_to_str(&path);

    let mut c = Compiler::new();
    c.compile(&input).unwrap();
    let bytecode = c.bytecode();

    let target_dir = root.join("target");
    let _ = fs::create_dir_all(&target_dir);
    let path = target_dir.join(format!("{filename}.bin"));
    let file = File::create(path.clone()).unwrap();
    let mut writer = BufWriter::new(file);
    let config = bincode::config::standard();

    encode_into_std_write(bytecode, &mut writer, config).unwrap();

    println!("Compiled: {}", path.display());

    writer.flush().unwrap();
}
