use std::{
    fs::{self, File},
    io::{BufWriter, Write},
    path::Path,
    time::Instant,
};

use coal_compiler::Compiler;
use rkyv::{rancor, to_bytes};

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

    let start = Instant::now();

    let mut c = Compiler::new();
    c.compile(&input).unwrap();
    let bytecode = c.bytecode();

    let target_dir = root.join("target");
    fs::create_dir_all(&target_dir).unwrap();
    let path = target_dir.join(format!("{filename}.bin"));
    let file = File::create(path.clone()).unwrap();

    let mut writer = BufWriter::new(file);

    let Ok(bytes) = to_bytes::<rancor::Error>(&bytecode) else {
        eprintln!("Failed to serialize bytecode");
        return;
    };
    if let Err(e) = writer.write_all(&bytes.into_vec()) {
        eprintln!("Failed to write bytecode: {e}");
        return;
    }

    let elapsed = start.elapsed().as_millis();
    println!("Finished `{}` in {elapsed}ms", path.display());
}
