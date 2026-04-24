use std::{
    env,
    fs::{self, File},
    io::{BufWriter, Write},
    path::Path,
    time::Instant,
};

use coal_compiler::Compiler;
use rkyv::{rancor, to_bytes};

use crate::{path::resolve_main, read_file_to_str, status};

/// Compile the project at the given working directory
pub fn compile(working_dir: &str, quiet: bool) {
    let project_root = env::current_dir().unwrap_or_default().join(working_dir);
    let project_root = project_root.canonicalize().unwrap_or(project_root);
    let name = project_root
        .file_name()
        .and_then(|n| n.to_str())
        .unwrap_or("project");

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

    if !quiet {
        println!("{} {name} ({})", status("Compiling"), project_root.display());
    }

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
    if !quiet {
        println!("{} `{}` in {elapsed}ms", status("Finished"), path.display());
    }
}
