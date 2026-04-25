use std::{
    env,
    fs::{self, File},
    io::{BufWriter, Write},
    path::Path,
    time::Instant,
};

use coal_compiler::Compiler;
use rkyv::{rancor, to_bytes};

use crate::{hash_str, path::resolve_main, read_file_to_str, status};

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

    let target_dir = root.join("target");
    let bin_path = target_dir.join(format!("{filename}.bin"));
    let hash_path = target_dir.join(format!("{filename}.hash"));

    let hash = format!("{:016x}", hash_str(&input));

    if bin_path.exists() && fs::read_to_string(&hash_path).is_ok_and(|h| h.trim() == hash) {
        if !quiet {
            println!("{} {name} ({})", status("Finished"), project_root.display());
        }
        return;
    }

    if !quiet {
        println!(
            "{} {name} ({})",
            status("Compiling"),
            project_root.display()
        );
    }

    let start = Instant::now();

    let mut c = Compiler::new();
    c.compile(&input).unwrap();
    let bytecode = c.bytecode();

    fs::create_dir_all(&target_dir).unwrap();
    let file = File::create(&bin_path).unwrap();

    let mut writer = BufWriter::new(file);

    let Ok(bytes) = to_bytes::<rancor::Error>(&bytecode) else {
        eprintln!("Failed to serialize bytecode");
        return;
    };
    if let Err(e) = writer.write_all(&bytes.into_vec()) {
        eprintln!("Failed to write bytecode: {e}");
        return;
    }

    if let Err(e) = fs::write(&hash_path, &hash) {
        eprintln!("Failed to write hash: {e}");
        return;
    }

    let elapsed = start.elapsed().as_millis();
    if !quiet {
        println!(
            "{} `{}` in {elapsed}ms",
            status("Finished"),
            bin_path.display()
        );
    }
}
