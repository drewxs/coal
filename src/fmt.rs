use std::{fs, time::Instant};

use ignore::Walk;

use crate::Program;

pub fn fmt(input: &str) -> (String, u128) {
    let start = Instant::now();
    let out = Program::from(input).to_string();
    let elapsed = start.elapsed().as_millis();
    (out, elapsed)
}

pub fn fmt_file(path: &str) -> Result<u128, String> {
    let input = fs::read_to_string(path).unwrap_or(String::from("failed to read file"));
    let (out, elapsed) = fmt(&input);

    match fs::write(path, out) {
        Ok(_) => Ok(elapsed),
        Err(_) => Err(String::from("failed to write file")),
    }
}

pub fn fmt_file_dry_run(path: &str) -> Result<String, String> {
    let input = fs::read_to_string(path).unwrap_or(String::from("failed to read file"));
    let (out, _) = fmt(&input);

    Ok(out)
}

pub fn fmt_path(path: &str, dry_run: bool) -> Result<String, String> {
    let mut out = String::new();

    let walker = Walk::new(path).flatten().filter(|e| {
        e.metadata().ok().map_or(false, |m| m.is_file())
            && e.path().extension().map_or(false, |ext| ext == "coal")
    });

    for entry in walker {
        let path = entry.path();
        let file_path = path.to_str().unwrap();

        if dry_run {
            let res = fmt_file_dry_run(file_path)?;
            out.push_str(&res);
        } else {
            let ms = fmt_file(file_path)?;
            out.push_str(&format!("{file_path} {ms}ms\n"));
        }
    }

    Ok(out)
}
