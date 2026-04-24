use std::{fs, path::Path};

use colored::Colorize;

/// Clean build artifacts
pub fn clean(working_dir: &str, dry_run: bool, quiet: bool) {
    let mut root = Path::new(working_dir);
    if root.join("src").exists() {
        root = root.parent().unwrap_or(root);
    }
    let target_dir = root.join("target");
    if !target_dir.exists() {
        return;
    }

    let (files, bytes) = dir_stats(&target_dir);

    if dry_run {
        if !quiet {
            println!(
                "     {} {files} file{}, {} total",
                "Summary".green().bold(),
                if files == 1 { "" } else { "s" },
                format_size(bytes),
            );
            println!(
                "{}: no files deleted due to --dry-run",
                "warning".yellow().bold(),
            );
        }
        return;
    }

    if let Err(e) = fs::remove_dir_all(&target_dir) {
        eprintln!("Failed to remove {}: {e}", target_dir.display());
        return;
    }

    if !quiet {
        println!(
            "     {} {files} file{}, {} total",
            "Removed".green().bold(),
            if files == 1 { "" } else { "s" },
            format_size(bytes),
        );
    }
}

fn dir_stats(path: &Path) -> (u64, u64) {
    let mut files = 0u64;
    let mut bytes = 0u64;

    let Ok(entries) = fs::read_dir(path) else {
        return (files, bytes);
    };

    for entry in entries.flatten() {
        let Ok(m) = entry.metadata() else { continue };
        if m.is_dir() {
            let (f, b) = dir_stats(&entry.path());
            files += f;
            bytes += b;
        } else if m.is_file() {
            files += 1;
            bytes += m.len();
        }
    }

    (files, bytes)
}

const UNITS: &[&str] = &["B", "KiB", "MiB", "GiB", "TiB"];

fn format_size(bytes: u64) -> String {
    let mut value = bytes as f64;
    let mut unit = 0;

    while value >= 1024.0 && unit < UNITS.len() - 1 {
        value /= 1024.0;
        unit += 1;
    }

    if unit == 0 {
        format!("{bytes}B")
    } else {
        format!("{value:.1}{}", UNITS[unit])
    }
}
