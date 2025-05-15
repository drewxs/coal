use std::{
    fs,
    path::{Path, PathBuf},
};

use ignore::{DirEntry, Walk};

pub fn resolve_source(root: &str, path: &str) -> Result<String, String> {
    resolve_path(root, path, "coal")
}

pub fn resolve_main(root: &str) -> Result<String, String> {
    resolve_source(root, "main")
}

pub fn resolve_bin(root: &str, path: &str) -> Result<String, String> {
    let stem = path.trim_end_matches(".coal.bin").trim_end_matches(".coal");
    resolve_path(root, stem, "coal.bin")
}

pub fn program_files(path: &str) -> impl Iterator<Item = DirEntry> {
    Walk::new(path).flatten().filter(|e| {
        e.metadata().ok().is_some_and(|m| m.is_file())
            && e.path().extension().is_some_and(|ext| ext == "coal")
    })
}

pub fn data() -> PathBuf {
    let data_dir = dirs::data_dir().unwrap_or_default();
    let path = resolve_program_dir(&data_dir);
    let _ = fs::create_dir_all(&path);
    path
}

pub fn cache() -> PathBuf {
    let cache_dir = dirs::cache_dir().unwrap_or_default();
    let path = resolve_program_dir(&cache_dir);
    let _ = fs::create_dir_all(&path);
    path
}

pub fn history() -> PathBuf {
    data().join("history")
}

fn resolve_path(root: &str, name: &str, ext: &str) -> Result<String, String> {
    let root = Path::new(root);
    let candidates = [format!("{name}.{ext}"), format!("src/{name}.{ext}")];

    candidates
        .iter()
        .map(|p| root.join(p))
        .find(|p| p.exists())
        .map(|p| p.to_str().unwrap().to_owned())
        .ok_or(format!("{name}.{ext} not found"))
}

fn resolve_program_dir(path: &Path) -> PathBuf {
    if path.exists() {
        path.join("coal")
    } else {
        dirs::home_dir().unwrap_or_default().join(".coal")
    }
}
