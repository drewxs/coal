use std::{
    fs,
    path::{Path, PathBuf},
};

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

fn resolve_program_dir(path: &Path) -> PathBuf {
    if path.exists() {
        path.join("coal")
    } else {
        dirs::home_dir().unwrap_or_default().join(".coal")
    }
}
