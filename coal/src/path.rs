use std::{
    fs,
    path::{Path, PathBuf},
};

pub fn main_program(path: Option<String>) -> Result<String, String> {
    path.or_else(|| {
        ["main.coal", "src/main.coal"]
            .iter()
            .find(|&p| Path::new(p).exists())
            .map(|&p| p.to_string())
    })
    .ok_or(String::from("no 'main.coal' or 'src/main.coal' found"))
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

fn resolve_program_dir(path: &Path) -> PathBuf {
    if path.exists() {
        path.join("coal")
    } else {
        dirs::home_dir().unwrap_or_default().join(".coal")
    }
}
