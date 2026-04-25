use std::{
    env,
    fs::{self, File},
    io::{BufWriter, Write},
    path::{Path, PathBuf},
    time::{Instant, UNIX_EPOCH},
};

use coal_compiler::Compiler;
use rkyv::{rancor, to_bytes};

use crate::{bin_header, path::resolve_main, read_file_to_str, read_meta, status, write_meta};

/// Compile the project at the given working directory
pub fn compile(working_dir: &str, quiet: bool) {
    let Some(ctx) = Ctx::new(working_dir, quiet) else {
        return;
    };

    if ctx.bin_path.exists()
        && let Some((cached_mtime, cached_hash)) = read_meta(&ctx.meta_path)
    {
        if cached_mtime == ctx.src_mtime {
            ctx.print("Finished");
            return;
        }

        let input = read_file_to_str(ctx.source.to_str().unwrap());
        let hash = *blake3::hash(input.as_bytes()).as_bytes();
        if hash == cached_hash {
            let _ = write_meta(&ctx.meta_path, ctx.src_mtime, &hash);
            ctx.print("Finished");
            return;
        }
        ctx.build(&input, &hash);
        return;
    }

    let input = read_file_to_str(ctx.source.to_str().unwrap());
    let hash = *blake3::hash(input.as_bytes()).as_bytes();
    ctx.build(&input, &hash);
}

struct Ctx {
    name: String,
    project_root: PathBuf,
    target_dir: PathBuf,
    bin_path: PathBuf,
    meta_path: PathBuf,
    source: PathBuf,
    src_mtime: u64,
    quiet: bool,
}

impl Ctx {
    fn new(working_dir: &str, quiet: bool) -> Option<Self> {
        let project_root = env::current_dir().unwrap_or_default().join(working_dir);
        let project_root = project_root.canonicalize().unwrap_or(project_root);
        let name = project_root
            .file_name()
            .and_then(|n| n.to_str())
            .unwrap_or("project")
            .to_string();

        let mut root = Path::new(working_dir);
        if root.join("src").exists() {
            root = root.parent().unwrap();
        }

        let source = match resolve_main(working_dir) {
            Ok(p) => PathBuf::from(p),
            Err(_) => {
                eprintln!("main.coal not found");
                return None;
            }
        };
        let filename = source.file_name()?.to_str()?;
        let target_dir = root.join("target");
        let bin_path = target_dir.join(format!("{filename}.bin"));
        let meta_path = target_dir.join(format!("{filename}.meta"));

        let src_mtime = fs::metadata(&source)
            .and_then(|m| m.modified())
            .ok()
            .and_then(|t| t.duration_since(UNIX_EPOCH).ok())
            .map(|d| d.as_nanos() as u64)
            .unwrap_or(0);

        Some(Self {
            name,
            project_root,
            target_dir,
            bin_path,
            meta_path,
            source,
            src_mtime,
            quiet,
        })
    }

    fn print(&self, label: &str) {
        if !self.quiet {
            println!("{} {} ({})", status(label), self.name, self.project_root.display());
        }
    }

    fn build(&self, input: &str, hash: &[u8; 32]) {
        self.print("Compiling");
        let start = Instant::now();

        let mut c = Compiler::new();
        c.compile(input).unwrap();
        let bytecode = c.bytecode();

        fs::create_dir_all(&self.target_dir).unwrap();
        let file = File::create(&self.bin_path).unwrap();
        let mut writer = BufWriter::new(file);

        let Ok(bytes) = to_bytes::<rancor::Error>(&bytecode) else {
            eprintln!("Failed to serialize bytecode");
            return;
        };
        if let Err(e) = writer.write_all(&bin_header()) {
            eprintln!("Failed to write header: {e}");
            return;
        }
        if let Err(e) = writer.write_all(&bytes.into_vec()) {
            eprintln!("Failed to write bytecode: {e}");
            return;
        }

        if let Err(e) = write_meta(&self.meta_path, self.src_mtime, hash) {
            eprintln!("Failed to write meta: {e}");
            return;
        }

        let elapsed = start.elapsed().as_millis();
        if !self.quiet {
            println!(
                "{} `{}` in {elapsed}ms",
                status("Finished"),
                self.bin_path.display()
            );
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::{
        thread,
        time::{Duration, SystemTime},
    };

    use crate::{BIN_HEADER_LEN, BIN_MAGIC, META_LEN};

    fn setup(tag: &str, source: &str) -> PathBuf {
        let nanos = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_nanos();
        let dir = std::env::temp_dir().join(format!(
            "coal-compile-{}-{tag}-{nanos}",
            std::process::id()
        ));
        fs::create_dir_all(&dir).unwrap();
        fs::write(dir.join("main.coal"), source).unwrap();
        dir
    }

    #[test]
    fn test_fresh_build_writes_bin_and_meta() {
        let dir = setup("fresh", r#"print("hi");"#);
        compile(dir.to_str().unwrap(), true);

        let bin = fs::read(dir.join("target/main.coal.bin")).unwrap();
        assert!(bin.len() > BIN_HEADER_LEN);
        assert_eq!(&bin[0..4], &BIN_MAGIC);

        let meta = fs::read(dir.join("target/main.coal.meta")).unwrap();
        assert_eq!(meta.len(), META_LEN);
    }

    #[test]
    fn test_mtime_hit_skips_recompile() {
        let dir = setup("mtime_hit", r#"print("hi");"#);
        compile(dir.to_str().unwrap(), true);
        let bin = dir.join("target/main.coal.bin");
        let mtime1 = fs::metadata(&bin).unwrap().modified().unwrap();

        thread::sleep(Duration::from_millis(20));
        compile(dir.to_str().unwrap(), true);
        let mtime2 = fs::metadata(&bin).unwrap().modified().unwrap();

        assert_eq!(mtime1, mtime2);
    }

    #[test]
    fn test_touch_unchanged_updates_meta_keeps_bin() {
        let dir = setup("touch", r#"print("hi");"#);
        let source = dir.join("main.coal");
        compile(dir.to_str().unwrap(), true);

        let bin = dir.join("target/main.coal.bin");
        let meta = dir.join("target/main.coal.meta");
        let bin_mtime1 = fs::metadata(&bin).unwrap().modified().unwrap();
        let meta1 = fs::read(&meta).unwrap();

        thread::sleep(Duration::from_millis(20));
        let content = fs::read_to_string(&source).unwrap();
        fs::write(&source, content).unwrap();

        compile(dir.to_str().unwrap(), true);
        let bin_mtime2 = fs::metadata(&bin).unwrap().modified().unwrap();
        let meta2 = fs::read(&meta).unwrap();

        assert_eq!(bin_mtime1, bin_mtime2, "bin should not be rewritten");
        assert_ne!(&meta1[0..8], &meta2[0..8], "meta mtime should differ");
        assert_eq!(&meta1[8..40], &meta2[8..40], "hash should match");
    }

    #[test]
    fn test_content_change_triggers_recompile() {
        let dir = setup("change", r#"print("hi");"#);
        let source = dir.join("main.coal");
        compile(dir.to_str().unwrap(), true);
        let meta1 = fs::read(dir.join("target/main.coal.meta")).unwrap();

        thread::sleep(Duration::from_millis(20));
        fs::write(&source, r#"print("bye");"#).unwrap();

        compile(dir.to_str().unwrap(), true);
        let meta2 = fs::read(dir.join("target/main.coal.meta")).unwrap();

        assert_ne!(&meta1[8..40], &meta2[8..40], "hash should change");
    }
}
