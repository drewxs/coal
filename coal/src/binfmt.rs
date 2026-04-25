use std::{fs, path::Path};

pub const BIN_MAGIC: [u8; 4] = [0, 0, 0, 0];
pub const BIN_HEADER_LEN: usize = 8;
pub const META_LEN: usize = 40;

/// Build a `.bin` header: 4-byte magic + 4-byte version (major:u8, minor:u8, patch:u16 LE).
pub fn bin_header() -> [u8; BIN_HEADER_LEN] {
    let mut parts = env!("CARGO_PKG_VERSION").split('.');
    let major: u8 = parts.next().and_then(|s| s.parse().ok()).unwrap_or(0);
    let minor: u8 = parts.next().and_then(|s| s.parse().ok()).unwrap_or(0);
    let patch: u16 = parts
        .next()
        .map(|s| s.split(['-', '+']).next().unwrap_or(""))
        .and_then(|s| s.parse().ok())
        .unwrap_or(0);

    let mut h = [0u8; BIN_HEADER_LEN];
    h[0..4].copy_from_slice(&BIN_MAGIC);
    h[4] = major;
    h[5] = minor;
    h[6..8].copy_from_slice(&patch.to_le_bytes());
    h
}

/// Write a `.meta` file: 8-byte source mtime (u64 LE) + 32-byte blake3 hash.
pub fn write_meta(path: &Path, mtime: u64, hash: &[u8; 32]) -> std::io::Result<()> {
    let mut buf = [0u8; META_LEN];
    buf[0..8].copy_from_slice(&mtime.to_le_bytes());
    buf[8..40].copy_from_slice(hash);
    fs::write(path, buf)
}

/// Parse a `.meta` file into (mtime, hash). Returns None on length mismatch.
pub fn read_meta(path: &Path) -> Option<(u64, [u8; 32])> {
    let bytes = fs::read(path).ok()?;
    if bytes.len() != META_LEN {
        return None;
    }
    let mtime = u64::from_le_bytes(bytes[0..8].try_into().ok()?);
    let hash: [u8; 32] = bytes[8..40].try_into().ok()?;
    Some((mtime, hash))
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::time::{SystemTime, UNIX_EPOCH};

    fn tmp_path(tag: &str) -> std::path::PathBuf {
        let nanos = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_nanos();
        std::env::temp_dir().join(format!(
            "coal-binfmt-{}-{tag}-{nanos}.tmp",
            std::process::id()
        ))
    }

    #[test]
    fn test_bin_header_layout() {
        let h = bin_header();
        assert_eq!(h.len(), BIN_HEADER_LEN);
        assert_eq!(&h[0..4], &BIN_MAGIC);

        let mut parts = env!("CARGO_PKG_VERSION").split('.');
        let major: u8 = parts.next().unwrap().parse().unwrap();
        let minor: u8 = parts.next().unwrap().parse().unwrap();
        let patch: u16 = parts
            .next()
            .unwrap()
            .split(['-', '+'])
            .next()
            .unwrap()
            .parse()
            .unwrap();
        assert_eq!(h[4], major);
        assert_eq!(h[5], minor);
        assert_eq!(u16::from_le_bytes([h[6], h[7]]), patch);
    }

    #[test]
    fn test_meta_roundtrip() {
        let path = tmp_path("roundtrip");
        let hash = [7u8; 32];
        write_meta(&path, 42_000_000u64, &hash).unwrap();
        let (mtime, got) = read_meta(&path).unwrap();
        assert_eq!(mtime, 42_000_000);
        assert_eq!(got, hash);
        let _ = fs::remove_file(&path);
    }

    #[test]
    fn test_meta_wrong_length_returns_none() {
        let path = tmp_path("bad");
        fs::write(&path, [0u8; 10]).unwrap();
        assert!(read_meta(&path).is_none());
        let _ = fs::remove_file(&path);
    }

    #[test]
    fn test_meta_missing_file_returns_none() {
        let path = tmp_path("missing");
        assert!(read_meta(&path).is_none());
    }
}
