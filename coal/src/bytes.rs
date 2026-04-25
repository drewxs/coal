const UNITS: &[&str] = &["B", "KiB", "MiB", "GiB", "TiB"];

pub fn format_size(bytes: u64) -> String {
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
