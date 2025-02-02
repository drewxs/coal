pub const INDENT: &str = "    ";

pub fn indent(level: usize) -> String {
    INDENT.repeat(level)
}
