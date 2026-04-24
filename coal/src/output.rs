use colored::{ColoredString, Colorize};

const STATUS_WIDTH: usize = 12;

/// Format label right-aligned to a 12-char column, green and bold.
pub fn status(label: &str) -> ColoredString {
    format!("{label:>STATUS_WIDTH$}").green().bold()
}
