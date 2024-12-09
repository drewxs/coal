use lazy_static::lazy_static;
use regex::Regex;

lazy_static! {
    static ref RE_SPACES: Regex = Regex::new(r"[ \t]{2,}").unwrap();
    static ref RE_NEWLINES: Regex = Regex::new(r"\n{3,}").unwrap();
    static ref RE_NEWLINE_SPACES: Regex = Regex::new(r"\n\ +").unwrap();
}

pub fn clean_input(input: &str) -> String {
    let input = RE_SPACES.replace_all(input.trim(), " ").to_string();
    let mut input = RE_NEWLINES.replace_all(&input, "\n\n").to_string();
    input = RE_NEWLINE_SPACES
        .replace_all(&input, "\n")
        .replace(" ;", ";")
        .replace("\t", "")
        .to_string();

    input
}
