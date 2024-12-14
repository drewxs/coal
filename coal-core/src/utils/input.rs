use lazy_static::lazy_static;
use regex::Regex;

lazy_static! {
    static ref RE_SPACES: Regex = Regex::new(r"[ \t]{2,}").unwrap();
    static ref RE_NEWLINES: Regex = Regex::new(r"\n{3,}").unwrap();
    static ref RE_NEWLINE_SPACES: Regex = Regex::new(r"\n\ +").unwrap();
    static ref RE_STRINGS: Regex = Regex::new(r#""[^"]*""#).unwrap();
    static ref RE_NEWLINES_AFTER_BRACE: Regex = Regex::new(r"\{\n{2,}").unwrap();
    static ref RE_NEWLINES_BEFORE_BRACE: Regex = Regex::new(r"\n{2,}\}").unwrap();
}

pub fn clean_input(input: &str) -> String {
    let mut strings: Vec<String> = Vec::new();
    let mut template = input.to_string();

    // Swap strings for placeholders to avoid formatting them
    for (i, captures) in RE_STRINGS.captures_iter(input).enumerate() {
        let matched = captures.get(0).unwrap().as_str();
        strings.push(matched.to_string());
        template = template.replace(matched, &format!("<<STR{}>>", i));
    }

    // Clean whitespace
    let mut output = clean(&template);

    // Restore strings
    for (i, original_str) in strings.iter().enumerate() {
        output = output.replace(&format!("<<STR{}>>", i), original_str);
    }

    output
}

fn clean(input: &str) -> String {
    let input = RE_SPACES.replace_all(input.trim(), " ").to_string();
    let mut input = RE_NEWLINES.replace_all(&input, "\n\n").to_string();
    input = RE_NEWLINE_SPACES
        .replace_all(&input, "\n")
        .replace(" ;", ";")
        .replace("\t", "")
        .to_string();
    input = RE_NEWLINES_AFTER_BRACE.replace_all(&input, "{").to_string();
    input = RE_NEWLINES_BEFORE_BRACE
        .replace_all(&input, "}")
        .to_string();

    input
}
