use std::{
    fmt::Write,
    fs,
    io::{self, IsTerminal, Read},
    time::Instant,
};

use coal_core::{trim_input, Parser};

use crate::path::program_files;

pub fn fmt(input: &str) -> String {
    let mut out = String::new();
    let mut parser = Parser::from(input);
    let stmts = parser.parse();

    if !parser.errors.is_empty() {
        let _ = writeln!(&mut out, "{}", trim_input(input));
        return out;
    }

    for stmt in stmts {
        let _ = write!(&mut out, "{}", stmt);
    }
    out
}

pub fn fmt_timed(input: &str) -> (String, u128) {
    let start = Instant::now();
    let out = fmt(input);
    let elapsed = start.elapsed().as_millis();

    (out, elapsed)
}

pub fn fmt_file(path: &str) -> Result<u128, String> {
    let input = fs::read_to_string(path).unwrap_or(String::from("failed to read file"));
    let (out, elapsed) = fmt_timed(&input);

    match fs::write(path, out) {
        Ok(_) => Ok(elapsed),
        Err(_) => Err(String::from("failed to write file")),
    }
}

pub fn fmt_file_dry_run(path: &str) -> Result<String, String> {
    let input = fs::read_to_string(path).unwrap_or(String::from("failed to read file"));
    let out = fmt(&input);

    Ok(out)
}

pub fn fmt_path(path: &str, dry_run: bool) -> Result<String, String> {
    let mut out = String::new();

    for entry in program_files(path) {
        let path = entry.path().to_str().expect("invalid utf-8");

        if dry_run {
            let res = fmt_file_dry_run(path)?;
            out.push_str(&res);
        } else {
            let ms = fmt_file(path)?;
            out.push_str(&format!("{path} {ms}ms\n"));
        }
    }

    Ok(out)
}

pub fn fmt_stdin() -> Result<String, String> {
    let mut input = io::stdin();
    if input.is_terminal() {
        return Err(String::from("No input from stdin"));
    }

    let mut buf = String::new();
    input
        .read_to_string(&mut buf)
        .map_err(|_| String::from("Failed to read from stdin"))?;
    let out = fmt(&buf);

    Ok(out)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_fmt_call_expr() {
        let input = r#"   hello  ( "hello, world!"   )  ; "#;
        let expected = "hello(\"hello, world!\");\n";

        assert_eq!(expected, fmt(input));
    }

    #[test]
    fn test_fmt_nested_ifs() {
        let input = "
          let       x =        5   ;
       let y   = 10  ;

               if x  > y {
        if x > 1 {        let z = 1.;
                return z;  } else {
            return 1;  }   } elif x < y {
                  return y; } else {
                            return   0;
                         }
            ";
        let expected = "let x: i32 = 5;
let y: i32 = 10;

if x > y {
    if x > 1 {
        let z: f64 = 1.0;
        return z;
    } else {
        return 1;
    }
} elif x < y {
    return y;
} else {
    return 0;
}
";

        assert_eq!(expected, fmt(input));
    }
}
