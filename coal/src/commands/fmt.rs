use std::{
    fs,
    io::{self, Read},
    time::Instant,
};

use coal_core::Program;

use crate::path::program_files;

pub fn fmt(input: &str) -> String {
    Program::from(input).to_string()
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
    let mut input = String::new();

    io::stdin()
        .read_to_string(&mut input)
        .map_err(|_| String::from("Failed to read from stdin"))?;

    let formatted = fmt(&input);

    Ok(formatted)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_fmt_call_expr() {
        let input = r#"   hello  ( "hello, world!"   )  ; "#;
        let expected = r#"hello("hello, world!")"#;

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
        let expected = "let x: int = 5;
let y: int = 10;

if x > y {
    if x > 1 {
        let z: float = 1.0;
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
