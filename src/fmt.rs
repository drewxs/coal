use std::{fs, time::Instant};

use ignore::Walk;

use crate::Program;

pub fn fmt(input: &str) -> (String, u128) {
    let start = Instant::now();
    let out = Program::from(input).to_string();
    let elapsed = start.elapsed().as_millis();
    (out, elapsed)
}

pub fn fmt_file(path: &str) -> Result<u128, String> {
    let input = fs::read_to_string(path).unwrap_or(String::from("failed to read file"));
    let (out, elapsed) = fmt(&input);

    match fs::write(path, out) {
        Ok(_) => Ok(elapsed),
        Err(_) => Err(String::from("failed to write file")),
    }
}

pub fn fmt_file_dry_run(path: &str) -> Result<String, String> {
    let input = fs::read_to_string(path).unwrap_or(String::from("failed to read file"));
    let (out, _) = fmt(&input);

    Ok(out)
}

pub fn fmt_path(path: &str, dry_run: bool) -> Result<String, String> {
    let mut out = String::new();

    let walker = Walk::new(path).flatten().filter(|e| {
        e.metadata().ok().map_or(false, |m| m.is_file())
            && e.path().extension().map_or(false, |ext| ext == "coal")
    });

    for entry in walker {
        let path = entry.path();
        let file_path = path.to_str().unwrap();

        if dry_run {
            let res = fmt_file_dry_run(file_path)?;
            out.push_str(&res);
        } else {
            let ms = fmt_file(file_path)?;
            out.push_str(&format!("{file_path} {ms}ms\n"));
        }
    }

    Ok(out)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_fmt_call_expr() {
        let input = r#"   hello  ( "hello, world!"   )  ; "#;
        let expected = r#"hello("hello, world!")"#;
        let (actual, _) = fmt(input);

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_fmt_nested_ifs() {
        let input = r#"
      let       x =        5   ;
   let y   = 10  ;

           if x  > y {
    if x > 1 {        let z = 1.;
            return z;  } else {
        return 1;  }   } elif x < y {
              return y; } else {
                        return   0;
                     }
        "#;
        let expected = r#"let x: int = 5;
let y: int = 10;
if (x > y) {
    if (x > 1) {
        let z: float = 1.0;
        return z;
    } else {
        return 1;
    }
} elif (x < y) {
    return y;
} else {
    return 0;
}
"#;
        let (actual, _) = fmt(input);

        assert_eq!(expected, actual);
    }
}
