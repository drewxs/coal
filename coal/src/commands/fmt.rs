use std::{fmt::Write, fs, time::Instant};

use coal_core::{Parser, trim_input};

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
        let _ = write!(&mut out, "{stmt}");
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
    let input = fs::read_to_string(path).map_err(|_| String::from("failed to read file"))?;
    let (out, elapsed) = fmt_timed(&input);

    match fs::write(path, out) {
        Ok(_) => Ok(elapsed),
        Err(_) => Err(String::from("failed to write file")),
    }
}

pub fn fmt_file_dry_run(path: &str) -> Result<String, String> {
    let input = fs::read_to_string(path).map_err(|_| String::from("failed to read file"))?;
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
    fn test_fmt_let_statements() {
        let input = "
          let       x =        5   ;
       let y   = 10  ;

        let    x :   u32  =         2
            ";

        let expected = "let x: i32 = 5;
let y: i32 = 10;

let x: u32 = 2;
";

        assert_eq!(expected, fmt(input));
    }

    #[test]
    fn test_fmt_nested_ifs() {
        let input = "
               if x  > y {
        if x > 1 {        let z = 1.;
                return z;  } else {
            return 1;  }   } elif x < y {
                  return y; } else {
                            return   0;
                         }
            ";

        let expected = "if x > y {
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

    #[test]
    fn test_fmt_functions() {
        let input = "
  fn  adder (x :  u32 ) ->  Fn ( u32  ) ->    u32  {
      fn add(n: u32) -> u32 {
let i = 0
  while  1 < n  { i   +=  1  }
return
x + n;
        }
 return  add; }

  let  add_two :   Fn ( u32  )  ->    u32  =   adder( x )
add_two( 2 )
            ";

        let expected = "fn adder(x: u32) -> Fn(u32) -> u32 {
    fn add(n: u32) -> u32 {
        let i: i32 = 0;
        while 1 < n {
            i += 1;
        }
        return x + n;
    }
    return add;
}

let add_two: Fn(u32) -> u32 = adder(x);
add_two(2);
";

        assert_eq!(expected, fmt(input));
    }

    #[test]
    fn test_fmt_lists() {
        let input = "
let l1 = [0 ;   100];
let l2     = [
    [1, 2],    [3, 4],
[5, 6],
];

println([
[1, 2],    [3, 4],[5, 6]  ]);
";

        let expected = "let l1: list[i32] = [0; 100];
let l2: list[list[i32]] = [[1, 2], [3, 4], [5, 6]];

println([[1, 2], [3, 4], [5, 6]]);
";

        assert_eq!(expected, fmt(input));
    }

    #[test]
    fn test_fmt_maps() {
        let input = "
let m: map[i32, i32] = {};
let m2 = {1: 2};
let m3: map[i32, i32] = { 1: 2, 3: 4, };

println ({1:2,3:4,5:6});
";

        let expected = "let m: map[i32, i32] = {};
let m2: map[i32, i32] = {1: 2};
let m3: map[i32, i32] = {1: 2, 3: 4};

println({1: 2, 3: 4, 5: 6});
";

        assert_eq!(expected, fmt(input));
    }

    //     #[test]
    //     fn test_fmt_structs() {
    //         let input = r#"struct Foo {    bar: i32;   baz  : str = "bazz"; blonk: u32 = 2;
    //                                 fn bleetz(blontz: i32) -> u64 { let bluntz: u64 = blontz; return bluntz;}}"#;

    //         let expected = r#"struct Foo {
    //     bar: i32;
    //     baz: str = "bazz";
    //     blonk: u32 = 2;

    //     fn bleetz(blontz: i32) -> u64 {
    //         let bluntz: u64 = blontz;
    //         return bluntz;
    //     }
    // }
    // "#;

    //         assert_eq!(expected, fmt(input));
    //     }
}
