use coal_core::{clean_input, Evaluator, Object};

pub fn eval(input: &str) {
    eval_with(&mut Evaluator::default(), input);
}

pub fn eval_with(evaluator: &mut Evaluator, input: &str) {
    if let Some(obj) = evaluator.eval(input) {
        match obj {
            Object::Error(e) => {
                let ((l1, c1), (l2, c2)) = e.span;

                if input.lines().count() > 1 {
                    let line = input.lines().nth(l1 - 1).unwrap();
                    println!("\x1b[31m{}\x1b[0m", "-".repeat(75));
                    println!("{}", clean_input(line));
                    println!(
                        "\x1b[31m{}\x1b[0m",
                        " ".repeat(c1 - 1) + &"^".repeat(c2 - c1 + 1),
                    );
                    println!("{l1}:{c1}-{l2}:{c2} {}\n", e.kind);
                } else {
                    println!("\x1b[31m{}\x1b[0m", "-".repeat(75));
                    println!("{}", clean_input(input));
                    println!(
                        "\x1b[31m{}\x1b[0m",
                        " ".repeat(c1 - 1) + &"^".repeat(c2 - c1 + 1),
                    );
                    println!("{}\n", e.kind);
                }
            }
            _ => println!("{obj}"),
        }
    }
}
