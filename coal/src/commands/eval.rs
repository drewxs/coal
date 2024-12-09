use coal_core::{Evaluator, Object};

pub fn eval(input: &str) {
    eval_with(&mut Evaluator::default(), input);
}

pub fn eval_with(evaluator: &mut Evaluator, input: &str) {
    if let Some(obj) = evaluator.eval(input) {
        match obj {
            Object::Error {
                message,
                span: ((l1, c1), (l2, c2)),
            } => {
                if input.lines().count() > 1 {
                    let line = input.lines().nth(l1 - 1).unwrap();
                    println!("\n{line}");
                    println!(
                        "\x1b[31m{}\x1b[0m",
                        " ".repeat(c1 - 1) + &"^".repeat(c2 - c1),
                    );
                    println!("{l1}:{c1}-{l2}:{c2} {message}");
                } else {
                    println!(
                        "\x1b[31m{}\x1b[0m\n",
                        " ".repeat(c1 + 2) + &"^".repeat(c2 - c1),
                    );
                    println!("{message}");
                }
            }
            _ => println!("{obj}"),
        }
    }
}
