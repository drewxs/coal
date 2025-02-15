use coal_core::{clean_input, Evaluator, Object};

pub fn eval(input: &str) {
    eval_with(&mut Evaluator::default(), input, true);
}

pub fn eval_with(evaluator: &mut Evaluator, input: &str, print: bool) {
    match evaluator.eval(input) {
        Ok(obj) => {
            if print && obj != Object::Void {
                println!("{obj}");
            }
        }
        Err(errs) => errs.iter().for_each(|e| {
            let ((l1, c1), (_, c2)) = e.span;

            if input.lines().count() > 1 {
                let line = input.lines().nth(l1 - 1).unwrap();
                println!("\x1b[31m{}\x1b[0m", "-".repeat(75));
                println!("{}", clean_input(line));
                println!(
                    "\x1b[31m{}\x1b[0m",
                    " ".repeat(c1 - 1) + &"^".repeat(c2.saturating_sub(c1) + 1),
                );
                println!("{e}");
            } else {
                println!("\x1b[31m{}\x1b[0m", "-".repeat(75));
                println!("{}", clean_input(input));
                println!(
                    "\x1b[31m{}\x1b[0m",
                    " ".repeat(c1 - 1) + &"^".repeat(c2.saturating_sub(c1) + 1),
                );
                println!("{}\n", e.kind);
            }
        }),
    }
}
