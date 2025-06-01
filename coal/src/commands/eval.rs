use terminal_size::{Width, terminal_size};

use coal_core::{Evaluator, Object, clean_input};

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
            let term_w = terminal_size().map(|(w, _)| w).unwrap_or(Width(80)).0 as usize;

            if input.lines().count() > 1 {
                let line = input.lines().nth(l1 - 1).unwrap();
                println!("\x1b[31m{}\x1b[0m", "-".repeat(term_w));
                println!("{}", clean_input(line));
                println!(
                    "\x1b[31m{}\x1b[0m",
                    " ".repeat(c1 - 1) + &"^".repeat(c2.saturating_sub(c1) + 1),
                );
                println!("{e}");
            } else {
                println!("\x1b[31m{}\x1b[0m", "-".repeat(term_w));
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
