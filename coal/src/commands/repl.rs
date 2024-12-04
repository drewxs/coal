use std::{borrow::Cow, collections::HashSet};

use rustyline::{
    completion::FilenameCompleter,
    error::ReadlineError,
    highlight::{CmdKind, Highlighter, MatchingBracketHighlighter},
    hint::{Hint, Hinter},
    history::FileHistory,
    validate::MatchingBracketValidator,
    Completer, CompletionType, Config, Context, EditMode, Editor, Helper, Highlighter, Hinter,
    Validator,
};

use coal_core::Evaluator;

const VERSION: &str = env!("CARGO_PKG_VERSION");

pub fn repl() {
    println!("Coal {VERSION}");

    let mut rl = editor();
    let mut evaluator = Evaluator::default();

    loop {
        match rl.readline(">> ") {
            Ok(line) => match line.trim() {
                "exit" | "quit" => break,
                "clear" => println!("\x1B[2J\x1B[1;1H"),
                _ => {
                    evaluator.print_eval(&line);
                    let _ = rl.add_history_entry(line);
                }
            },
            Err(ReadlineError::Interrupted) => {
                println!("CTRL-C");
                break;
            }
            Err(ReadlineError::Eof) => {
                println!("CTRL-D");
                break;
            }
            Err(err) => {
                println!("Error: {err:?}");
                break;
            }
        }
    }

    let _ = rl.save_history(&crate::path::history());
}

#[derive(Completer, Helper, Validator, Highlighter)]
struct ReplHinter {
    hints: HashSet<ReplHint>,
}

impl Default for ReplHinter {
    fn default() -> Self {
        let mut hints = HashSet::new();
        hints.insert(ReplHint::new("print", "print"));
        Self { hints }
    }
}

impl Hinter for ReplHinter {
    type Hint = ReplHint;

    fn hint(&self, line: &str, pos: usize, _ctx: &Context<'_>) -> Option<ReplHint> {
        if line.is_empty() || pos < line.len() {
            return None;
        }

        self.hints
            .iter()
            .filter_map(|hint| {
                if hint.display.starts_with(line) {
                    Some(hint.suffix(pos))
                } else {
                    None
                }
            })
            .next()
    }
}

#[derive(Hash, Debug, PartialEq, Eq)]
struct ReplHint {
    display: String,
    complete_up_to: usize,
}

impl Hint for ReplHint {
    fn display(&self) -> &str {
        &self.display
    }

    fn completion(&self) -> Option<&str> {
        if self.complete_up_to > 0 {
            Some(&self.display[..self.complete_up_to])
        } else {
            None
        }
    }
}

impl ReplHint {
    fn new(text: &str, complete_up_to: &str) -> Self {
        assert!(text.starts_with(complete_up_to));
        Self {
            display: text.into(),
            complete_up_to: complete_up_to.len(),
        }
    }

    fn suffix(&self, strip_chars: usize) -> Self {
        Self {
            display: self.display[strip_chars..].to_owned(),
            complete_up_to: self.complete_up_to.saturating_sub(strip_chars),
        }
    }
}

#[derive(Helper, Completer, Hinter, Validator)]
struct ReplHelper {
    #[rustyline(Completer)]
    completer: FilenameCompleter,
    highlighter: MatchingBracketHighlighter,
    #[rustyline(Validator)]
    validator: MatchingBracketValidator,
    #[rustyline(Hinter)]
    hinter: ReplHinter,
}

impl Highlighter for ReplHelper {
    fn highlight_prompt<'b, 's: 'b, 'p: 'b>(
        &'s self,
        prompt: &'p str,
        _default: bool,
    ) -> Cow<'b, str> {
        Cow::Borrowed(prompt)
    }

    fn highlight_hint<'h>(&self, hint: &'h str) -> Cow<'h, str> {
        Cow::Owned("\x1b[1m".to_owned() + hint + "\x1b[m")
    }

    fn highlight<'l>(&self, line: &'l str, pos: usize) -> Cow<'l, str> {
        self.highlighter.highlight(line, pos)
    }

    fn highlight_char(&self, line: &str, pos: usize, kind: CmdKind) -> bool {
        self.highlighter.highlight_char(line, pos, kind)
    }
}

fn editor() -> Editor<ReplHelper, FileHistory> {
    let config = Config::builder()
        .history_ignore_space(true)
        .completion_type(CompletionType::List)
        .edit_mode(EditMode::Emacs)
        .build();

    let helper = ReplHelper {
        completer: FilenameCompleter::new(),
        highlighter: MatchingBracketHighlighter::new(),
        hinter: ReplHinter::default(),
        validator: MatchingBracketValidator::new(),
    };

    let mut rl = Editor::with_config(config).unwrap();
    rl.set_helper(Some(helper));
    let _ = rl.load_history(&crate::path::history());

    rl
}
