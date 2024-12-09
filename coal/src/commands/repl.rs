use std::{borrow::Cow, cell::Cell};

use rustyline::{
    completion::{Completer, Pair},
    error::ReadlineError,
    highlight::{CmdKind, Highlighter},
    hint::{Hint, Hinter},
    history::{FileHistory, SearchDirection},
    validate::MatchingBracketValidator,
    Cmd, Completer, CompletionType, ConditionalEventHandler, Config, Context, EditMode, Editor,
    Event, EventContext, EventHandler, Helper, Highlighter, Hinter, KeyCode, KeyEvent, Modifiers,
    RepeatCount, Result, Validator,
};

use coal_core::Evaluator;

const VERSION: &str = env!("CARGO_PKG_VERSION");
const KEYWORDS_BUILTINS: &[&str] = &[
    "let", "fn", "if", "else", "elif", "then", "return", "true", "false", "print",
];

pub fn repl() {
    println!("Coal {VERSION}");

    let mut rl = editor();
    let mut evaluator = Evaluator::default();

    loop {
        match rl.readline(">> ") {
            Ok(line) => match line.trim() {
                "exit" | "quit" => break,
                "clear" => println!("\x1B[2J\x1B[1;1H"),
                input => {
                    evaluator.print_eval(input);
                    let _ = rl.add_history_entry(input);
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
struct ReplHinter;

impl ReplHinter {
    pub fn new() -> Self {
        Self
    }
}

impl Hinter for ReplHinter {
    type Hint = ReplHint;

    fn hint(&self, line: &str, pos: usize, ctx: &Context<'_>) -> Option<ReplHint> {
        if line.is_empty() || pos < line.len() {
            return None;
        }

        if let Ok(Some(s)) = ctx.history().search(line, pos, SearchDirection::Forward) {
            Some(ReplHint {
                display: s.entry[pos..].to_string(),
                complete_up_to: s.entry.len() - pos,
            })
        } else {
            None
        }
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

struct ReplCompleter;

impl ReplCompleter {
    pub fn new() -> Self {
        Self
    }
}

impl Completer for ReplCompleter {
    type Candidate = Pair;

    fn complete(&self, line: &str, pos: usize, _ctx: &Context<'_>) -> Result<(usize, Vec<Pair>)> {
        let mut matches = vec![];

        for &kw in KEYWORDS_BUILTINS {
            if kw.starts_with(line) {
                matches.push(Pair {
                    display: kw.to_string(),
                    replacement: kw[pos..].to_string(),
                });
            }
        }

        Ok((pos, matches))
    }
}

struct MatchingBracketHighlighter {
    bracket: Cell<Option<(u8, usize)>>,
}

impl MatchingBracketHighlighter {
    pub fn new() -> Self {
        Self {
            bracket: Cell::new(None),
        }
    }
}

impl Highlighter for MatchingBracketHighlighter {
    fn highlight<'l>(&self, line: &'l str, _pos: usize) -> Cow<'l, str> {
        if line.len() <= 1 {
            return Cow::Borrowed(line);
        }

        if let Some((bracket, pos)) = self.bracket.get() {
            if let Some((matching, idx)) = find_matching_bracket(line, pos, bracket) {
                let mut copy = line.to_owned();
                if idx + 1 < pos {
                    copy.replace_range(
                        idx..=pos,
                        &format!(
                            "\x1b[1;34m{}\x1b[0m{}\x1b[1;34m{}\x1b[0m",
                            matching as char,
                            &line[idx + 1..pos],
                            line.chars().nth(pos).unwrap()
                        ),
                    );
                }
                return Cow::Owned(copy);
            }
        }

        Cow::Borrowed(line)
    }

    fn highlight_char(&self, line: &str, pos: usize, kind: CmdKind) -> bool {
        if kind == CmdKind::ForcedRefresh {
            self.bracket.set(None);
            return false;
        }

        self.bracket.set(check_bracket(line, pos));
        self.bracket.get().is_some()
    }
}

fn find_matching_bracket(line: &str, pos: usize, bracket: u8) -> Option<(u8, usize)> {
    let matching = matching_bracket(bracket);
    let mut idx;
    let mut unmatched = 1;

    if is_open_bracket(bracket) {
        idx = pos + 1;
        let bytes = &line.as_bytes()[idx..];
        for b in bytes {
            if *b == matching {
                unmatched -= 1;
                if unmatched == 0 {
                    debug_assert_eq!(matching, line.as_bytes()[idx]);
                    return Some((matching, idx));
                }
            } else if *b == bracket {
                unmatched += 1;
            }
            idx += 1;
        }
    } else {
        idx = pos;
        let bytes = &line.as_bytes()[..idx];
        for b in bytes.iter().rev() {
            if *b == matching {
                unmatched -= 1;
                if unmatched == 0 {
                    debug_assert_eq!(matching, line.as_bytes()[idx - 1]);
                    return Some((matching, idx - 1));
                }
            } else if *b == bracket {
                unmatched += 1;
            }
            idx -= 1;
        }
    }

    None
}

const fn check_bracket(line: &str, pos: usize) -> Option<(u8, usize)> {
    if line.is_empty() {
        return None;
    }

    let mut pos = pos;
    if pos >= line.len() {
        pos = line.len() - 1;
        let b = line.as_bytes()[pos];
        if is_close_bracket(b) {
            Some((b, pos))
        } else {
            None
        }
    } else {
        let mut under_cursor = true;
        loop {
            let b = line.as_bytes()[pos];
            if is_close_bracket(b) {
                return if pos == 0 { None } else { Some((b, pos)) };
            } else if is_open_bracket(b) {
                return if pos + 1 == line.len() {
                    None
                } else {
                    Some((b, pos))
                };
            } else if under_cursor && pos > 0 {
                under_cursor = false;
                pos -= 1;
            } else {
                return None;
            }
        }
    }
}

const fn matching_bracket(bracket: u8) -> u8 {
    match bracket {
        b'{' => b'}',
        b'}' => b'{',
        b'[' => b']',
        b']' => b'[',
        b'(' => b')',
        b')' => b'(',
        b => b,
    }
}

const fn is_open_bracket(bracket: u8) -> bool {
    matches!(bracket, b'{' | b'[' | b'(')
}

const fn is_close_bracket(bracket: u8) -> bool {
    matches!(bracket, b'}' | b']' | b')')
}

#[derive(Helper, Completer, Hinter, Validator)]
struct ReplHelper {
    #[rustyline(Completer)]
    completer: ReplCompleter,
    highlighter: MatchingBracketHighlighter,
    #[rustyline(Validator)]
    validator: MatchingBracketValidator,
    #[rustyline(Hinter)]
    hinter: ReplHinter,
}

impl ReplHelper {
    pub fn new() -> Self {
        Self {
            completer: ReplCompleter::new(),
            highlighter: MatchingBracketHighlighter::new(),
            hinter: ReplHinter::new(),
            validator: MatchingBracketValidator::new(),
        }
    }
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

struct EnterHandler;

impl ConditionalEventHandler for EnterHandler {
    fn handle(
        &self,
        _evt: &Event,
        _n: RepeatCount,
        _positive: bool,
        ctx: &EventContext,
    ) -> Option<Cmd> {
        if ctx.line().len() > ctx.pos() {
            return Some(Cmd::Insert(1, String::from("\n")));
        }
        None
    }
}

fn editor() -> Editor<ReplHelper, FileHistory> {
    let mut rl = Editor::with_config(editor_config()).unwrap();

    rl.set_helper(Some(ReplHelper::new()));

    rl.bind_sequence(
        Event::KeySeq(vec![KeyEvent(KeyCode::Tab, Modifiers::NONE)]),
        EventHandler::Simple(Cmd::Insert(1, String::from("\t"))),
    );
    rl.bind_sequence(
        Event::KeySeq(vec![KeyEvent(KeyCode::Enter, Modifiers::NONE)]),
        EventHandler::Conditional(Box::new(EnterHandler)),
    );

    let _ = rl.load_history(&crate::path::history());

    rl
}

fn editor_config() -> Config {
    Config::builder()
        .completion_type(CompletionType::List)
        .edit_mode(EditMode::Emacs)
        .build()
}
