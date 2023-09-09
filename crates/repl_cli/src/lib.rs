//! Command Line Interface (CLI) functionality for the Read-Evaluate-Print-Loop (REPL).
mod cli_gen;

use std::borrow::Cow;

use bumpalo::Bump;
use roc_load::MonomorphizedModule;
use roc_mono::ir::OptLevel;
use roc_repl_eval::gen::{Problems, ReplOutput};
use roc_repl_ui::colors::{END_COL, GREEN, PINK};
use roc_repl_ui::repl_state::{ReplAction, ReplState};
use roc_repl_ui::{is_incomplete, CONT_PROMPT, PROMPT, SHORT_INSTRUCTIONS, TIPS, WELCOME_MESSAGE};
use roc_reporting::report::DEFAULT_PALETTE;
use roc_target::TargetInfo;
use rustyline::highlight::{Highlighter, PromptInfo};
use rustyline::validate::{self, ValidationContext, ValidationResult, Validator};
use rustyline_derive::{Completer, Helper, Hinter};
use target_lexicon::Triple;

use crate::cli_gen::eval_llvm;

#[derive(Completer, Helper, Hinter, Default)]
pub struct ReplHelper {
    validator: InputValidator,
    state: ReplState,
}

pub fn main() -> i32 {
    use rustyline::error::ReadlineError;
    use rustyline::Editor;

    // To debug rustyline:
    // <UNCOMMENT> env_logger::init();
    // <RUN WITH:> RUST_LOG=rustyline=debug cargo run repl 2> debug.log
    print!("{WELCOME_MESSAGE}{SHORT_INSTRUCTIONS}");

    let mut editor = Editor::<ReplHelper>::new();
    let repl_helper = ReplHelper::default();
    editor.set_helper(Some(repl_helper));
    let target = Triple::host();
    let target_info = TargetInfo::from(&target);
    let mut arena = Bump::new();

    loop {
        match editor.readline(PROMPT) {
            Ok(line) => {
                editor.add_history_entry(line.trim());

                let dimensions = editor.dimensions();
                let repl_state = &mut editor
                    .helper_mut()
                    .expect("Editor helper was not set")
                    .state;

                arena.reset();
                match repl_state.step(&arena, &line, target_info, DEFAULT_PALETTE) {
                    // If there was no output, don't print a blank line!
                    // (This happens for something like a type annotation.)
                    ReplAction::Eval {
                        opt_mono,
                        problems,
                        opt_var_name,
                    } => {
                        let output =
                            evaluate(opt_mono, problems, opt_var_name, &target, dimensions);
                        if !output.is_empty() {
                            println!("{output}");
                        }
                    }
                    ReplAction::Exit => {
                        return 0;
                    }
                    ReplAction::Help => {
                        println!("{TIPS}");
                    }
                    ReplAction::Nothing => {}
                }
            }
            #[cfg(windows)]
            Err(ReadlineError::WindowResize) => {
                // This is fine; just ignore it.
            }
            Err(ReadlineError::Eof) => {
                // End of input; we're done!
                return 0;
            }
            Err(ReadlineError::Interrupted) => {
                eprintln!("CTRL-C");
                return 1;
            }
            Err(err) => {
                eprintln!("REPL error: {err:?}");
                return 1;
            }
        }
    }
}

pub fn evaluate<'a>(
    opt_mono: Option<MonomorphizedModule<'a>>,
    problems: Problems,
    opt_var_name: Option<String>,
    target: &Triple,
    dimensions: Option<(usize, usize)>,
) -> String {
    let opt_output = opt_mono.and_then(|mono| eval_llvm(mono, target, OptLevel::Normal));
    format_output(opt_output, problems, opt_var_name, dimensions)
}

#[derive(Default)]
struct InputValidator {}

impl Validator for InputValidator {
    fn validate(&self, ctx: &mut ValidationContext) -> rustyline::Result<ValidationResult> {
        if is_incomplete(ctx.input()) {
            Ok(ValidationResult::Incomplete)
        } else {
            Ok(ValidationResult::Valid(None))
        }
    }
}

impl Highlighter for ReplHelper {
    fn has_continuation_prompt(&self) -> bool {
        true
    }

    fn highlight_prompt<'b, 's: 'b, 'p: 'b>(
        &'s self,
        prompt: &'p str,
        info: PromptInfo<'_>,
    ) -> Cow<'b, str> {
        if info.line_no() > 0 {
            CONT_PROMPT.into()
        } else {
            prompt.into()
        }
    }
}

impl Validator for ReplHelper {
    fn validate(
        &self,
        ctx: &mut validate::ValidationContext,
    ) -> rustyline::Result<validate::ValidationResult> {
        self.validator.validate(ctx)
    }

    fn validate_while_typing(&self) -> bool {
        self.validator.validate_while_typing()
    }
}

fn format_output(
    opt_output: Option<ReplOutput>,
    problems: Problems,
    opt_var_name: Option<String>,
    dimensions: Option<(usize, usize)>,
) -> String {
    let mut buf = String::new();

    for message in problems.errors.iter().chain(problems.warnings.iter()) {
        if !buf.is_empty() {
            buf.push_str("\n\n");
        }

        buf.push('\n');
        buf.push_str(message);
        buf.push('\n');
    }

    if let Some(ReplOutput { expr, expr_type }) = opt_output {
        // If expr was empty, it was a type annotation or ability declaration;
        // don't print anything!
        //
        // Also, for now we also don't print anything if there was a compile-time error.
        // In the future, it would be great to run anyway and print useful output here!
        if !expr.is_empty() && problems.errors.is_empty() {
            const EXPR_TYPE_SEPARATOR: &str = " : "; // e.g. in "5 : Num *"

            // Print the expr and its type
            {
                buf.push('\n');
                buf.push_str(&expr);
                buf.push_str(PINK); // Color for the type separator
                buf.push_str(EXPR_TYPE_SEPARATOR);
                buf.push_str(END_COL);
                buf.push_str(&expr_type);
            }

            // Print var_name right-aligned on the last line of output.
            if let Some(var_name) = opt_var_name {
                use unicode_segmentation::UnicodeSegmentation;

                const VAR_NAME_PREFIX: &str = " # "; // e.g. in " # val1"
                const VAR_NAME_COLUMN_MAX: usize = 32; // Right-align the var_name at this column

                let term_width = match dimensions {
                    Some((width, _)) => width.min(VAR_NAME_COLUMN_MAX),
                    None => VAR_NAME_COLUMN_MAX,
                };

                let expr_with_type = format!("{expr}{EXPR_TYPE_SEPARATOR}{expr_type}");

                // Count graphemes because we care about what's *rendered* in the terminal
                let last_line_len = expr_with_type
                    .split('\n')
                    .last()
                    .unwrap_or_default()
                    .graphemes(true)
                    .count();
                let var_name_len =
                    var_name.graphemes(true).count() + VAR_NAME_PREFIX.graphemes(true).count();
                let spaces_needed = if last_line_len + var_name_len > term_width {
                    buf.push('\n');
                    term_width - var_name_len
                } else {
                    term_width - last_line_len - var_name_len
                };

                for _ in 0..spaces_needed {
                    buf.push(' ');
                }

                buf.push_str(GREEN);
                buf.push_str(VAR_NAME_PREFIX);
                buf.push_str(&var_name);
                buf.push_str(END_COL);
                buf.push('\n');
            }
        }
    }

    buf
}
